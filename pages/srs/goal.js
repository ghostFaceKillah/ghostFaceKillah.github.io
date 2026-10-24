// Daily goal — pure functions, no I/O (see DESIGN.md, "Daily goal").
//
// After a break the due pile can be far bigger than a day's sitting. The goal
// caps the day at a budget of EXPECTED minutes of work and counts reaching it
// as success; everything past the cut simply stays due. Nothing here rewrites
// a due date or an FSRS state: FSRS prices the extra delay from the true
// elapsed time when a waiting card is finally reviewed — exactly how Anki's
// "Maximum reviews/day" and the FSRS simulator's max_cost_perday behave.
//
// Everything is derived from the review log, the card states and the
// settings, so every device holding the synced log computes the same goal:
//   GOAL.prior(entries, t0)   → the day's yardstick: costs, retention, last
//                               review per card (log entries before t0 only)
//   GOAL.planDay({...})       → { planted, goal, more, met, clear, … }
//   GOAL.nextChunk(plan)      → the next budget-sized helping past the goal
//
// Log entries are parsed {ts, grade, ivl, id, ms} (ms = -1 when unknown),
// ivl = the whole-day interval carried into the review, -1 for a first sight.
window.GOAL = (function () {
  "use strict";

  const FSRS = window.FSRS;
  const DAY = FSRS.DAY_MS;

  // Time on a card is capped (Anki's idiom: past this the user wandered off).
  const TIME_CAP_MS = 60000;
  const NOMINAL_MS = 15000; // a day with no measurable pace at all

  // Defaults the measured costs shrink toward (fsrs-rs style: a cost is the
  // mean of n observations pulled toward its default with weight 50/(n+50)).
  // A lapse is the Again plus its relearning pass, ~3× a passing review.
  const DEFAULT_COST = { pass: 8000, lapse: 23000, learn: 18000 };
  const SHRINK_N = 50;
  const WINDOW_DAYS = 30;   // costs come from the 30 days before today…
  const MIN_CARD_DAYS = 50; // …or all history, if that window is too thin

  const clamp = (x, lo, hi) => Math.min(hi, Math.max(lo, x));
  const midnight = ts => { const d = new Date(ts); d.setHours(0, 0, 0, 0); return d.getTime(); };

  // Durations for one day's entries, oldest first. Entries carry an exact
  // show→grade ms; older ones are estimated as the gap since the previous
  // grade in the same sitting (a longer gap = a break, charged as the day's
  // median instead).
  function entryDurations(list) {
    const known = [];
    const durs = list.map((e, i) => {
      if (e.ms >= 0) { const d = Math.min(e.ms, TIME_CAP_MS); known.push(d); return d; }
      const gap = i > 0 ? e.ts - list[i - 1].ts : -1;
      if (gap > 0 && gap <= TIME_CAP_MS) { known.push(gap); return gap; }
      return -1;
    });
    known.sort((x, y) => x - y);
    const median = known.length ? known[known.length >> 1] : NOMINAL_MS;
    return durs.map(d => d >= 0 ? d : median);
  }

  // The day's yardstick, built from log entries BEFORE local midnight t0 only,
  // so nothing graded today can move it. A card-day (one card on one day) is
  // classified by its first grade that day — learn (first sight), pass, or
  // lapse — and costs the sum of all its entries that day, so a lapse carries
  // its same-session relearning pass with it.
  function prior(entries, t0) {
    const lastTs = new Map();
    const byDay = new Map();
    for (const e of entries) {
      if (e.ts >= t0) continue;
      if (!(lastTs.get(e.id) >= e.ts)) lastTs.set(e.id, e.ts);
      const d = midnight(e.ts);
      if (!byDay.has(d)) byDay.set(d, []);
      byDay.get(d).push(e);
    }
    const cardDays = [];
    for (const [day, list] of byDay) {
      list.sort((x, y) => x.ts - y.ts);
      const durs = entryDurations(list);
      const perCard = new Map();
      list.forEach((e, i) => {
        let g = perCard.get(e.id);
        if (!g) {
          g = { day, kind: e.ivl < 0 ? "learn" : e.grade > 1 ? "pass" : "lapse", passed: e.grade > 1, ms: 0 };
          perCard.set(e.id, g);
        }
        g.ms += durs[i];
      });
      for (const g of perCard.values()) cardDays.push(g);
    }
    const from = t0 - WINDOW_DAYS * DAY;
    let use = cardDays.filter(g => g.day >= from);
    if (use.length < MIN_CARD_DAYS) use = cardDays; // e.g. back from a long break
    const sum = { pass: 0, lapse: 0, learn: 0 };
    const n = { pass: 0, lapse: 0, learn: 0 };
    let newPassed = 0;
    for (const g of use) {
      sum[g.kind] += g.ms;
      n[g.kind]++;
      if (g.kind === "learn" && g.passed) newPassed++;
    }
    const shrink = (total, count, dflt) => (total + SHRINK_N * dflt) / (count + SHRINK_N);
    const cost = {
      pass: shrink(sum.pass, n.pass, DEFAULT_COST.pass),
      lapse: shrink(sum.lapse, n.lapse, DEFAULT_COST.lapse),
      learn: shrink(sum.learn, n.learn, DEFAULT_COST.learn),
    };
    cost.lapse = Math.max(cost.lapse, cost.pass);
    return {
      t0,
      lastTs,
      cost,
      n,
      // true retention (first review of a learned card per day), shrunk toward the target
      retention: shrink(n.pass, n.pass + n.lapse, FSRS.REQUEST_RETENTION),
      newPass: shrink(newPassed, n.learn, FSRS.REQUEST_RETENTION),
    };
  }

  // Chance a review at model recall r passes, calibrated to the measured
  // retention (the model aims reviews at the target, so measured/target is
  // how far reality sits from it).
  function passProb(r, p) {
    return clamp(r * p.retention / FSRS.REQUEST_RETENTION, 0.05, 0.98);
  }
  // Expected time of a review at recall r: lapses cost more, so an overdue
  // card (low r) costs more than one reviewed on time.
  function expectedMs(r, p) {
    const q = passProb(r, p);
    return q * p.cost.pass + (1 - q) * p.cost.lapse;
  }

  // A card's cost for today, fixed for the whole day: read from the card as
  // it stood before today's first grade — its first log entry today if it
  // has one, else its untouched state (grade() logs exactly
  // round((due − lastReview)/day) as ivl, so both read the same number) —
  // with recall taken at midnight. At the 0.9 target FSRS's interval is
  // round(S), so S ≈ ivl. No log history before today → recall at target.
  function cardCost(id, s, first, p) {
    if (first ? first.ivl < 0 : !s) return p.cost.learn;
    const ivl = first ? first.ivl : Math.round((s.due - s.lastReview) / DAY);
    const last = p.lastTs.get(id);
    const r = last === undefined ? FSRS.REQUEST_RETENTION
      : FSRS.retrievability(Math.max(0, (p.t0 - last) / DAY), Math.max(1, ivl));
    return expectedMs(r, p);
  }

  // Longest-needed prefix of `order` whose cost covers `needMs` — the card
  // that crosses the line is admitted (fsrs-rs's max_cost_perday check), so
  // a budget smaller than one card still gets one card. While any budget is
  // left, the first `forced` cards are always in; once it's spent, nothing
  // is (so an Again in a helping past a met goal can't un-bloom the day).
  function cut(order, costOf, needMs, forced) {
    if (!(needMs > 0)) return 0;
    if (needMs === Infinity) return order.length;
    let k = 0, acc = 0;
    while (k < order.length && (k < forced || acc < needMs)) acc += costOf(order[k++].id);
    return k;
  }

  // Today's plan. o = {
  //   cards     enabled cards, curriculum order
  //   states    cardId → FSRS state
  //   today     parsed log entries since local midnight, log order
  //   quotaLeft new cards still allowed today (newPerDay − introduced)
  //   prior     GOAL.prior(log before today, midnight)
  //   cutoff    end of today (ms) — due at or before it = due today
  //   budgetMs  the day's goal in expected ms; Infinity = no goal
  // }
  // planted = enabled cards graded today and not due again before tomorrow
  //           (today's plot's "put to bed"); they've spent their cost.
  // Today's work, in order: relearning steps (graded today, still due —
  // the 10-minute step is part of the same review), then due cards oldest
  // due first, then today's new-card slice in curriculum order. The goal is
  // the prefix of that order covering what's left of the budget; `more` is
  // the rest, waiting with its state untouched.
  // Why this is stable: every card's cost and every sort key is fixed for
  // the day, so the plan only changes when something is graded — and
  // grading a goal card can only shrink the goal (a pass moves its cost
  // from `goal` to `spent`; an Again moves it to the forced relearning
  // block). So working through the goal always ends in `met`.
  function planDay(o) {
    const { cards, states, today, quotaLeft, prior: p, cutoff, budgetMs } = o;
    const byId = new Map(cards.map(c => [c.id, c]));
    const first = new Map();
    for (const e of today) if (!first.has(e.id)) first.set(e.id, e);
    const costs = new Map();
    const costOf = id => {
      let c = costs.get(id);
      if (c === undefined) costs.set(id, c = cardCost(id, states[id], first.get(id), p));
      return c;
    };

    const planted = [];
    let spentMs = 0;
    for (const [id, e] of first) {
      const c = byId.get(id), s = states[id];
      if (c && s && s.due > cutoff) {
        planted.push({ card: c, fresh: e.ivl < 0 });
        spentMs += costOf(id);
      }
    }
    const relearn = [], due = [], fresh = [];
    for (const c of cards) {
      const s = states[c.id];
      if (!s) { if (fresh.length < quotaLeft) fresh.push(c); }
      else if (s.due <= cutoff) (first.has(c.id) ? relearn : due).push(c);
    }
    // a relearning step left over from an earlier day (Again, then the
    // session ended) is the tail of that day's review — it goes first
    const stepLeft = s => s.due - s.lastReview < DAY / 2;
    due.sort((a, b) => {
      const sa = states[a.id], sb = states[b.id];
      return stepLeft(sb) - stepLeft(sa) || sa.due - sb.due || (a.id < b.id ? -1 : a.id > b.id ? 1 : 0);
    });
    const order = relearn.concat(due, fresh);
    const k = cut(order, costOf, budgetMs - spentMs, relearn.length);
    const goal = order.slice(0, k);
    let goalMs = 0;
    for (const c of goal) goalMs += costOf(c.id);
    const met = planted.length > 0 && goal.length === 0;
    return {
      planted, goal, more: order.slice(k),
      spentMs, budgetMs, goalMs, costOf,
      relearnN: relearn.length,
      met,                                   // the goal is in bloom
      clear: met && k === order.length,      // …and nothing is left at all
    };
  }

  // The next helping past a met goal ("keep going"): leftover relearning
  // steps first, then the same order, one budget's worth at a time.
  function nextChunk(plan) {
    const forced = Math.max(0, plan.relearnN - plan.goal.length);
    return plan.more.slice(0, cut(plan.more, plan.costOf, plan.budgetMs, forced));
  }

  return {
    TIME_CAP_MS,
    entryDurations,
    prior,
    passProb,
    expectedMs,
    cardCost,
    cut,
    planDay,
    nextChunk,
    DEFAULT_COST,
  };
})();
