// Tests for the daily goal. Run with:  node pages/srs/goal.test.js
//
// Property tests over simulated decks: a seeded history graded through the
// real FSRS.review and logged exactly like the app's grade(), then a break
// with nothing graded, then "today" — whole daily sessions replayed against
// GOAL.planDay the way the app drives it.
"use strict";
const fs = require("fs");
const path = require("path");

global.window = {};
new Function("window", fs.readFileSync(path.join(__dirname, "fsrs.js"), "utf8"))(global.window);
new Function("window", fs.readFileSync(path.join(__dirname, "goal.js"), "utf8"))(global.window);
const FSRS = global.window.FSRS;
const GOAL = global.window.GOAL;
const { AGAIN, HARD, GOOD, EASY, DAY_MS } = FSRS;

let failures = 0;
function check(name, cond, detail) {
  if (cond) console.log("  ok  " + name);
  else { failures++; console.log("FAIL  " + name + (detail ? " — " + detail : "")); }
}

// A property is asserted over many generated cases but reports as one check:
// on failure, how many cases failed and the first counterexample (its seed,
// break and budget are in ctx).
function property(name) {
  let cases = 0, bad = 0, first = "";
  return {
    expect(cond, ctx, detail) {
      cases++;
      if (!cond) { if (!bad) first = ctx + (detail ? ": " + detail : ""); bad++; }
      return cond;
    },
    report(note) {
      check(`${name} (${cases} cases${note ? ", " + note : ""})`, cases > 0 && bad === 0,
        cases ? `${bad} failing; first: ${first}` : "no applicable cases were generated");
    },
  };
}

// ----- seeded randomness (never Math.random) ----------------------------------
function mulberry32(a) {
  return function () {
    a = (a + 0x6D2B79F5) | 0;
    let t = Math.imul(a ^ (a >>> 15), a | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
function shuffle(xs, rnd) {
  const a = xs.slice();
  for (let i = a.length - 1; i > 0; i--) {
    const j = Math.floor(rnd() * (i + 1));
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}
// Time on a card by grade: a miss takes longer than a clean pass.
function msFor(g, rnd) {
  const [lo, hi] = g === AGAIN ? [12000, 40000] : g === HARD ? [8000, 25000] : g === GOOD ? [4000, 14000] : [3000, 8000];
  return lo + Math.floor(rnd() * (hi - lo));
}
function randomGrade(rnd, pAgain) {
  const r = rnd();
  if (r < pAgain) return AGAIN;
  const q = (r - pAgain) / (1 - pAgain);
  return q < 0.12 ? HARD : q < 0.85 ? GOOD : EASY;
}
const ids = list => list.map(c => c.id);

// ----- time --------------------------------------------------------------------
// "Today" is Tue 1 Sep 2026, local time. History and breaks fall in
// June–August (90 days back at most), clear of the usual DST changes in either hemisphere.
const day = (offset, h = 0, m = 0) => new Date(2026, 8, 1 + offset, h, m).getTime();
const midnightOf = offset => new Date(2026, 8, 1 + offset).setHours(0, 0, 0, 0);
const endOfDay = offset => new Date(2026, 8, 1 + offset).setHours(23, 59, 59, 999);

// Runs fn with the clock pinned to ts (Date.now() and `new Date()`), so a
// hidden dependence on the wall clock would show up as a changed plan.
function withClock(ts, fn) {
  const RealDate = Date;
  class FakeDate extends RealDate {
    constructor(...a) { if (a.length) super(...a); else super(ts); }
    static now() { return ts; }
  }
  global.Date = FakeDate;
  try { return fn(); } finally { global.Date = RealDate; }
}

// ----- the app's grading, and the plan inputs it derives ------------------------
// Exactly the app's grade(): review, then log the whole-day interval the card
// carried INTO the review (−1 for a first sight).
function review(states, id, g, now, ms) {
  const prev = states[id];
  const next = FSRS.review(prev || null, g, now);
  const entry = { ts: now, grade: g, ivl: prev ? Math.round((prev.due - prev.lastReview) / DAY_MS) : -1, id, ms };
  return { next, entry };
}

// A world is one day's worth of what the caller derives planDay's inputs
// from. Grading returns a new world, so one starting point can be replayed
// along different paths. `pre` = log before midnight, `today` = since.
function grade(w, id, g, now, ms) {
  const { next, entry } = review(w.states, id, g, now, ms);
  return { ...w, states: { ...w.states, [id]: next }, today: w.today.concat([entry]) };
}
const quotaLeftOf = w => Math.max(0, w.newPerDay - w.today.filter(e => e.ivl < 0).length);
const inputs = (w, budgetMs) => ({
  cards: w.cards, states: w.states, today: w.today, quotaLeft: quotaLeftOf(w),
  prior: w.prior, cutoff: w.cutoff, budgetMs,
});
const planOf = (w, budgetMs) => GOAL.planDay(inputs(w, budgetMs));

const N_CARDS = 360;
const HISTORY_DAYS = 60;
const HISTORY_NEW_PER_DAY = 6; // leaves ~60 cards never seen
const NEW_PER_DAY = 10;

// A deck with a realistic past: daily sessions (some days skipped, some
// sessions abandoned midway — leaving overdue cards and stray relearning
// steps), random grades, Again re-queued a few cards later as the app does.
// The oldest entries predate the ms field (ms = −1). Then `breakDays` with
// nothing graded, then today. The history depends on the seed only.
function buildWorld(seed, breakDays) {
  const rnd = mulberry32(seed);
  const cards = Array.from({ length: N_CARDS }, (_, i) => ({ id: "c" + String(i + 1).padStart(3, "0") }));
  const states = {};
  const log = [];
  const firstDay = -(HISTORY_DAYS + breakDays);
  for (let d = firstDay; d < -breakDays; d++) {
    if (rnd() < 0.15) continue; // a skipped day
    const end = endOfDay(d);
    const oldFormat = d < firstDay + 12;
    let now = day(d, 7 + Math.floor(rnd() * 12), Math.floor(rnd() * 60));
    const due = shuffle(cards.filter(c => states[c.id] && states[c.id].due <= end).map(c => c.id), rnd);
    const fresh = cards.filter(c => !states[c.id]).slice(0, HISTORY_NEW_PER_DAY).map(c => c.id);
    const queue = due.concat(fresh);
    const stopAfter = rnd() < 0.25 ? Math.floor(rnd() * queue.length) : Infinity; // walked away
    for (let n = 0; queue.length && n < stopAfter; n++) {
      const id = queue.shift();
      const g = randomGrade(rnd, 0.15);
      const ms = msFor(g, rnd);
      now += 500 + Math.floor(rnd() * 2000) + ms;
      const { next, entry } = review(states, id, g, now, oldFormat ? -1 : ms);
      states[id] = next;
      log.push(entry);
      if (g === AGAIN) queue.splice(Math.min(3 + Math.floor(rnd() * 5), queue.length), 0, id);
    }
  }
  const t0 = midnightOf(0);
  return {
    cards, states, pre: log, today: [], t0, cutoff: endOfDay(0), newPerDay: NEW_PER_DAY,
    prior: GOAL.prior(log, t0),
  };
}

// ----- properties ------------------------------------------------------------------
const P1 = property("1. cost stability: costOf(id) of every card unchanged by any grade earlier today");
const P2 = property("2. shrink-only (strict): one grade of goal card X from a fresh plan → new goal ⊆ (goal − X) ∪ {X if Again}");
const P2s = property("2. shrink-only through whole sessions (re-entry only for a dropped card failed into the forced relearning block)");
const P3 = property("3. session ⇒ met: working through the starting goal ends met with an empty goal");
const P3c = property("3. a goal cut short of the whole order covers the starting need: Σ costOf(goal) ≥ budget − spent");
const P4 = property("4. time invariance: the plan at 00:05 equals the plan at 23:30 (clock stubbed)");
const P5 = property("5. off (Infinity) = old behavior: goal = due ∪ fresh slice, more empty, met ⇔ planted ∧ nothing left");
const P6 = property("6. tight cut: Σ costOf(goal) ≥ need > Σ costOf(goal − last)");
const P7 = property("7. reviews before new: no new card in the goal while a due card waits in more");
const P8 = property("8. bonus never un-blooms: grading nextChunk (with Agains) keeps the plan met");
const P9 = property("9. expectedMs(r, prior) is non-increasing in r on [0, 1]");
const P10 = property("10. device B: JSON-copied states and log + freshly built prior → same plan and costs");
const P11 = property("11. introducing the goal's new cards in two different orders leaves the same remaining goal");
const P12 = property("12. budget of 1 ms with due cards, nothing graded yet → goal is exactly one review");
const P12r = property("12. budget of 1 ms, two due cards failed, nothing planted → goal is exactly those two relearning steps");
const P13 = property("13. prior: cost.lapse ≥ cost.pass for every prior built");
const P13t = property("13. prior ignores entries at or after t0");
const PC = property("costs are finite and positive");
const PF = property("forced relearning: while budget is left, every pending relearning step is in the goal, first");
const PO = property("order: today's relearning steps, then leftover steps, then due oldest first, then the new-card slice");
const PN = property("8. nextChunk: a prefix of more — pending relearning steps, then one budget's worth");
let leftoverPlans = 0;

// Checks that hold for every plan, whatever happened earlier in the day.
function planInvariants(plan, w, ctx) {
  const st = w.states;
  const dueNow = id => !!st[id] && st[id].due <= w.cutoff;

  const need = plan.budgetMs - plan.spentMs;
  if (plan.more.length && plan.goal.length > plan.relearnN) {
    let all = 0, butLast = 0;
    plan.goal.forEach((c, i) => {
      const k = plan.costOf(c.id);
      all += k;
      if (i < plan.goal.length - 1) butLast += k;
    });
    P6.expect(all >= need && need > butLast, ctx,
      `need ${need} ms, Σgoal ${all}, Σ(goal − last) ${butLast}, goal ${plan.goal.length}, relearnN ${plan.relearnN}`);
  }

  // the documented order, rebuilt independently
  const seenToday = new Set(w.today.map(e => e.id));
  const relearn = w.cards.filter(c => seenToday.has(c.id) && dueNow(c.id)).map(c => c.id);
  const leftover = id => st[id].due - st[id].lastReview < DAY_MS / 2;
  const dueOld = w.cards.filter(c => !seenToday.has(c.id) && dueNow(c.id)).map(c => c.id)
    .sort((a, b) => leftover(b) - leftover(a) || st[a].due - st[b].due || (a < b ? -1 : 1));
  if (dueOld.some(leftover)) leftoverPlans++;
  const freshSlice = w.cards.filter(c => !st[c.id]).slice(0, quotaLeftOf(w)).map(c => c.id);
  const order = ids(plan.goal.concat(plan.more));
  PO.expect(order.join() === relearn.concat(dueOld, freshSlice).join() && plan.relearnN === relearn.length, ctx,
    `got [${order.slice(0, 12)}…], want [${relearn.concat(dueOld, freshSlice).slice(0, 12)}…]`);
  if (need > 0) {
    PF.expect(ids(plan.goal.slice(0, relearn.length)).join() === relearn.join(), ctx,
      `need ${need} ms, relearning [${relearn}], goal [${ids(plan.goal).slice(0, relearn.length + 3)}…]`);
  }

  const newInGoal = plan.goal.filter(c => !st[c.id]).length;
  const dueWaiting = plan.more.filter(c => dueNow(c.id)).length;
  P7.expect(newInGoal === 0 || dueWaiting === 0, ctx,
    `${newInGoal} new card(s) in the goal while ${dueWaiting} due card(s) wait in more`);

  if (plan.budgetMs === Infinity) {
    const due = w.cards.filter(c => dueNow(c.id)).map(c => c.id);
    const fresh = w.cards.filter(c => !st[c.id]).slice(0, quotaLeftOf(w)).map(c => c.id);
    const want = new Set(due.concat(fresh));
    const goal = ids(plan.goal);
    const planted = w.cards.filter(c => seenToday.has(c.id) && st[c.id] && st[c.id].due > w.cutoff).map(c => c.id);
    const errs = [];
    if (goal.length !== want.size || !goal.every(id => want.has(id))) errs.push(`goal has ${goal.length} cards, due ∪ fresh has ${want.size}`);
    if (goal.filter(id => !st[id]).join() !== fresh.join()) errs.push("new cards are not the curriculum-order slice");
    if (plan.more.length) errs.push(`more has ${plan.more.length} cards`);
    if (ids(plan.planted.map(x => x.card)).sort().join() !== planted.sort().join()) errs.push("planted differs from the old plot's");
    const oldMet = planted.length > 0 && due.length === 0 && fresh.length === 0;
    if (plan.met !== oldMet) errs.push(`met ${plan.met}, old bloom ${oldMet}`);
    P5.expect(errs.length === 0, ctx, errs.join("; "));
  }
}

function planDiff(a, b, cards) {
  const d = [];
  if (ids(a.goal).join() !== ids(b.goal).join()) d.push(`goal [${ids(a.goal)}] vs [${ids(b.goal)}]`);
  if (ids(a.more).join() !== ids(b.more).join()) d.push("more differs");
  if (ids(a.planted.map(x => x.card)).sort().join() !== ids(b.planted.map(x => x.card)).sort().join()) d.push("planted differs");
  if (a.spentMs !== b.spentMs) d.push(`spentMs ${a.spentMs} vs ${b.spentMs}`);
  if (a.met !== b.met) d.push(`met ${a.met} vs ${b.met}`);
  for (const c of cards) {
    if (a.costOf(c.id) !== b.costOf(c.id)) { d.push(`costOf(${c.id}) ${a.costOf(c.id)} vs ${b.costOf(c.id)}`); break; }
  }
  return d.join("; ");
}

const copy = x => JSON.parse(JSON.stringify(x));
function priorKey(p) {
  return JSON.stringify([p.t0, p.cost, p.n, p.retention, p.newPass, [...p.lastTs].sort()]);
}

// A session in progress: the world, its current plan, and the checks every
// grade must pass — each card's cost as it stood at the start of the day (1),
// and a goal that only shrinks (2): a passed card leaves it, a failed one
// stays, and nothing else enters — except a card that had been in today's
// goal, was dropped, and is now failed: it comes back as a forced
// relearning step (the known allowance, counted in s.reentries).
function startSession(w, budgetMs, ctx) {
  const plan = planOf(w, budgetMs);
  return {
    w, plan, budgetMs, ctx, n: 0, reentries: 0,
    costs0: new Map(w.cards.map(c => [c.id, plan.costOf(c.id)])),
    everInGoal: new Set(ids(plan.goal)),
  };
}
function sameCosts(s, plan, why) {
  for (const c of s.w.cards) {
    const a = s.costs0.get(c.id), b = plan.costOf(c.id);
    if (a !== b) return P1.expect(false, s.ctx, `${why}: costOf(${c.id}) ${a} → ${b}`);
  }
  return P1.expect(true, s.ctx);
}
function sessionGrade(s, x, g, now, ms) {
  const old = new Set(ids(s.plan.goal));
  s.w = grade(s.w, x, g, now, ms);
  const next = planOf(s.w, s.budgetMs);
  s.n++;
  sameCosts(s, next, `grade #${s.n} (${x} → ${g})`);
  const nextIds = ids(next.goal);
  const gained = nextIds.filter(id => !old.has(id));
  const reentry = id => id === x && g === AGAIN && s.everInGoal.has(x) && nextIds.indexOf(x) < next.relearnN;
  const bad = gained.filter(id => !reentry(id));
  const passStays = g !== AGAIN && nextIds.includes(x);
  P2s.expect(bad.length === 0 && !passStays, s.ctx,
    `grade #${s.n}: ${x} (${old.has(x) ? "in" : "not in"} goal) → ${g}: ` +
    (passStays ? "passed card stayed in the goal" : `goal gained [${bad}]`));
  if (gained.length && !bad.length) s.reentries++;
  planInvariants(next, s.w, s.ctx);
  for (const id of nextIds) s.everInGoal.add(id);
  s.plan = next;
}

// One day: single grades from the fresh plan, then a whole session through
// the goal, then bonus chunks past it. Returns worlds for the cross-checks.
function runDay(w0, budgetMs, seed, ctx) {
  const rnd = mulberry32(seed);
  let now = day(0, 9, Math.floor(rnd() * 60));
  const s = startSession(w0, budgetMs, ctx);
  const plan0 = s.plan;
  planInvariants(plan0, w0, ctx);
  PC.expect([...s.costs0.values()].every(c => Number.isFinite(c) && c > 0), ctx);

  // 1 + 2 (strict): each goal card graded alone from the fresh plan
  const goal0 = ids(plan0.goal);
  const step = Math.max(1, Math.ceil(goal0.length / 30));
  for (let i = 0; i < goal0.length; i += step) {
    const x = goal0[i];
    for (const g of [AGAIN, GOOD]) {
      const w1 = grade(w0, x, g, now, 9000);
      const p1 = planOf(w1, budgetMs);
      sameCosts(s, p1, `after ${x} graded ${g}`);
      const allowed = new Set(goal0.filter(id => id !== x));
      if (g === AGAIN) allowed.add(x);
      const gained = ids(p1.goal).filter(id => !allowed.has(id));
      P2.expect(gained.length === 0, ctx, `${x} graded ${g}: goal gained [${gained}]`);
      planInvariants(p1, w1, ctx);
    }
  }

  // 3: the whole session — reviews first, then new, each block shuffled;
  // Again puts the card back 3–7 places later, anything else retires it
  if (plan0.more.length) {
    let sum = 0;
    for (const id of goal0) sum += plan0.costOf(id);
    P3c.expect(sum >= budgetMs - plan0.spentMs, ctx, `Σ ${sum} < need ${budgetMs - plan0.spentMs}`);
  }
  const queue = shuffle(goal0.filter(id => w0.states[id]), rnd)
    .concat(shuffle(goal0.filter(id => !w0.states[id]), rnd));
  const half = Math.floor(queue.length / 2);
  let mid = w0;
  while (queue.length) {
    const x = queue.shift();
    const g = rnd() < 0.2 ? AGAIN : randomGrade(rnd, 0);
    const ms = 4000 + Math.floor(rnd() * 12000);
    now += ms + Math.floor(rnd() * 1000);
    sessionGrade(s, x, g, now, ms);
    if (g === AGAIN) queue.splice(Math.min(3 + Math.floor(rnd() * 5), queue.length), 0, x);
    if (s.n === half) mid = s.w;
  }
  if (goal0.length) {
    P3.expect(s.plan.met === true && s.plan.goal.length === 0, ctx,
      `after ${s.n} grades: met ${s.plan.met}, goal [${ids(s.plan.goal)}], need ${s.plan.budgetMs - s.plan.spentMs}`);
  }

  // 8: keep going past the goal, a budget's worth at a time
  for (let round = 0; round < 2 && s.plan.met; round++) {
    const chunk = GOAL.nextChunk(s.plan), more = s.plan.more;
    let sum = 0;
    for (const c of chunk) sum += s.plan.costOf(c.id);
    PN.expect(chunk.every((c, i) => c === more[i]) && chunk.length >= s.plan.relearnN &&
      (chunk.length === more.length || (sum >= budgetMs && sum - s.plan.costOf(chunk[chunk.length - 1].id) < budgetMs) ||
        chunk.length === s.plan.relearnN), ctx,
      `chunk ${chunk.length} of ${more.length}, relearnN ${s.plan.relearnN}, Σ ${sum} vs budget ${budgetMs}`);
    const q = ids(chunk);
    while (q.length) {
      const x = q.shift();
      const g = rnd() < 0.25 ? AGAIN : randomGrade(rnd, 0);
      const ms = 4000 + Math.floor(rnd() * 12000);
      now += ms;
      sessionGrade(s, x, g, now, ms);
      P8.expect(s.plan.met && s.plan.goal.length === 0, ctx,
        `bonus round ${round + 1}: ${x} → ${g}: met ${s.plan.met}, goal [${ids(s.plan.goal)}]`);
      if (g === AGAIN) q.splice(Math.min(3 + Math.floor(rnd() * 5), q.length), 0, x);
    }
  }
  return { mid, end: s.w, reentries: s.reentries };
}

// Device B: the synced copies of the states and the log, prior rebuilt there.
function onDeviceB(w) {
  const entries = copy(w.pre.concat(w.today));
  return {
    ...w,
    cards: copy(w.cards),
    states: copy(w.states),
    pre: entries.filter(e => e.ts < w.t0),
    today: entries.filter(e => e.ts >= w.t0),
    prior: GOAL.prior(entries, w.t0),
  };
}

const SEEDS = [11, 23, 37, 41, 59];
const BREAKS = [0, 3, 14, 30];
const BUDGETS = [10, 15, 30, Infinity];
const label = b => (b === Infinity ? "off" : b + " min");
const priors = [];
let reentries = 0, cutDays = 0;

for (const seed of SEEDS) {
  for (const breakDays of BREAKS) {
    const w0 = buildWorld(seed, breakDays);
    const wctx = `seed ${seed}, break ${breakDays}d`;
    priors.push([wctx, w0.prior]);

    // 12: a budget smaller than one card
    const dueIds = w0.cards.filter(c => w0.states[c.id] && w0.states[c.id].due <= w0.cutoff).map(c => c.id);
    if (dueIds.length >= 2) {
      const p = planOf(w0, 1);
      P12.expect(p.goal.length === 1 && p.relearnN === 0 && dueIds.includes(p.goal[0].id), wctx,
        `goal [${ids(p.goal)}], relearnN ${p.relearnN}`);
      // fail two due cards (nothing planted): both relearning steps, nothing else
      const [x, y] = ids(p.goal.concat(p.more)).slice(0, 2);
      const w2 = grade(grade(w0, x, AGAIN, day(0, 9, 0), 9000), y, AGAIN, day(0, 9, 1), 9000);
      const p2 = planOf(w2, 1);
      const want = w0.cards.map(c => c.id).filter(id => id === x || id === y).join();
      P12r.expect(p2.spentMs === 0 && p2.relearnN === 2 && ids(p2.goal).join() === want, wctx,
        `after failing ${x}, ${y}: goal [${ids(p2.goal)}], relearnN ${p2.relearnN}, spent ${p2.spentMs}`);
    }

    for (const minutes of BUDGETS) {
      const budgetMs = minutes * 60000;
      const ctx = `${wctx}, budget ${label(minutes)}`;
      const plan0 = planOf(w0, budgetMs);
      if (plan0.more.length) cutDays++;

      // 4: no grading, two times of day, clock stubbed for both
      const at = ts => withClock(ts, () =>
        GOAL.planDay({ ...inputs(w0, budgetMs), prior: GOAL.prior(w0.pre.concat(w0.today), w0.t0) }));
      const diff4 = planDiff(at(day(0, 0, 5)), at(day(0, 23, 30)), w0.cards);
      P4.expect(!diff4, ctx, diff4);

      // 11: the goal's new cards introduced in two orders
      const newIds = ids(plan0.goal).filter(id => !w0.states[id]);
      if (newIds.length >= 2) {
        const introduce = order => order.reduce((w, id, i) => grade(w, id, GOOD, day(0, 9, 0) + i * 10000, 10000), w0);
        const rnd11 = mulberry32(seed * 1000 + breakDays);
        let other = shuffle(newIds, rnd11);
        if (other.join() === newIds.join()) other = newIds.slice().reverse();
        const diff11 = planDiff(planOf(introduce(newIds), budgetMs), planOf(introduce(other), budgetMs), w0.cards);
        P11.expect(!diff11, ctx, `[${newIds}] vs [${other}]: ${diff11}`);
      }

      const sessionSeed = seed * 10007 + breakDays * 101 + (minutes === Infinity ? 99 : minutes);
      const day1 = runDay(w0, budgetMs, sessionSeed, ctx);
      reentries += day1.reentries;

      // 10: device B, mid-session and at the end of the day
      for (const [when, w] of [["mid-session", day1.mid], ["day's end", day1.end]]) {
        const diff10 = planDiff(planOf(w, budgetMs), planOf(onDeviceB(w), budgetMs), w.cards);
        P10.expect(!diff10, `${ctx}, ${when}`, diff10);
      }

      // 13: today's entries (and one exactly at t0) never reach the prior
      const atT0 = { ts: w0.t0, grade: AGAIN, ivl: 3, id: "c001", ms: 59000 };
      const withToday = GOAL.prior(day1.end.pre.concat(day1.end.today, [atT0]), w0.t0);
      P13t.expect(priorKey(withToday) === priorKey(w0.prior), ctx);
    }
  }
}

// ----- 2. the re-entry allowance, staged ---------------------------------------------
// Random sessions almost never drop a card: the cut's last card is the least
// overdue, so usually the cheapest. Staged: four long-interval cards due
// first, then L — a 1-day card unreviewed for 5½ days, due last but priced
// higher. With the slack under cost(L) − cost(d4), passing L first drops
// d4, which is still in the session's queue; failing d4 brings it back as a
// forced relearning step. Once the budget is spent the goal is met even with
// d4's step pending — the step leads the next chunk instead (what 8 needs).
{
  const states = {}, pre = [];
  const put = (id, lastReview, ivlDays) => {
    states[id] = { stability: ivlDays, difficulty: 5, due: lastReview + ivlDays * DAY_MS, lastReview, reps: 4, lapses: 0 };
    pre.push({ ts: lastReview, grade: GOOD, ivl: Math.max(1, Math.round(ivlDays / 3)), id, ms: 9000 });
  };
  ["d1", "d2", "d3", "d4"].forEach((id, i) => put(id, day(-70 + i, 10), 60)); // due 10…7 days ago
  put("l", day(-6, 11), 1);                                                    // due 5 days ago
  pre.sort((a, b) => a.ts - b.ts);
  const t0 = midnightOf(0);
  const base = {
    cards: ["d1", "d2", "d3", "d4", "l"].map(id => ({ id })), states, pre, today: [], t0,
    cutoff: endOfDay(0), newPerDay: 0, prior: GOAL.prior(pre, t0),
  };
  const probe = planOf(base, Infinity);
  const cD = ["d1", "d2", "d3", "d4"].map(id => probe.costOf(id)), cL = probe.costOf("l");
  const budgetMs = cD.reduce((a, b) => a + b) + (cL - cD[3]) / 2;
  const s = startSession(base, budgetMs, "staged re-entry");
  check("2. staged: L is due last yet costs more than d4; the goal is all five, L last",
    cL > cD[3] && ids(s.plan.goal).join() === "d1,d2,d3,d4,l",
    `cost(L) ${cL}, cost(d4) ${cD[3]}, goal [${ids(s.plan.goal)}]`);
  let now = day(0, 9, 0);
  sessionGrade(s, "l", GOOD, now += 10000, 10000);
  check("2. staged: passing L first drops d4 from the goal", !ids(s.plan.goal).includes("d4"),
    `goal [${ids(s.plan.goal)}]`);
  sessionGrade(s, "d4", AGAIN, now += 10000, 10000);
  check("2. staged: failing the dropped d4 brings it back as a forced relearning step (the allowance)",
    s.reentries === 1 && ids(s.plan.goal)[0] === "d4" && s.plan.relearnN === 1,
    `goal [${ids(s.plan.goal)}], relearnN ${s.plan.relearnN}, re-entries ${s.reentries}`);
  for (const id of ["d1", "d2", "d3"]) sessionGrade(s, id, GOOD, now += 10000, 10000);
  check("2. staged: budget spent with d4's step pending → met, and d4 leads the next chunk",
    s.plan.met && s.plan.goal.length === 0 && ids(GOAL.nextChunk(s.plan))[0] === "d4",
    `met ${s.plan.met}, goal [${ids(s.plan.goal)}], next chunk [${ids(GOAL.nextChunk(s.plan))}]`);
  sessionGrade(s, "d4", GOOD, now += 10000, 10000);
  check("2. staged: the session ends met with nothing left", s.plan.met && s.plan.clear,
    `met ${s.plan.met}, clear ${s.plan.clear}`);
}

// ----- prior sanity (synthetic logs) ------------------------------------------------
const T0 = midnightOf(0);
// n card-days, one per card, spread over the 28 days before T0
function synthLog(n, { grade: g = GOOD, ivl = 5, ms = 40000, daysBack = 1 } = {}) {
  return Array.from({ length: n }, (_, i) => ({
    ts: day(-(daysBack + (i % 28)), 10, 0) + i * 61000,
    grade: g, ivl, id: "s" + i, ms,
  }));
}
{
  const D = GOAL.DEFAULT_COST;
  const p = GOAL.prior([], T0);
  priors.push(["empty log", p]);
  check("13. prior, empty log: costs are DEFAULT_COST",
    p.cost.pass === D.pass && p.cost.lapse === D.lapse && p.cost.learn === D.learn, JSON.stringify(p.cost));
  check("13. prior, empty log: retention = 0.9", Math.abs(p.retention - 0.9) < 1e-12, `retention ${p.retention}`);

  const passCost = n => { const q = GOAL.prior(synthLog(n), T0); priors.push([`${n}× 40 s passes`, q]); return q.cost.pass; };
  const c50 = passCost(50), c200 = passCost(200), c1000 = passCost(1000);
  check("13. prior: 40 s passes pull the pass cost from 8 s toward 40 s, more so with more evidence",
    D.pass < c50 && c50 < c200 && c200 < c1000 && c1000 < 40000 && c1000 > 38000,
    `${c50.toFixed(0)}, ${c200.toFixed(0)}, ${c1000.toFixed(0)} ms`);

  const capped = GOAL.prior(synthLog(200, { ms: 300000 }), T0).cost.pass;
  const atCap = GOAL.prior(synthLog(200, { ms: GOAL.TIME_CAP_MS }), T0).cost.pass;
  check("13. prior: time on a card is capped at TIME_CAP_MS", capped === atCap, `${capped} vs ${atCap}`);

  // back from a long break: nothing in the 30-day window, costs come from all history
  const old = GOAL.prior(synthLog(200, { daysBack: 40 }), T0).cost.pass;
  check("13. prior: with an empty 30-day window, costs come from older history", old > 30000, `${old.toFixed(0)} ms`);

  const lapses = GOAL.prior(synthLog(300, { grade: AGAIN, ms: 25000 }), T0);
  priors.push(["300 lapses", lapses]);
  check("13. prior: a log of lapses drives retention well below the target", lapses.retention < 0.3,
    `retention ${lapses.retention}`);
}

for (const [ctx, p] of priors) {
  P13.expect(p.cost.lapse >= p.cost.pass, ctx, `lapse ${p.cost.lapse} < pass ${p.cost.pass}`);
  // 9: tolerance of 1e-9 ms only absorbs float rounding when lapse === pass
  let prev = Infinity, worst = "";
  for (let i = 0; i <= 1000; i++) {
    const r = i / 1000, e = GOAL.expectedMs(r, p);
    if (e > prev + 1e-9 && !worst) worst = `expectedMs(${r}) = ${e} > expectedMs(${(i - 1) / 1000}) = ${prev}`;
    prev = e;
  }
  P9.expect(!worst, ctx, worst);
}

for (const P of [P1, P2, P2s, P3, P3c, P4, P5, P6, P7, P8, PN, P9, P10, P11, P12, P12r, P13, P13t, PC, PF, PO]) {
  P.report(P === P2s ? `${reentries} allowed re-entries in the random sessions` : P === P3c ? `${cutDays} days cut short`
    : P === PO ? `${leftoverPlans} with leftover steps` : "");
}

console.log(failures === 0 ? "\nAll goal tests passed." : `\n${failures} FAILURE(S)`);
process.exit(failures ? 1 : 0);
