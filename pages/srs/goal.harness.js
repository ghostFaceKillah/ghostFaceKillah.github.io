// Browser harness for the SRS app's daily goal (see DESIGN.md, "Daily goal").
// cloud.js is stubbed out, the Firebase CDN is blocked and the clock is fake,
// so the day, the pace of a session and the cloud's answers are all under the
// test's control. The fixture is a backlog after a break: 120 overdue reviews
// plus 50 unseen cards in the picked decks, with a logged pace of 20 s a card
// (shrunk to ~16 s by the yardstick), so a 10-minute goal holds ~35 cards and
// whole sessions run in seconds. Signed-in scenarios inject the same
// FAKE_CLOUD as sync.harness.js.
//
// Run (needs node, playwright with chromium, and any static server at the
// repo root):
//   npx http-server . -p 8765 -s &
//   node pages/srs/goal.harness.js 8765
"use strict";
const { chromium } = require("playwright");

const PORT = process.argv[2] || "8765";
const URL = `http://127.0.0.1:${PORT}/pages/srs/index.html`;
const T0 = new Date("2026-10-24T10:00:00");          // local time
const DAY = 86400000;
// "today" is a local calendar day on both sides of the fixture — make the
// browser's zone match the one node builds the seed in
const TZ = Intl.DateTimeFormat().resolvedOptions().timeZone;

let failures = 0;
function check(name, cond, detail) {
  console.log((cond ? "  ok  " : "FAIL  ") + name + (detail !== undefined ? `  [${detail}]` : ""));
  if (!cond) failures++;
}

const FAKE_CLOUD = `
  window.__pushes = { cards: [], settings: [], logs: [] };
  window.__fetchResult = { cards: {}, settings: null, log: {} };
  window.__fetchDelay = 0;
  window.__fetchCalls = 0;
  let user = undefined;             // undefined = auth not yet resolved (like the real one)
  const listeners = [];
  window.SRS_CLOUD = {
    get user() { return user; },
    onUserChange(cb) { listeners.push(cb); if (user !== undefined) cb(user); },
    async signIn() {}, async signOut() {},
    pushCard(id, s) { window.__pushes.cards.push([id, s]); },
    pushSettings(s) { window.__pushes.settings.push(JSON.parse(JSON.stringify(s))); },
    pushLog(m, e) { window.__pushes.logs.push([m, e.length]); },
    async fetchAll() {
      window.__fetchCalls++;
      if (window.__fetchDelay) await new Promise(r => setTimeout(r, window.__fetchDelay));
      return JSON.parse(JSON.stringify(window.__fetchResult));
    },
    async importAll() {},
  };
  window.__setUser = u => { user = u; listeners.forEach(cb => cb(u)); };
  window.dispatchEvent(new CustomEvent("srs-cloud-ready"));
`;

async function newPage(browser, seed, { time = T0 } = {}) {
  const ctx = await browser.newContext({ timezoneId: TZ });
  const page = await ctx.newPage();
  const errors = [];
  page.on("pageerror", e => { errors.push(e.message); console.log("   PAGE ERROR:", e.message); });
  page.on("console", m => { if (m.type() === "error") console.log("   console.error:", m.text()); });
  await page.route("**/pages/srs/cloud.js", r => r.fulfill({ contentType: "text/javascript", body: "// stubbed by harness" }));
  await page.route("**/gstatic.com/**", r => r.abort());
  await page.clock.install({ time });
  // seed once per context — a reload must keep what the app wrote since
  await page.addInitScript(seed => {
    if (localStorage.getItem("harness:seeded")) return;
    for (const [k, v] of Object.entries(seed)) localStorage.setItem(k, JSON.stringify(v));
    localStorage.setItem("harness:seeded", "1");
  }, seed || {});
  await page.goto(URL);
  await page.waitForFunction(() => window.SRS_REGISTRY && document.getElementById("statDue"));
  return { ctx, page, errors };
}

const active = page => page.evaluate(() => document.querySelector(".screen.active").id);
const text = (page, sel) => page.evaluate(sel => { const el = document.querySelector(sel); return el ? el.textContent : null; }, sel);
const shown = (page, sel) => page.evaluate(sel => { const el = document.querySelector(sel); return !!el && getComputedStyle(el).display !== "none"; }, sel);
const hasClass = (page, sel, cls) => page.evaluate(([sel, cls]) => document.querySelector(sel).classList.contains(cls), [sel, cls]);
const count = (page, sel) => page.evaluate(sel => document.querySelectorAll(sel).length, sel);
const ls = (page, key) => page.evaluate(k => JSON.parse(localStorage.getItem(k) || "null"), key);
const startBtn = page => page.evaluate(() => {
  const b = document.getElementById("startBtn");
  return { text: b.textContent, disabled: b.disabled, syncing: b.classList.contains("syncing") };
});
const settle = page => page.evaluate(() => new Promise(r => setTimeout(r, 0)));
const goalSel = page => page.evaluate(() =>
  [...document.querySelectorAll("#goalToggle .opt.sel")].map(b => b.dataset.min).join(","));

// The SDK "fails to load": identity settles as a guest (the real path when
// the Firebase CDN is unreachable), so Start stops reading "connecting…".
async function asGuest(page) {
  await page.evaluate(() => window.dispatchEvent(new CustomEvent("srs-cloud-unavailable")));
  await settle(page);
}
async function signIn(page, fetchResult) {
  await page.evaluate(FAKE_CLOUD);
  await page.evaluate(r => { window.__fetchResult = r; }, fetchResult);
  await page.evaluate(() => window.__setUser({ uid: "u1", displayName: "Tester" }));
  await page.waitForFunction(() => window.__fetchCalls >= 1);
  await settle(page);
}
async function tapGoal(page, min) {
  await page.click(`#goalToggle .opt[data-min="${min}"]`);
  await page.waitForFunction(min =>
    document.querySelector(`#goalToggle .opt[data-min="${min}"]`).classList.contains("sel"), min);
  await settle(page);
}

// Works a review session until it ends on its own: Again on the first sight
// of every `againEvery`-th card (≈ 1 in 5), Good otherwise (so every lapse is
// re-passed in the same session), `stepMs` of fake time on each card. Reveal
// and grade by keyboard, or by tapping the card and the grade buttons.
// Returns the card ids served and what the progress line said for each.
async function work(page, { stepMs = 3000, againEvery = 5, clicks = false } = {}) {
  const served = [], progress = [], seen = new Set();
  let firsts = 0, grades = 0;
  while ((await active(page)) === "review" && grades < 500) {
    const cur = await page.evaluate(() => ({
      id: current && current.id,
      progress: document.getElementById("progressText").textContent,
    }));
    served.push(cur.id);
    progress.push(cur.progress);
    await page.clock.runFor(stepMs);                 // time on the card
    const again = !seen.has(cur.id) && ++firsts % againEvery === 0;
    seen.add(cur.id);
    if (clicks) {
      await page.click("#card");
      await page.waitForSelector("#grades.show");
      await page.click(again ? ".g-again" : ".g-good");
    } else {
      await page.keyboard.press("Space");
      await page.keyboard.press(again ? "1" : "3");
    }
    grades++;
  }
  return { served, progress, grades, again: Math.floor(firsts / againEvery), screen: await active(page) };
}

// ----- Fixture: a backlog after a break ---------------------------------------
const GROUPS = { "recall/hsk1-1-50": true, "recall/hsk1-51-100": true, "recall/hsk1-101-150": true, "coll/1-20": true };
const N_DUE = 120;            // recall cards 1–120 seen and overdue; 121–150 + coll 1–20 unseen
const N_UNSEEN = 50;
const monthOf = ts => { const d = new Date(ts); return d.getFullYear() + "-" + String(d.getMonth() + 1).padStart(2, "0"); };
const logLine = (ts, grade, ivl, id, ms) => `${ts},${grade},${ivl},${id},${ms}`;
function addLog(seed, ns, line) {
  const k = ns + "log:" + monthOf(+line.split(",")[0]);
  (seed[k] = seed[k] || []).push(line);
}
function sortLogs(seed) {
  for (const k of Object.keys(seed)) if (k.includes(":log:")) seed[k].sort((a, b) => +a.split(",")[0] - +b.split(",")[0]);
  return seed;
}
function backlog(ids, ns, t0, settings) {
  const t = +t0;
  const cards = {};
  const seed = { [ns + "cards"]: cards, [ns + "settings"]: Object.assign({ groups: GROUPS, newPerDay: 15 }, settings) };
  ids.recall.slice(0, N_DUE).forEach((id, i) => {
    const ivl = 2 + (i * 7) % 29;                         // 2–30 day interval
    const due = t - (1 + i % 12) * DAY - i * 60000;       // 1–12 days overdue, no ties
    const lastReview = due - ivl * DAY;
    cards[id] = { stability: ivl, difficulty: 5, due, lastReview, reps: 3, lapses: 0 };
    addLog(seed, ns, logLine(lastReview, 3, Math.max(1, Math.round(ivl / 2)), id, 20000));
  });
  return sortLogs(seed);
}

(async () => {
  const browser = await chromium.launch();
  console.log(`\n=== daily goal harness on port ${PORT} (tz ${TZ}) ===`);

  // card ids straight from the registry, so the fixture follows the decks
  let ids;
  {
    const { ctx, page } = await newPage(browser, {});
    ids = await page.evaluate(() => {
      const R = window.SRS_REGISTRY;
      const g = (deck, id) => R.deckById[deck].groups.find(x => x.id === id).cards.map(c => c.id);
      return {
        recall: [...g("recall", "hsk1-1-50"), ...g("recall", "hsk1-51-100"), ...g("recall", "hsk1-101-150")],
        coll: g("coll", "1-20"),
      };
    });
    await ctx.close();
  }
  console.log(`   fixture: ${N_DUE} overdue + ${N_UNSEEN} unseen of ${ids.recall.length + ids.coll.length} enabled cards`);

  // ---- A–E + I: one guest's day, from goal off to the plot in full bloom ----
  {
    const { ctx, page, errors } = await newPage(browser, backlog(ids, "srs:anon:", T0, {}));
    await asGuest(page);

    console.log("\n[A] Goal off — the app as before");
    const pace = await page.evaluate(() => { const c = goalPrior(Date.now()).cost; return [c.pass, c.lapse, c.learn].map(ms => Math.round(ms / 100) / 10); });
    console.log(`   yardstick: pass ${pace[0]} s · lapse ${pace[1]} s · learn ${pace[2]} s`);
    const a = await startBtn(page);
    check("Start reads plain 'Start review'", !a.disabled && a.text === "Start review", a.text);
    check("goal toggle shows 'off'", (await goalSel(page)) === "0", await goalSel(page));
    check("#statDue = every due card", (await text(page, "#statDue")) === String(N_DUE), await text(page, "#statDue"));
    check("#statDueLabel 'due now'", (await text(page, "#statDueLabel")) === "due now", await text(page, "#statDueLabel"));
    check("#statNew = today's 15 new", (await text(page, "#statNew")) === "15", await text(page, "#statNew"));
    check("bed draws all due + 15 new", (await count(page, "#plotBed > i")) === N_DUE + 15, await count(page, "#plotBed > i"));
    const moreA = await text(page, "#plotMore");
    check("#plotMore nudges toward a goal (> 30 min of work)",
      (await shown(page, "#plotMore")) && /in the ground today — a daily goal ↓ makes it bite-size/.test(moreA), moreA);
    const learnA = await text(page, "#learnCount");
    check("learn caption '15 new today · 35 waiting'", learnA === `15 new today · ${N_UNSEEN - 15} waiting`, learnA);

    console.log("\n[I] Stats screen, goal off");
    await page.click("#statsBtn");
    await page.waitForSelector("#statsScreen.active");
    check("#goalNote hidden", !(await shown(page, "#goalNote")), await text(page, "#goalNote"));
    check("no .goal-line in #burnChart", (await count(page, "#burnChart .goal-line")) === 0);
    await page.click("#statsBack");

    console.log("\n[B] Tap '10m' (guest)");
    await tapGoal(page, 10);
    const checkGoalHome = async when => {
      check(`${when}: 10m button selected`, (await goalSel(page)) === "10", await goalSel(page));
      const due = +(await text(page, "#statDue"));
      check(`${when}: #statDue shrinks to the goal`, due > 0 && due < N_DUE, due);
      check(`${when}: #statDueLabel 'in today's goal'`, (await text(page, "#statDueLabel")) === "in today's goal", await text(page, "#statDueLabel"));
      const more = await text(page, "#plotMore");
      check(`${when}: #plotMore '＋ N more due · M new can wait'`,
        (await shown(page, "#plotMore")) &&
        new RegExp(`^＋ ${N_DUE - due} more due · 15 new can wait — oldest first next time$`).test(more), more);
      check(`${when}: bed draws the goal only`, (await count(page, "#plotBed > i")) === due, await count(page, "#plotBed > i"));
      const learn = await text(page, "#learnCount");
      check(`${when}: learn caption 'paused · reviews first'`, /^paused · reviews first · 50 waiting$/.test(learn), learn);
      const s = await startBtn(page);
      check(`${when}: Start 'Start review · ≈ 10–11m'`, !s.disabled && /^Start review · ≈ 1[01]m$/.test(s.text), s.text);
      return due;
    };
    const goalDue = await checkGoalHome("after tap");
    check("minutesPerDay 10 saved in srs:anon:settings", (await ls(page, "srs:anon:settings")).minutesPerDay === 10);
    console.log(`   goal holds ${goalDue} of ${N_DUE} due cards`);
    await page.reload();
    await page.waitForFunction(() => window.SRS_REGISTRY && document.getElementById("statDue"));
    await asGuest(page);
    await checkGoalHome("after reload");

    console.log("\n[I] Stats screen, 10m goal");
    await page.click("#statsBtn");
    await page.waitForSelector("#statsScreen.active");
    const note = await text(page, "#goalNote");
    check("#goalNote visible and non-empty", (await shown(page, "#goalNote")) && note.trim().length > 0, note);
    check("#burnChart has a .goal-line", (await count(page, "#burnChart .goal-line")) === 1, await text(page, "#burnChart .goal-line"));
    await page.click("#statsBack");

    console.log("\n[C] Work the goal session to the end (~1 in 5 Again)");
    await page.click("#startBtn");
    await page.waitForSelector("#review.active");
    const c = await work(page);
    console.log(`   ${c.grades} grades (${c.again} Again), first progress "${c.progress[0]}"`);
    check("session ended on its own", c.screen === "doneScreen", c.screen);
    check("progress line carried '≈ Xm' under the goal", c.progress.every(p => /· ≈ (\d+m|<1m)$/.test(p)), c.progress.find(p => !/≈/.test(p)));
    check("progress line not marked bonus", !c.progress.some(p => p.startsWith("bonus")));
    check("served exactly the goal's due cards", new Set(c.served).size === goalDue, `${new Set(c.served).size} vs ${goalDue}`);
    check("done: 'Today's goal is in bloom'", (await text(page, "#doneBig")) === "Today's goal is in bloom", await text(page, "#doneBig"));
    check("done: bloom hero shown", await shown(page, "#bloomHero"));
    const keepC = await text(page, "#keepGoingBtn");
    check("#keepGoingBtn visible '🌱 Keep going · N more ≈ Xm'",
      (await shown(page, "#keepGoingBtn")) && /^🌱 Keep going · \d+ more ≈ (\d+m|<1m)$/.test(keepC), keepC);
    await page.click("#homeBtn");
    await page.waitForSelector("#home.active");
    check("home: #plotBed blooms", await hasClass(page, "#plotBed", "bloom"));
    check("home: #plotSub 'today's goal in bloom'", /today's goal in bloom/.test(await text(page, "#plotSub")), await text(page, "#plotSub"));
    const sc = await startBtn(page);
    check("home: Start offers 'Keep going'", !sc.disabled && /^🌱 Keep going · ≈ (\d+m|<1m) more$/.test(sc.text), sc.text);
    check("home: #statDue 0", (await text(page, "#statDue")) === "0", await text(page, "#statDue"));

    // (D starts from home's "Keep going"; E then uses the done screen's #keepGoingBtn —
    // both buttons run the same dailyQueue helping)
    console.log("\n[D] A bonus helping past the met goal");
    await page.click("#startBtn");
    await page.waitForSelector("#review.active");
    const d = await work(page, { clicks: true });
    console.log(`   ${d.grades} grades (${d.again} Again), first progress "${d.progress[0]}"`);
    check("progress line reads 'bonus · …'", d.progress.length > 0 && d.progress.every(p => p.startsWith("bonus · ")), d.progress[0]);
    check("no card from the goal served again", !d.served.some(id => c.served.includes(id)));
    const dTitle = await text(page, "#doneBig");
    check("done: 'Bonus round done 🌼' (or full bloom if it cleared)",
      dTitle === "Bonus round done 🌼" || dTitle === "Today's plot is in full bloom", dTitle);
    check("the bed stays bloomed", await hasClass(page, "#plotBed", "bloom"));

    console.log("\n[E] Keep going until nothing is left");
    let rounds = 0, lastTitle = dTitle, allBonus = true;
    while ((await active(page)) === "doneScreen" && (await shown(page, "#keepGoingBtn")) && rounds < 10) {
      await page.click("#keepGoingBtn");
      await page.waitForSelector("#review.active");
      const e = await work(page);
      rounds++;
      allBonus = allBonus && e.progress.every(p => p.startsWith("bonus · "));
      lastTitle = await text(page, "#doneBig");
      console.log(`   helping ${rounds}: ${e.grades} grades → "${lastTitle}"`);
      if (lastTitle !== "Today's plot is in full bloom") {
        check(`helping ${rounds} ends 'Bonus round done 🌼'`, lastTitle === "Bonus round done 🌼", lastTitle);
        check(`helping ${rounds}: the bed stays bloomed`, await hasClass(page, "#plotBed", "bloom"));
      }
    }
    check("#keepGoingBtn helpings are bonus rounds too", rounds > 0 && allBonus);
    check("final: 'Today's plot is in full bloom'", lastTitle === "Today's plot is in full bloom", lastTitle);
    check("final: #keepGoingBtn hidden", !(await shown(page, "#keepGoingBtn")));
    await page.click("#homeBtn");
    await page.waitForSelector("#home.active");
    const se = await startBtn(page);
    check("home: Start 'All done for today 🎉'", se.disabled && se.text === "All done for today 🎉", se.text);
    check("home: bed in full bloom", (await hasClass(page, "#plotBed", "bloom")) && /in full bloom/.test(await text(page, "#plotSub")), await text(page, "#plotSub"));
    check("all due + 15 new put to bed", (await text(page, "#plotDone")) === String(N_DUE + 15), await text(page, "#plotDone"));
    check("[A–E, I] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  // ---- F. Cloud settings doc from before the goal existed ----
  console.log("\n[F] Signed in; the cloud's settings doc has no minutesPerDay");
  {
    const seed = backlog(ids, "srs:u1:", T0, { minutesPerDay: 15 });
    seed["srs:u1:importOffered"] = true;
    const { ctx, page, errors } = await newPage(browser, seed);
    await signIn(page, { cards: {}, settings: { groups: GROUPS, newPerDay: 15 }, log: {} });
    check("goal stays 15m after hydration", (await goalSel(page)) === "15", await goalSel(page));
    check("srs:u1:settings keeps minutesPerDay 15", (await ls(page, "srs:u1:settings")).minutesPerDay === 15);
    const healed = await page.evaluate(() => window.__pushes.settings.map(s => s.minutesPerDay));
    check("cloud doc healed: settings pushed with minutesPerDay 15", healed.includes(15), JSON.stringify(healed));
    const sf = await startBtn(page);
    check("Start quotes the 15m goal", /^Start review · ≈ 1[56]m$/.test(sf.text), sf.text);
    await tapGoal(page, 20);
    const after = await page.evaluate(() => window.__pushes.settings.map(s => s.minutesPerDay));
    check("tapping 20m pushes settings with 20", after[after.length - 1] === 20, JSON.stringify(after));
    // the other half of "absent isn't off": an explicit 0 in the cloud is honored
    await page.clock.fastForward(61000);
    await page.evaluate(() => { window.__fetchResult.settings = Object.assign({}, window.__fetchResult.settings, { minutesPerDay: 0 }); });
    await page.evaluate(() => window.dispatchEvent(new Event("focus")));
    await page.waitForFunction(() => window.__fetchCalls >= 2);
    await settle(page);
    check("an explicit 'off' from the cloud is adopted", (await goalSel(page)) === "0", await goalSel(page));
    check("[F] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  // ---- G. Another device's lapse arrives mid-session ----
  console.log("\n[G] Top-up: another device's lapse lands mid-session");
  {
    const t = +T0;
    const X = ids.recall[N_DUE];             // seen here yesterday, planted until next week
    const seed = backlog(ids, "srs:u1:", T0, { minutesPerDay: 10 });
    seed["srs:u1:importOffered"] = true;
    seed["srs:u1:cards"][X] = { stability: 6, difficulty: 5, due: t + 5 * DAY, lastReview: t - DAY, reps: 4, lapses: 0 };
    addLog(seed, "srs:u1:", logLine(t - DAY, 3, 3, X, 20000));
    sortLogs(seed);
    const { ctx, page, errors } = await newPage(browser, seed);
    const settings = { groups: GROUPS, newPerDay: 15, minutesPerDay: 10 };
    await signIn(page, { cards: {}, settings, log: {} });
    const inGoal0 = await page.evaluate(x => todaysPlot(Date.now()).goal.some(c => c.id === x), X);
    check("X is not in the goal on this device's state", !inGoal0);
    // this morning, on another device: X forgotten (Again), its 10-minute step still due
    const failed = { stability: 1.2, difficulty: 6.5, due: t - 50 * 60000, lastReview: t - 3600000, reps: 5, lapses: 1 };
    await page.clock.fastForward(61000);     // past STALE_MS → Start refreshes
    await page.evaluate(([X, failed, settings, month, line]) => {
      window.__fetchResult = { cards: { [X]: failed }, settings, log: { [month]: [line] } };
      window.__fetchDelay = 20000;           // well past the 5 s session-start timeout
    }, [X, failed, settings, monthOf(t), logLine(t - 3600000, 1, 6, X, 15000)]);
    await page.click("#startBtn");
    await settle(page);
    check("Start shows it is syncing", (await startBtn(page)).syncing);
    await page.clock.runFor(5200);           // session-start timeout → opens on local state
    await page.waitForSelector("#review.active", { timeout: 5000 });
    const g = await work(page);
    const landed = await page.evaluate(x => cardStates[x].lapses, X);
    console.log(`   ${g.grades} grades; X served ${g.served.filter(id => id === X).length}×; fetches ${await page.evaluate(() => window.__fetchCalls)}`);
    check("the slow fetch landed mid-session (X's lapse adopted)", landed >= 1 && (await page.evaluate(() => window.__fetchCalls)) === 2, `X.lapses=${landed}`);
    check("X's relearning step served before the done screen", g.served.includes(X),
      `X at plan.more[${await page.evaluate(x => todaysPlot(Date.now()).more.findIndex(c => c.id === x), X)}]`);
    const plan = await page.evaluate(() => { const p = todaysPlot(Date.now()); return { goal: p.goal.length, relearnN: p.relearnN, spent: Math.round(p.spentMs / 1000), budget: p.budgetMs / 1000, met: p.met }; });
    console.log(`   plan at the done screen: ${JSON.stringify(plan)}`);
    const gTitle = await text(page, "#doneBig");
    check("session still ends met", g.screen === "doneScreen" && /^Today's (goal|plot) is in (full )?bloom$/.test(gTitle), gTitle);
    check("[G] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  // ---- H. A goal session that runs past midnight ----
  console.log("\n[H] Session started at 23:58:30, finished after midnight");
  {
    const TH = new Date("2026-10-24T23:58:30");
    const { ctx, page, errors } = await newPage(browser, backlog(ids, "srs:anon:", TH, { minutesPerDay: 10 }), { time: TH });
    await asGuest(page);
    const sh = await startBtn(page);
    check("goal session on offer before midnight", /^Start review · ≈ 1[01]m$/.test(sh.text), sh.text);
    await page.click("#startBtn");
    await page.waitForSelector("#review.active");
    const h = await work(page, { stepMs: 5000, clicks: true });
    const endDay = await page.evaluate(() => new Date().getDate());
    console.log(`   ${h.grades} grades; ended ${await page.evaluate(() => new Date().toString())}`);
    check("the session crossed midnight", endDay === 25);
    check("session ended", h.screen === "doneScreen", h.screen);
    const hTitle = await text(page, "#doneBig");
    check("done screen claims no bloom", !/in bloom|full bloom/.test(hTitle) && hTitle === "All clear! 🎉", hTitle);
    check("no bloom hero", !(await shown(page, "#bloomHero")));
    check("no keep-going offer", !(await shown(page, "#keepGoingBtn")));
    check("[H] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  // ---- J. The goal tapped, then Start, while a slow rebase is in flight ----
  console.log("\n[J] Tap '10m' then Start on a slow connection");
  {
    const seed = backlog(ids, "srs:u1:", T0, {});
    seed["srs:u1:importOffered"] = true;
    const { ctx, page, errors } = await newPage(browser, seed);
    const settings = { groups: GROUPS, newPerDay: 15 };      // the cloud doc: no goal yet
    await signIn(page, { cards: {}, settings, log: {} });
    await page.clock.fastForward(61000);                     // stale → both taps rebase
    await page.evaluate(() => { window.__fetchDelay = 20000; });
    await page.click(`#goalToggle .opt[data-min="10"]`);
    await settle(page);
    check("the goal shows as picked at once", (await goalSel(page)) === "10", await goalSel(page));
    await page.click("#startBtn");
    await page.clock.runFor(5200);                           // Start gives up waiting → opens
    await page.waitForSelector("#review.active", { timeout: 5000 });
    const queued = await page.evaluate(() => queue.length);
    check("the session serves the 10m goal, not the whole pile", queued < 60, `${queued} cards queued`);
    await page.clock.runFor(16000);                          // the fetch lands mid-session
    await settle(page);
    const pushed = await page.evaluate(() => window.__pushes.settings.map(s => s.minutesPerDay));
    check("after the rebase the goal is kept and pushed", pushed[pushed.length - 1] === 10 &&
      (await page.evaluate(() => settings.minutesPerDay)) === 10, JSON.stringify(pushed));
    check("[J] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  await browser.close();
  console.log(failures ? `\n${failures} FAILURE(S)` : "\nAll harness checks passed.");
  process.exit(failures ? 1 : 0);
})().catch(e => { console.error(e); process.exit(2); });
