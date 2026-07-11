// Tests for the FSRS scheduler. Run with:  node pages/srs/fsrs.test.js
"use strict";
const fs = require("fs");
const path = require("path");

global.window = {};
new Function("window", fs.readFileSync(path.join(__dirname, "fsrs.js"), "utf8"))(global.window);
const FSRS = global.window.FSRS;
const { AGAIN, HARD, GOOD, EASY, DAY_MS } = FSRS;

let failures = 0;
function check(name, cond, detail) {
  if (cond) console.log("  ok  " + name);
  else { failures++; console.log("FAIL  " + name + (detail ? " — " + detail : "")); }
}
const days = ms => ms / DAY_MS;

const T0 = Date.UTC(2026, 0, 1, 12, 0, 0);

// ----- new cards ------------------------------------------------------------
{
  const s = FSRS.review(null, GOOD, T0);
  check("new+Good: reps=1, no lapse", s.reps === 1 && s.lapses === 0);
  check("new+Good: due ≈ stability days out",
    days(s.due - T0) >= 1 && days(s.due - T0) <= 7, `got ${days(s.due - T0)}d`);
  check("new+Good: difficulty in [1,10]", s.difficulty >= 1 && s.difficulty <= 10);

  const a = FSRS.review(null, AGAIN, T0);
  check("new+Again: counts a lapse and comes back within the hour",
    a.lapses === 1 && a.due - T0 <= 60 * 60 * 1000, `delay ${days(a.due - T0)}d`);

  const e = FSRS.review(null, EASY, T0);
  const h = FSRS.review(null, HARD, T0);
  check("new: Easy schedules further than Good, Good further than Hard",
    e.due > s.due && s.due > h.due);
}

// ----- grade ordering on review ----------------------------------------------
{
  let st = FSRS.review(null, GOOD, T0);
  const at = st.due; // review exactly when due
  const prev = FSRS.previewIntervals(st, at);
  check("review previews ordered Again < Hard < Good < Easy",
    prev[AGAIN] < prev[HARD] && prev[HARD] < prev[GOOD] && prev[GOOD] < prev[EASY]);
  check("review previews: all success grades at least 1 day",
    prev[HARD] >= DAY_MS && prev[GOOD] >= DAY_MS && prev[EASY] >= DAY_MS);
}

// ----- growth under repeated Good --------------------------------------------
{
  let st = null, t = T0;
  const intervals = [];
  for (let i = 0; i < 8; i++) {
    st = FSRS.review(st, GOOD, t);
    intervals.push(days(st.due - t));
    t = st.due;
  }
  const growing = intervals.every((iv, i) => i === 0 || iv >= intervals[i - 1]);
  check("8× Good: intervals never shrink", growing, intervals.map(x => x.toFixed(1)).join(", "));
  check("8× Good: spaced out beyond a month", intervals[7] > 30, `last ${intervals[7].toFixed(1)}d`);
  check("8× Good: reps counted", st.reps === 8);
}

// ----- lapse behavior ---------------------------------------------------------
{
  let st = null, t = T0;
  for (let i = 0; i < 4; i++) { st = FSRS.review(st, GOOD, t); t = st.due; }
  const before = st.stability;
  const lapsed = FSRS.review(st, AGAIN, t);
  check("Again after streak: stability drops", lapsed.stability < before,
    `${before.toFixed(1)} → ${lapsed.stability.toFixed(1)}`);
  check("Again after streak: lapse counted", lapsed.lapses === 1);
  check("Again after streak: relearn within the hour", lapsed.due - t <= 60 * 60 * 1000);

  // Recovering: next Good schedules ≥ 1 day again
  const rec = FSRS.review(lapsed, GOOD, lapsed.due);
  check("Good after lapse: back on a daily+ interval", rec.due - lapsed.due >= DAY_MS);
}

// ----- late & early reviews ----------------------------------------------------
{
  let st = FSRS.review(null, GOOD, T0);
  const onTime = FSRS.review(st, GOOD, st.due);
  const late = FSRS.review(st, GOOD, st.due + 30 * DAY_MS);
  check("late review (harder recall) earns more stability than on-time",
    late.stability > onTime.stability,
    `${late.stability.toFixed(1)} vs ${onTime.stability.toFixed(1)}`);

  const early = FSRS.review(st, GOOD, st.due - Math.floor((st.due - T0) / 2));
  check("early review earns less stability than on-time",
    early.stability < onTime.stability);
}

// ----- retrievability & interval round-trip ------------------------------------
{
  const r = FSRS.retrievability(FSRS.intervalDays(10), 10);
  check("interval targets ~90% retention", Math.abs(r - 0.9) < 0.02, `R=${r.toFixed(3)}`);
  check("R decays with time",
    FSRS.retrievability(1, 5) > FSRS.retrievability(10, 5));
  check("R at t=0 is 1", FSRS.retrievability(0, 5) === 1);
}

// ----- guards -------------------------------------------------------------------
{
  let threw = false;
  try { FSRS.review(null, 5, T0); } catch (e) { threw = true; }
  check("invalid grade throws", threw);
  const s = FSRS.review(null, GOOD, T0);
  check("difficulty clamped to [1,10] under extreme Easy spam", (() => {
    let st = s, t = s.due;
    for (let i = 0; i < 50; i++) { st = FSRS.review(st, EASY, t); t = st.due; }
    return st.difficulty >= 1 && st.difficulty <= 10;
  })());
}

console.log(failures === 0 ? "\nAll FSRS tests passed." : `\n${failures} FAILURE(S)`);
process.exit(failures ? 1 : 0);
