// FSRS-4.5 scheduler — pure functions, no I/O (see DESIGN.md).
//
// Card state shape (all times are epoch milliseconds):
//   { stability, difficulty, due, lastReview, reps, lapses }
// A card that has never been reviewed has no state (null/undefined).
//
// Grades: 1 = Again, 2 = Hard, 3 = Good, 4 = Easy.
//
// The one impure-looking input, `now`, is always passed in explicitly so the
// whole module is testable in isolation:
//   FSRS.review(state, grade, now) → newState
window.FSRS = (function () {
  "use strict";

  // Default FSRS-4.5 weights.
  const W = [
    0.4872, 1.4003, 3.7145, 13.8206, 5.1618, 1.2298, 0.8975, 0.031,
    1.6474, 0.1367, 1.0461, 2.1072, 0.0793, 0.3246, 1.587, 0.2272, 2.8755,
  ];
  const DECAY = -0.5;
  const FACTOR = 19 / 81; // 0.9 ** (1 / DECAY) - 1
  const REQUEST_RETENTION = 0.9;
  const MAX_INTERVAL_DAYS = 36500;
  const AGAIN_DELAY_MS = 10 * 60 * 1000; // relearning step: back in ~10 min
  const DAY_MS = 24 * 60 * 60 * 1000;

  const AGAIN = 1, HARD = 2, GOOD = 3, EASY = 4;

  const clamp = (x, lo, hi) => Math.min(hi, Math.max(lo, x));

  // Probability of recall after `elapsedDays` at stability S.
  function retrievability(elapsedDays, stability) {
    return Math.pow(1 + FACTOR * (elapsedDays / stability), DECAY);
  }

  // Interval (days) that hits the requested retention at stability S.
  function intervalDays(stability) {
    const days = (stability / FACTOR) * (Math.pow(REQUEST_RETENTION, 1 / DECAY) - 1);
    return clamp(Math.round(days), 1, MAX_INTERVAL_DAYS);
  }

  function initStability(grade) {
    return Math.max(W[grade - 1], 0.1);
  }

  function initDifficulty(grade) {
    return clamp(W[4] - (grade - 3) * W[5], 1, 10);
  }

  function nextDifficulty(d, grade) {
    const next = d - W[6] * (grade - 3);
    // mean reversion toward D0(Easy)
    return clamp(W[7] * initDifficulty(EASY) + (1 - W[7]) * next, 1, 10);
  }

  function nextRecallStability(d, s, r, grade) {
    const hardPenalty = grade === HARD ? W[15] : 1;
    const easyBonus = grade === EASY ? W[16] : 1;
    return s * (1 +
      Math.exp(W[8]) *
      (11 - d) *
      Math.pow(s, -W[9]) *
      (Math.exp(W[10] * (1 - r)) - 1) *
      hardPenalty *
      easyBonus);
  }

  function nextForgetStability(d, s, r) {
    const sf = W[11] *
      Math.pow(d, -W[12]) *
      (Math.pow(s + 1, W[13]) - 1) *
      Math.exp(W[14] * (1 - r));
    return Math.min(sf, s); // a lapse never increases stability
  }

  // The scheduler: (cardState|null, grade, nowMs) → newState.
  function review(state, grade, now) {
    if (!(grade >= AGAIN && grade <= EASY)) throw new Error("FSRS: bad grade " + grade);

    let stability, difficulty, lapses, reps;
    if (!state || !state.reps) {
      // First review of a new card.
      stability = initStability(grade);
      difficulty = initDifficulty(grade);
      reps = 1;
      lapses = grade === AGAIN ? 1 : 0;
    } else {
      const elapsedDays = Math.max(0, (now - state.lastReview) / DAY_MS);
      const r = retrievability(elapsedDays, state.stability);
      difficulty = nextDifficulty(state.difficulty, grade);
      if (grade === AGAIN) {
        stability = nextForgetStability(state.difficulty, state.stability, r);
        lapses = state.lapses + 1;
      } else {
        stability = nextRecallStability(state.difficulty, state.stability, r, grade);
        lapses = state.lapses;
      }
      reps = state.reps + 1;
    }

    const due = grade === AGAIN
      ? now + AGAIN_DELAY_MS
      : now + intervalDays(stability) * DAY_MS;

    return {
      stability,
      difficulty,
      due,
      lastReview: now,
      reps,
      lapses,
    };
  }

  // Preview the due delay for each grade — used to label the grade buttons.
  // Returns { 1: ms, 2: ms, 3: ms, 4: ms } of delay-from-now per grade.
  function previewIntervals(state, now) {
    const out = {};
    for (const g of [AGAIN, HARD, GOOD, EASY]) {
      out[g] = review(state, g, now).due - now;
    }
    return out;
  }

  return {
    AGAIN, HARD, GOOD, EASY,
    DAY_MS,
    review,
    previewIntervals,
    retrievability,
    intervalDays,
    // exposed for tests
    _internals: { W, initStability, initDifficulty, nextDifficulty, nextRecallStability, nextForgetStability },
  };
})();
