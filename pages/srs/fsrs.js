// FSRS-6 scheduler — pure functions, no I/O (see DESIGN.md).
//
// Card state shape (all times are epoch milliseconds):
//   { stability, difficulty, due, lastReview, reps, lapses }
// A card that has never been reviewed has no state (null/undefined).
// State written by the earlier FSRS-4.5 scheduler is read as-is: stability
// and difficulty live on the same scales, so no migration is needed.
//
// Grades: 1 = Again, 2 = Hard, 3 = Good, 4 = Easy.
//
// The one impure-looking input, `now`, is always passed in explicitly so the
// whole module is testable in isolation:
//   FSRS.review(state, grade, now) → newState
window.FSRS = (function () {
  "use strict";

  // Default FSRS-6 weights (open-spaced-repetition defaults, 21 params).
  const W = [
    0.212, 1.2931, 2.3065, 8.2956, 6.4133, 0.8334, 3.0194, 0.001,
    1.8722, 0.1666, 0.796, 1.4835, 0.0614, 0.2629, 1.6483, 0.6014,
    1.8729, 0.5425, 0.0912, 0.0658, 0.1542,
  ];
  const DECAY = -W[20]; // FSRS-6: the forgetting-curve shape is itself a weight
  const FACTOR = Math.pow(0.9, 1 / DECAY) - 1;
  const REQUEST_RETENTION = 0.9;
  const MAX_INTERVAL_DAYS = 36500;
  const MIN_STABILITY = 0.001;
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
    return Math.max(W[grade - 1], MIN_STABILITY);
  }

  // D0 may fall outside [1,10] (D0(Easy) is negative with default weights);
  // the raw value is what mean reversion pulls toward, per the reference impl.
  function rawInitDifficulty(grade) {
    return W[4] - Math.exp(W[5] * (grade - 1)) + 1;
  }

  function initDifficulty(grade) {
    return clamp(rawInitDifficulty(grade), 1, 10);
  }

  function nextDifficulty(d, grade) {
    const delta = -W[6] * (grade - 3);
    const damped = d + delta * (10 - d) / 9; // linear damping near D=10
    // mean reversion toward D0(Easy)
    return clamp(W[7] * rawInitDifficulty(EASY) + (1 - W[7]) * damped, 1, 10);
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
    const longTerm = W[11] *
      Math.pow(d, -W[12]) *
      (Math.pow(s + 1, W[13]) - 1) *
      Math.exp(W[14] * (1 - r));
    // a lapse can keep at most the short-term fraction of the old stability
    return Math.min(longTerm, s / Math.exp(W[17] * W[18]));
  }

  // Same-day review (< 1 day since the last one): the short-term memory
  // model — grades move stability even when retrievability is still ~1.
  function shortTermStability(s, grade) {
    let inc = Math.exp(W[17] * (grade - 3 + W[18])) * Math.pow(s, -W[19]);
    if (grade === GOOD || grade === EASY) inc = Math.max(inc, 1); // a pass never shrinks a card
    return s * inc;
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
      difficulty = nextDifficulty(state.difficulty, grade);
      if (elapsedDays < 1) {
        stability = shortTermStability(state.stability, grade);
      } else {
        const r = retrievability(elapsedDays, state.stability);
        stability = grade === AGAIN
          ? nextForgetStability(state.difficulty, state.stability, r)
          : nextRecallStability(state.difficulty, state.stability, r, grade);
      }
      lapses = grade === AGAIN ? state.lapses + 1 : state.lapses;
      reps = state.reps + 1;
    }
    stability = Math.max(stability, MIN_STABILITY);

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
    _internals: { W, initStability, initDifficulty, nextDifficulty, nextRecallStability, nextForgetStability, shortTermStability },
  };
})();
