# SRS App — Design & Handoff Notes

Spaced repetition system covering all cards from the language trainers in `pages/`.
Decided 2026-07-11. This doc is the source of truth for sessions continuing the work.

## Decisions (all confirmed with the owner)

- **Standalone app** at `pages/srs/` — existing trainer pages stay untouched in look
  and behavior; no review buttons or header changes on them. The SRS app is the
  daily-review experience; trainers remain the browse/study experience.
- **Scheduler:** FSRS (grades Again / Hard / Good / Easy). Pure function
  `(cardState, grade, now) → newState`, no I/O, testable in isolation.
  Card state: `{stability, difficulty, due, lastReview, reps, lapses}`.
- **Card IDs:** stable, derived from existing data. Examples:
  `words/L1D1/你`, `sent/hsk1/42`, `coll/1`, `kana/あ`. ~2,500 cards total.
- **Card intake:** opt-in by deck/lesson (IC1 lessons individually, colloquial chars
  in bands of 20, IC1 sentence chapters, HSK recall sets of 50, kana rows split
  per script (hiragana and katakana selectable separately), radicals,
  numbers, usage). New cards introduced at a per-user daily cap (default ~15).
- **Selection groups** (decided 2026-07-11): three generic slots (①②③), each
  remembering its own set of enabled deck groups; switching slots swaps the whole
  selection (leave-slot keeps its picks, target slot's picks are recalled).
  Settings shape: `{ slot: 0..2, slots: [g0, g1, g2], groups, newPerDay }` where
  `groups` is a live in-memory REFERENCE to `slots[slot]` — every existing
  reader/writer of `settings.groups` transparently operates on the active slot.
  References don't survive JSON, so `normalizeSettings()` re-ties them on every
  load/adopt; legacy settings (plain `groups`) migrate to slot 0, and
  `migrateGroupKeys` runs per slot. Queues, stats, and the forecast all follow
  the active slot; the focus picker stays selection-independent by design.
- **Sentence directions** (decided 2026-07-11): Integrated Chinese sentences stay
  recognition cards (Chinese front → English back, deck `sent`). HSK sentences are
  production cards in their own deck `recall` (English front → Chinese back), split
  into opt-in sets of 50. HSK card IDs are `recall/hsk1/N` — deliberately new, so
  history from the retired Chinese-first `sent/hsk1/N` cards never schedules the
  flipped exercise. Old `sent/hsk1`, `sent/hsk2` group opt-ins migrate to all the
  matching `recall/*` groups (same `migrateGroupKeys` mechanism as earlier splits).
- **Four session modes, one engine** (decided 2026-07-11). Every mode grades
  through the same `FSRS.review` with real elapsed time — early/late reviews are
  handled by the math itself (early review ⇒ high retrievability ⇒ tiny stability
  gain), so no mode needs special scheduling rules:
  - **daily** — due cards + new cards up to the daily cap (the default habit).
  - **learn** — unseen cards only, one `newPerDay`-sized batch per session.
    Deliberately allowed past the daily cap (explicit user intent), but each card
    still counts as introduced, so the daily queue never double-adds new cards.
  - **focus** — every card of one chosen group (e.g. an IC1 chapter before a tutor
    lesson), due or not, enabled for daily review or not. Cramming a group does
    NOT opt it into the daily rotation — the deck chips stay the only intake switch.
  - **ahead** — extra rounds after the daily queue is clear: not-yet-due cards
    sorted by retrievability (weakest first), 20 at a time, repeatable.
- **Stats screen** (decided 2026-07-11): the three idiomatic SRS charts, computed
  entirely from existing state (`days` + card states, nothing new persisted):
  review heatmap + streak (26 Monday-first weeks, GitHub-style), a card-maturity
  "garden" (🌱 <7d, 🌿 7–21d, 🌸 ≥21d — Anki's 21-day "mature" idiom), and a
  28-day due forecast for enabled decks. Styling (owner iterated twice on
  2026-07-11: single-hue ramps felt monochrome → kana rainbow → final ask:
  chart colors must be EXACTLY the four grade-button hues, and heatmap days
  should be cute emoji, not dots — a different one per day): all chart vars
  alias the button colors (`--gs/--gg/--gb` = red/amber/green for 🌱/🌿/🌸 —
  traffic-light, maturing toward "Good"; `--w1..--w4` = red/amber/green/sky
  in Again→Easy order; `--ret` = green, "Good" = remembered). Aliases resolve
  at use-time, so dark mode follows the buttons' dark values with no
  per-theme chart overrides. Heatmap: a reviewed day is a cute emoji — an
  8-strong set (coprime with the 7 weekday rows) cycled by calendar date, so
  each day keeps its emoji forever — whose font-size carries the review count
  (s1–s4); the month-hue cycle (`--m1..--m5`) is gone. Magnitude never rides
  on color, stages carry emoji + labeled counts, weeks carry axis labels, and
  every mark has a tooltip + aria-label.
- **Review log & retention** (decided 2026-07-11): every grade appends
  `"ts,grade,prevIvl,cardId"` to a month-chunked log (`log:YYYY-MM` in
  localStorage; `users/{uid}/log/{YYYY-MM}` docs in Firestore — covered by the
  existing per-UID wildcard rules, no rules redeploy needed; each push rewrites
  the month doc so offline writes coalesce). `prevIvl` is the whole-day
  interval carried into the review, `-1` for a first sight. Retention is
  Anki-style *true retention*: per day, only the first review of each
  already-learned card counts (new cards and same-session relearning repeats
  excluded); remembered = graded above Again. The stats screen shows a 30-day
  headline + 12 weekly bars against a 90% goal line (`--ret`, the green
  "Good" button hue), and heatmap tooltips gain "· N% remembered". The log
  starts empty — history from before this feature has no retention data.
  Guest import carries log chunks; sign-in hydrates them from Firestore.
- **Guest mode:** the app fully works signed-out on localStorage. On first sign-in,
  offer a one-time "import this progress into your account?" — never merge silently.
- **Multi-user:** open to any Google account. All state private per user.
  localStorage cache namespaced `srs:{uid}:` / `srs:anon:`.
- **Persistence:** Firebase Firestore, localStorage as write-through cache
  (reviews never block on network; Firestore offline persistence enabled).
  Data model: `users/{uid}/settings` (enabled decks, daily cap) and
  `users/{uid}/cards/{cardId}` (FSRS state) — docs created lazily on first review.

## Data sources

| Deck | Source | Status |
|---|---|---|
| sentences (~1314) | `pages/sentences_data.js` (`window.SENTENCE_DATA`) | ready to load |
| colloquial (200) | `pages/colloquial_data.js` (`window.COLLOQUIAL_DATA`) | ready to load |
| words (~787) | inline `const WORDS` in `pages/words.html` | needs extraction |
| kana (~100) | inline `const KANA` in `pages/kana.html` | needs extraction |
| radicals | inline `const RADICALS` in `pages/radicals.html` | needs extraction |
| numbers | inline in `pages/numbers.html` | needs extraction |
| usage | inline `const ITEMS` in `pages/usage.html` | needs extraction |
| word mini lessons | `pages/word_lessons_data.js` (`window.WORD_LESSONS`) | hand-written |

Word mini lessons (added 2026-07-11): hand-written usage notes for function
words the colloquial-200 lookup can't reach — multi-character conjunctions
(还是/或者, 可是/但是, 要是, 因为/所以) and single characters outside the 200
(才, 再, 别, 跟, 让). Same `{tone, lesson}` shape as colloquial entries, keyed
by the word's `w`; the registry throws on keys that match no word, and on word
cards a WORD_LESSONS entry takes precedence over the colloquial-character one.

Extraction = move each inline array into `pages/<name>_data.js` setting a
`window.*_DATA` global, and load it from the old page via a script tag
(same pattern sentences/colloquial already use). Behavior of old pages must be
identical. Owner approved touching old files for this extraction only.

## Firebase (fully provisioned — nothing left to click)

- Project **ghostface-srs**; web config committed at `pages/srs/firebase-config.js`
  (`window.FIREBASE_CONFIG`; safe to be public, access control is in rules).
- Firestore `(default)` in `eur3`; per-UID rules deployed from `/firestore.rules`
  (`firebase.json` + `.firebaserc` at repo root; redeploy needs the owner's laptop,
  where firebase-tools is logged in). Tighten rules with doc-shape validation when
  app code lands.
- Google sign-in provider enabled; authorized domains include
  `ghostfacekillah.github.io` and `localhost`.

## Build order (each phase = working site + own PR)

1. **Data extraction + card registry** — `*_data.js` files, stable IDs, a manifest
   per deck (deck id, display name, card list, front/back render).
2. **SRS app** at `pages/srs/index.html` — FSRS scheduler, review UI, deck opt-in
   settings, daily queue, stats; localStorage-only (fully usable as guest product).
3. **Firebase layer** — Google sign-in, Firestore sync, guest-import flow,
   account widget, namespaced caches.

Site conventions: plain static HTML/JS (no build step), pages load shared data via
script tags, cutesy styling per trainer; keep the SRS app self-contained under
`pages/srs/`.
