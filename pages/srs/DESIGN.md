# SRS App — Design & Handoff Notes

Spaced repetition system covering all cards from the language trainers in `pages/`.
Decided 2026-07-11. This doc is the source of truth for sessions continuing the work.

## Decisions (all confirmed with the owner)

- **Standalone app** at `pages/srs/` — existing trainer pages stay untouched in look
  and behavior; no review buttons or header changes on them. The SRS app is the
  daily-review experience; trainers remain the browse/study experience.
- **Scheduler:** FSRS-6 (grades Again / Hard / Good / Easy; upgraded from
  FSRS-4.5 on 2026-07-11 — the 4.5 same-day-review blind spot made all passing
  grades collapse to "1d" on cards relearned within a session). Pure function
  `(cardState, grade, now) → newState`, no I/O, testable in isolation.
  Reviews < 1 day after the previous one go through FSRS-6's short-term
  memory formulas, so grades differentiate stability within a session; the
  1-day interval floor still shows "1d" on all passing grades while a card's
  stability is under ~1.5 days — same as Anki, working as intended.
  Card state: `{stability, difficulty, due, lastReview, reps, lapses}` —
  unchanged from 4.5, so stored states carry over with no migration.
- **Card IDs:** stable, derived from existing data. Examples:
  `words/L1D1/你`, `sent/hsk1/42`, `coll/1`, `kana/あ`. ~2,500 cards total.
- **Card intake:** opt-in by deck/lesson (IC1 lessons individually, topic word
  packs — Food & Drink, Common Verbs, Numbers words, tutor notes — as their own
  deck in sets of 10 (split out of the words deck 2026-07-12; card IDs keep the
  `words/` prefix so history carries over, saved opt-ins migrate via
  `migrateGroupKeys`), colloquial chars in bands of 20, IC1 sentence chapters,
  HSK recall sets of 50, kana rows split per script (hiragana and katakana
  selectable separately), radicals, numbers in magnitude bands (0–10 … 10 000+,
  was one 56-card group) plus clock times, usage). New cards introduced at a
  per-user daily cap (default ~15).
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
  review heatmap + streak (18 Monday-first weeks ≈ 4 months, GitHub-style), a card-maturity
  "garden" (🌱 <7d, 🌿 7–21d, 🌸 ≥21d — Anki's 21-day "mature" idiom), and a
  28-day due forecast for enabled decks. Styling (owner iterated twice on
  2026-07-11: single-hue ramps felt monochrome → kana rainbow → final ask:
  chart colors must be EXACTLY the four grade-button hues, and heatmap days
  should be cute emoji, not dots — a different one per day): all chart vars
  alias the button colors (`--gs/--gg/--gb` = red/amber/green for 🌱/🌿/🌸 —
  traffic-light, maturing toward "Good"; `--w1..--w4` = red/amber/green/sky
  in Again→Easy order; `--ret` = green, "Good" = remembered). Aliases resolve
  at use-time, so dark mode follows the buttons' dark values with no
  per-theme chart overrides. Heatmap: it IS a garden (owner iterated from
  hearts → 136-emoji jar → garden, 2026-07-11): a practiced day grows a plant
  / garden visitor (48-emoji set) whose font-size carries the review count
  (s1–s4); a skipped day shows bare ground (🪨🪵🍂🌰, small + faded), window
  trimmed to 18 weeks ≈ 4 months. Picks are a deterministic hash of the
  calendar day (`scatter()` — wild-looking but stable across visits; different
  salt for ground); the month-hue cycle (`--m1..--m5`) is gone. Magnitude never rides
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
- **Cross-device sync hardening** (decided 2026-07-12, after the owner saw
  slot picks vanish and "reviews today" read 0 on a second computer):
  - Root causes, both reproduced in a two-browser-profile harness against the
    Firebase emulators: (1) `days` (streak / heatmap / "reviews today" / the
    new-card intake counter) was device-local — never synced; (2) settings and
    month-log docs are whole-doc pushes, so one click on a stale tab (the
    other computer's tab, open since yesterday) overwrote newer cloud state —
    classic last-write-wins clobber.
  - `days` stays un-synced as a doc; it is **derived**: on every hydration,
    recount from the review log and take max(local, derived) per day+field
    (local may hold pre-log history; for any logged day the true count is
    never below the log's). New-card quota then holds across devices too.
  - Log hydration **merges** chunks (union of entries, time-ordered) instead
    of replacing, and pushes the union back when the cloud copy is missing
    entries — a clobbered month self-heals on the victim's next hydration.
  - Whole-doc pushes are **gated** on `cloudHydrated` (set by the first
    successful hydration per sign-in; guest mode unaffected). A tab that
    hasn't seen the cloud yet cannot overwrite it. Per-card pushes stay
    ungated — they're fine-grained and a fresh review is always the winner.
  - **Rebase before acting**: every settings mutation (chips, select all/none,
    slot switch, daily cap) and every session start first runs
    `maybeRefreshCloud()` — single-flight, 60 s-throttled fetchAll+adopt,
    errors keep local state so offline still works — then applies the user's
    change on top. Chip/cap/session paths go through `refreshKeepingSlot()`:
    the adopt may bring another device's active slot, but the action must
    land in the slot the user was looking at.
  - Tabs also re-hydrate on `visibilitychange`/window `focus` (skipped
    mid-review and while the import modal is open), so a tab you return to
    is fresh before you can click anything. `buildDeckList()` preserves
    which decks are expanded across these rebuilds.
  - Known accepted trade-off: settings changed while truly offline are kept
    locally but not queued for push (the gate); they can be overwritten by
    the next successful hydration. Reviews (cards + log + days) survive
    offline fine via Firestore's queued per-card writes and the log merge.
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
