# Offline review & state merge — design proposal

Written 2026-09-04. **Status: proposal, nothing here is decided yet.** DESIGN.md
stays the source of truth; fold the parts the owner confirms into it when they
ship. Everything below was derived from reading `index.html`, `cloud.js`,
`sync.harness.js` and DESIGN.md at commit `9680dfe` — line references are to
that revision.

## The ask

Connectivity on a train or a plane comes and goes. The user should be able to
open the app, review on the local copy of the state, and have that work merged
with the cloud copy once the network is back — without ever losing the fact
that some of today's reviews already happened (the grades, the "reviews today"
count, the new-card quota already spent, today's plot).

## Short answer

Most of the *mechanism* already exists: reviews write to localStorage
synchronously, per-card merges take the newer review, the log merges by union,
`days` is derived from the log, session start falls back to local state after
5 s. What is missing is (1) the app can't even start offline in the right
account, (2) two of the three cloud writes are whole-document overwrites that
depend on a "have I seen the cloud?" gate, which is exactly the gate an
intermittent connection keeps tripping, and (3) a few counting and multi-tab
details that quietly lose or double-count today's work.

The design therefore has one guiding rule: **every cloud write must be
commutative and idempotent, and every local store must be mergeable by a pure
function.** Once that holds, "offline" is no longer a mode with special rules —
it is just writes that haven't landed yet, and the order in which devices
reconnect stops mattering. The hydration gate (`cloudHydrated`) and the
"settings changed offline are lost" trade-off both disappear as consequences,
not as extra features.

## 1. What the state is, and what can go wrong with each piece

| Store | Local (namespace `srs:{uid}:`) | Cloud | Merge today | Risk today |
|---|---|---|---|---|
| Card states | `cards` — `{id → {stability, difficulty, due, lastReview, reps, lapses}}` | `users/{uid}/cards/{id}` one doc per card | per-card, newer `lastReview` wins, local-newer pushed back (`mergeCardStates`, 2788) | fine. Ties on the same card graded on two devices lose one grade's *effect* (not its log entry) — see §4.8 |
| Review log | `log:YYYY-MM` — `["ts,grade,prevIvl,cardId,ms", …]` | `users/{uid}/log/{YYYY-MM}` — `{entries: [...]}` | union of entries, pushed back when the cloud lacks some (2818–2828) | the push is a **whole-doc rewrite** (`pushLog`, cloud.js 92). A queued offline write that lands late erases entries another device added meanwhile; the victim only heals if it hydrates again |
| `days` | `days` — `{"YYYY-MM-DD" → {introduced, reviews}}` | never synced | recounted from the log, `max(local, derived)` per field (`rebuildDaysFromLog`, 1232) | `introduced` counts *entries* with `prevIvl < 0`, not *cards*: two devices meeting the same new card = 2 |
| Settings | `settings` — `{slot, slots:[g0,g1,g2], newPerDay}` | `users/{uid}/settings/app` | **replaced** by the fetch (2806–2814); pushed whole-doc, gated on `cloudHydrated` | changes made before a successful hydration are kept locally but never pushed, and the next hydration overwrites them (DESIGN.md's "known accepted trade-off"). A chip tap offline waits the full Firestore offline-detection delay with no feedback |
| Identity | none — `uid` comes only from `onAuthStateChanged` | Firebase Auth (persisted in IndexedDB by the SDK) | — | if the SDK can't be imported (CDN unreachable, not in the HTTP cache) the app becomes a **guest**: grades land in `srs:anon:` and are never offered for import (import is only offered when the cloud is *empty*, 2937) |
| Firestore write queue | IndexedDB, single-tab lease | — | — | a second tab falls back to a memory cache (cloud.js 55–59): its queued writes die with the tab |
| App shell | browser HTTP cache only | GitHub Pages | — | with no service worker a cold start with no network is a browser error page. GitHub Pages sends `max-age=600`; past that, browsers do not reliably serve stale |

## 2. What actually happens on a plane today — three walkthroughs

**A. Tab was open and hydrated before boarding, network drops, user taps Start.**
`maybeRefreshCloud` → `fetchAll` → Firestore hasn't yet noticed it is offline,
so the read waits for the backend; `startAfterSync` gives up waiting after
5 s and opens the session on local state ✓. Grades go to localStorage,
`pushCard` is queued by Firestore's persistence, log pushes are debounced and
also queued ✓. Firestore eventually resolves the read *from its cache*
(`metadata.fromCache = true`, which `fetchAll` drops); `adoptCloudState` merges
it — harmless for cards and the log, and settings come back with the tab's
own pending writes, so nothing visible breaks. **Mostly works.** Chip taps,
however, `await refreshKeepingSlot()` with no timeout: 10 s or so of nothing
happening, then the change applies.

**B. Cold start on the plane, the SDK modules happen to be in the HTTP cache.**
Page loads (if *it* is in the cache). SDK imports resolve from cache,
`onAuthStateChanged` fires with the persisted user without a network ✓.
Firestore is offline: `fetchAll` in `handleUser` returns the cache after the
detection delay, `cloudHydrated` becomes true from a **cache-served snapshot**,
and the rest is case A. Works, but the gate is now open on a possibly-stale
base, and if this device is one the user hasn't opened for a week, its cached
settings doc is a week old and replaces whatever local had.

**C. Cold start on the plane, the SDK is not cached (or the page is not).**
Either the page doesn't load at all, or `cloud.js` fails its import →
`srs-cloud-unavailable` → identity settles as **guest** → Start opens in
`srs:anon:`. Every grade of the flight lands in the guest namespace. On
landing, `handleUser(u1)` hydrates the account; the cloud isn't empty, so no
import is offered; the flight's reviews are stranded forever (until someone
digs them out of localStorage). **This is the case the ask is really about,
and it is the one that loses work.**

**D. (bonus) Two devices offline on the same day — phone on the plane, laptop
at the hotel before either syncs.** Both build the daily queue from the same
last-synced state, so both present the same due cards. Each device's grades
are safe locally; on sync, per-card LWW keeps the later grade, the log keeps
both entries, `days.reviews` counts both, `days.introduced` double-counts any
new card both devices met. Whichever device syncs *second* rewrites the month
log doc with its own union — fine if it hydrated first (it did: hydration
precedes the push in `adoptCloudState`)… unless its write was queued *before*
it hydrated, which is exactly what `flushLogPush` on `pagehide` does when the
tab was hydrated earlier that day: the queued whole-doc write carries the
pre-merge chunk and lands after the other device's, dropping the other
device's entries. The other device heals the cloud on its next hydration; a
device that is never opened again does not.

## 3. Principles

1. **Local-first.** The device's own namespace is authoritative for what the
   device did. The cloud is a peer to merge with, never a master to replace
   from. A grade is durable in localStorage (synchronous) before anything
   async is attempted.
2. **Every cloud write is commutative and idempotent.** Per-card set (newest
   wins), per-month log *union*, per-field settings (newest wins). Then no
   write can clobber, no push needs a gate, and replay order after a
   reconnect is irrelevant.
3. **Merge is a pure function with tests.** `merge.js`:
   `mergeState(local, remote) → {merged, pushBack}`, no I/O, tested in node
   next to `fsrs.test.js`. `adoptCloudState` becomes "call merge, write the
   result, push `pushBack`".
4. **"Today's work" is derived, never stored.** Reviews today, new cards
   introduced, the plot, the streak: all functions of the merged log and card
   states. If the log survives, today's work survives. So the log is the
   thing to protect hardest, and it is a grow-only set — the easiest kind of
   thing to protect.
5. **Never block a review on the network; never *silently* wait on it
   either.** Every action either applies on local state immediately or shows
   what it is waiting for, with a bounded wait.

## 4. The design

### 4.1 Remembered identity (fixes walkthrough C)

- On every `onAuthStateChanged` with a user, write `srs:lastUser` =
  `{uid, displayName, email}` (un-namespaced key, like `theme`). On sign-out,
  delete it.
- At boot, *before* `cloud.js` has done anything: if `FIREBASE_CONFIG` and
  `srs:lastUser` exist, set `uid` from it, `loadState()` in that namespace,
  show the account widget from the remembered name, and **settle the identity
  gate immediately**. The 15 s timer and the `srs-cloud-unavailable` path no
  longer demote this device to guest: they switch the widget to
  "☁️ offline — syncing later" and leave `cloud = null`.
- When auth does resolve: same uid → nothing changes (no `loadState()`, no
  session yank, no clearing of dirty months — see the trap below). Different
  uid or `null` → today's namespace switch (rare: revoked token, signed out
  elsewhere), which keeps the existing yank behavior.
- Devices with no `srs:lastUser` (first visit, signed out, or first load
  after this ships) keep today's gate exactly as it is, including the
  guest-import offer. So the 2026-09-03 identity fix is preserved for the
  case it was written for.

Trap to avoid: `handleUser` decides "is this a change?" by comparing with
`handledUid`, which is `undefined` until the first auth answer (2903–2906).
With a remembered uid the first answer must instead compare with the live
`uid`; otherwise the first `onAuthStateChanged` for the *same* user would
`show("home")` on a running session, re-read state from localStorage (losing
any in-memory state whose `store.write` failed), and clear `dirtyLogMonths`.

Why this is safe: Firebase Auth restores the persisted user from IndexedDB
without a network, so the SDK's answer and the remembered uid agree whenever
both are available. The only way they disagree is a real sign-out, which we
handle by clearing the key at the same moment.

### 4.2 App shell service worker (fixes "the page doesn't load at all")

- `pages/srs/sw.js`, registered from `index.html`, scope `/pages/srs/`. A
  service worker intercepts every request made by the pages it controls,
  including `../words_data.js` outside its scope path, so one worker covers
  the data files.
- Precache on install, versioned by a `VERSION` constant bumped by hand (no
  build step on this site): `index.html`, `registry.js`, `fsrs.js`,
  `cloud.js`, `firebase-config.js`, the eight `../*_data.js` files, and the
  three Firebase SDK modules from gstatic (they are served with CORS, so the
  cached responses are real, not opaque). No audio or images: the SRS page
  uses `speechSynthesis`, not files.
- Strategy: **network-first with a 3 s timeout, cache fallback** for the
  site's own files (fresh code when online, instant when not);
  **cache-first** for the versioned SDK URLs (immutable). `activate` drops
  old caches. Firestore/Auth traffic passes through untouched.
- A harness check (or a 20-line node script) asserts every `<script src>` in
  `index.html` is in the precache list, so a new data file can't be forgotten.
- Update UX: because the shell is network-first, a deploy is picked up on the
  next online load; no "new version available" toast needed.

GitHub Pages is HTTPS, so the secure-context requirement is met. `localhost`
works for the harness.

### 4.3 Write path: three durable stores, three merges

**Cards — unchanged.** Per-card newest-`lastReview` wins; local-newer cards
are pushed back. `pushCard` is already a per-doc set, so it is already
commutative in the sense that matters (the last review of a card is always
the intended winner). One addition for determinism: tie-break equal
`lastReview` by `reps`, then `stability`, so two devices always pick the same
side.

**Log — from whole-doc rewrite to a grow-only set.**
- Cloud write becomes an append:
  `setDoc(logDoc(month), {entries: arrayUnion(...newEntries)}, {merge: true})`.
  `arrayUnion` is a server-side set union by value, so it is idempotent (an
  entry sent twice is stored once), commutative (devices' queued writes land
  in any order to the same result), and creates the doc if missing (`merge`).
  Entries are exact strings, which is the same identity `mergeLogChunks`
  already uses, so local and cloud dedupe agree.
- Locally, keep `log:pending` — the set of entries not yet acknowledged by
  the server. `appendLog` adds to it; the (still debounced) flush sends every
  pending entry for its month and removes them **when the write promise
  resolves** (Firestore resolves it only once the server has accepted the
  write, so on a plane the entries stay pending across reloads — the
  double insurance costs nothing thanks to idempotence). On hydration, any
  pending entry already present in the cloud chunk is dropped from pending;
  local entries the cloud lacks are pushed with the same `arrayUnion` instead
  of today's whole-union `pushLog`.
- Hydration keeps the local union (`mergeLogChunks`), unchanged.
- Ceiling: Firestore docs cap at 1 MiB ≈ 20 000 entries a month at ~50 bytes,
  i.e. ~650 reviews a day. Not a concern now; if it ever is, chunk by
  `YYYY-MM-a/b` — the merge is per chunk, so nothing else changes.
- Retire `pushLog` (whole doc) entirely once this lands, including the guest
  import path (`importAll` batches can use `arrayUnion` too).

**`days` — still derived, counted correctly.**
- `reviews` per day = number of log entries that day (keep `max` with the
  local value: pre-log history lives only in local `days`).
- `introduced` per day = number of **distinct card ids** with a first-sight
  entry (`prevIvl < 0`) that day. For a day that has any log entries, the
  derived value replaces the local one rather than maxing with it, so an
  inflated count from the old entry-based derivation heals itself. Pre-log
  days keep their local value.
- Consequence for the quota: after two offline devices each meet different
  new cards, `introduced` can exceed `newPerDay`. That is the truth of what
  happened and the quota is a cap on intake, not an invariant; the next day
  is clean. Meeting the *same* new card on both devices no longer counts
  twice.

**Settings — per-field last-writer-wins, with the change stamped where it
happened.**
- Shape gains timestamps: `{slot, slotAt, slots:[…], slotsAt:[t0,t1,t2],
  newPerDay, newPerDayAt}`. Every mutation stamps its own field with
  `Date.now()` (chip/all/none → `slotsAt[slot]`; slot switch → `slotAt`;
  cap → `newPerDayAt`). `normalizeSettings` fills missing stamps with 0, so
  legacy docs and the guest import merge as "older than anything".
- Merge: per field, the newer stamp wins; ties keep local. Fields where local
  is newer go into `pushBack`.
- Cloud write touches only the fields that changed:
  `setDoc(settingsDoc(), doc, {mergeFields: ["slots", "slotsAt"]})` etc.
  `mergeFields` replaces the named fields whole — no deep merge of the groups
  map, so a chip turned off stays off (no `false` keys, which matters for
  `Object.keys(settings.slots[i]).length` at 1652). Since a slot is one element
  of an array field and Firestore can't address array elements, changing any
  slot writes the whole `slots` array — acceptable: the stamps travel with it
  and the reader-side merge repairs a stale array on the next hydration on
  any device. (If that ever proves too coarse, store slots as three map
  fields `s0,s1,s2` with `s0At…` and let `mergeFields` isolate them; the
  merge function doesn't care which.)
- With this, the `cloudHydrated` gate has nothing left to protect and goes
  away; `cloudPushSettings` pushes whenever a cloud exists, offline writes are
  queued by Firestore, and in the no-SDK case they are pushed by the next
  hydration's `pushBack`. The "settings changed offline are lost" trade-off is
  closed.

### 4.4 Read path

- `fetchAll` returns `fromCache` (OR of the three snapshots' `metadata.fromCache`).
  A cache-served snapshot is still merged — with the merges above that is
  always safe — but it doesn't update `lastHydrate`, so the next action
  re-tries the network instead of trusting a 60 s throttle set by a cache
  read. The status line shows it as "offline".
- `adoptCloudState` = `mergeState` + write merged stores + push `pushBack` +
  `rebuildDaysFromLog` + `refreshHome`, exactly as now minus the gate.
- New trigger: `window.addEventListener("online", …)` → flush pending log,
  then `maybeRefreshCloud()` (throttle bypassed for this trigger). On a train
  this is the moment the phone's work arrives on the laptop. `visibilitychange`
  and `focus` stay.
- `persistentLocalCache({tabManager: persistentMultipleTabManager()})` in
  cloud.js so a second tab shares the persistent queue instead of a memory
  cache that dies with it.

### 4.5 Session start and settings actions

- `navigator.onLine === false` short-circuits every wait: the fetch is still
  *started* (Firestore will serve its cache and the merge is free) but nothing
  waits for it. `onLine` is only trustworthy when false, which is the only way
  it is used.
- Session start keeps the 5 s bounded wait when `onLine` is true or unknown.
- Settings actions get the *same* bounded wait (today they have none, 1662,
  1675, 1745, 1765) — safe now, because a settings write no longer clobbers.
  The pending-chip intent logic (1734–1760) stays.
- Status line under the account widget, from real counters:
  "☁️ synced" · "☁️ syncing…" · "☁️ offline · 12 reviews waiting". The count
  is `log:pending` size — the honest number of grades the server hasn't
  confirmed — plus, in the no-SDK case, "since the last sync".

### 4.6 The merge function

```
mergeState(local, remote) → { merged, pushBack }
  cards:
    for id in local ∪ remote:
      pick = newer lastReview, tie → higher reps, tie → higher stability, tie → local
      if pick is local and (remote missing or differs) → pushBack.cards.push(id)
  log:
    for month in local ∪ remote:
      merged[month] = union(local[month], remote[month]) sorted by ts
      pushBack.logAppend[month] = local[month] \ remote[month]      // arrayUnion these
      merged.pending = local.pending \ remote entries               // acked by presence
  settings:
    for field in {slot, slots, newPerDay}:
      take the side with the newer <field>At; tie → local
      if local won and remote differs → pushBack.settingsFields.push(field, fieldAt)
    groups = slots[slot]     // re-tie the live reference (normalizeSettings)
  days: not merged — derived afterwards from merged.log with the §4.3 rules
```

Properties worth a test each: idempotent (`merge(merge(l,r), r) = merge(l,r)`),
commutative up to `pushBack` (both devices converge after each pushes its
`pushBack`), and "no grade lost" (every log entry on either side is in the
result; every card on either side is in the result with a `lastReview` ≥ both
inputs').

### 4.7 What "don't drop today's reviews" means precisely, and why it holds

After any interleaving of offline and online sessions on any devices, once
every device has hydrated once *after* its last write:

- **Grades:** every log entry ever written is in every device's log. (Log
  entries are only ever unioned; the cloud only ever grows by `arrayUnion`;
  a device's own entries stay in its `log:pending` until the server confirms
  them.)
- **Card states:** every card carries the newest review from any device.
  (Per-card newest-wins on merge, local-newer pushed back, and `pushCard` is
  queued durably by Firestore while the SDK is present; without the SDK, the
  next hydration's `pushBack` carries it.)
- **Reviews today / streak / heatmap / field guide:** derived from the log,
  so they follow the first point.
- **New-card quota:** `introduced` = distinct cards met today across devices,
  so the quota already spent on the plane is spent everywhere as soon as the
  log has arrived — not before, which brings us to the next item.
- **Between the reconnect and the merge landing**, a device may build a
  queue from stale state (walkthrough D). The design accepts that a *queue*
  can be stale and instead makes it self-correct: when a merge lands during
  a daily session, the not-yet-shown remainder of the queue is filtered to
  cards still due (`s.due ≤ endOfToday`) and new cards still inside the
  updated quota; the card on screen is never swapped out. `sessionTotal` is
  adjusted so the progress bar doesn't jump backwards.

### 4.8 The one thing LWW can't do, and why we don't do it

If the *same* card is graded on two devices while both are offline, the
merged state reflects only the later grade; the earlier grade survives in the
log (so retention, time and the plot are right) but its effect on stability is
gone. The exact fix would be a three-way merge: replay both devices' entries
for that card from the common ancestor state through `FSRS.review` (it is
pure and takes a timestamp, so this is possible in principle). Rejected for
now because: it needs the ancestor *state* per card (a second copy of
`cards` in localStorage, or reconstructing it from the log, which is
incomplete for anything reviewed before 2026-07-11); the outcome differs by
one review's worth of stability on a card that was, by construction, just
reviewed twice in a day; and any future FSRS parameter change would make the
replay diverge from stored states. Revisit only if the two-device-offline
pattern turns out to be common. The queue self-correction in §4.7 is what
actually reduces the double work.

Clock skew: `lastReview` and log timestamps are the grading device's clock
today already; the design adds no new dependence on it beyond the settings
stamps, where a wrong clock costs at worst one chip flip.

Time zones: `todayKey` buckets by the viewing device's zone, as today. Two
devices in different zones can disagree about which day a review belongs to
around midnight; the log timestamps are absolute so nothing is lost, only
binned differently. Not addressed here.

## 5. Scenario table

| Scenario | Today | With this design |
|---|---|---|
| Cold start, no network, page & SDK cached | works after ~10 s stalls; gate opens on a cache snapshot | instant; status says offline; merges when the network is back |
| Cold start, no network, SDK not cached | **reviews land in the guest namespace, lost** | reviews land in the remembered account's namespace, pushed on the next hydration |
| Cold start, no network, page not cached | browser error page | loads from the service worker |
| Network drops mid-session | works; late fetch merged | same, plus the queue trims cards another device already did |
| Chip toggled offline | 10 s stall, then kept locally, **overwritten** by the next hydration | applies instantly, wins on merge by timestamp, pushed when online |
| Phone and laptop both offline the same day | both sets of grades survive; log may lose one side in the cloud until the victim re-hydrates; same new card counted twice | both survive everywhere; log is a pure union; distinct-card intake count; the second device's queue trims once the merge lands |
| Second tab open | memory cache; queued writes die with the tab | shared persistent queue |
| Tab closed with unflushed log entries, SDK present | Firestore's queue *usually* has them; otherwise healed on next hydration | same, plus `log:pending` survives in localStorage and is re-sent regardless |
| Tab closed with unflushed entries, no SDK | healed on next hydration (local union pushed) | same, via `pushBack` |

## 6. Rollout — four independently shippable steps

Each step leaves the site working and is its own PR, in this order (each
earlier step is also the bigger win for the effort):

1. **Identity + reconnect basics.** `srs:lastUser`; boot in the remembered
   namespace; `handleUser` compares against the live `uid`; `online`
   listener; `navigator.onLine` short-circuit; `fromCache` surfaced and kept
   out of `lastHydrate`; status line. No data-shape changes. Fixes
   walkthrough C outright.
2. **Log as a grow-only set + correct `days`.** `arrayUnion` appends,
   `log:pending`, distinct-card `introduced`, multi-tab manager, queue
   self-correction on a mid-session merge. `mergeState` extracted into
   `merge.js` with node tests for the log and cards halves.
3. **Settings per-field LWW.** Timestamps, `mergeFields` pushes, bounded
   waits on settings actions, retire `cloudHydrated` and `pushLog`.
4. **Service worker.** Precache list + manifest check in the harness.

Verification to add to `sync.harness.js` (Playwright, fake cloud): a page
booted with `srs:lastUser` and no cloud answer grades into the right
namespace; `context.setOffline(true)` mid-session keeps grading and shows the
offline status; an `online` event triggers flush + refresh; a fetch that
returns a stale settings doc no longer flips a chip back; a mid-session
merge that puts a queued card to bed removes it from the remainder without
touching the card on screen. Against the Firestore **emulator** (not the fake):
two browser profiles both offline, each grades overlapping and disjoint
cards, both come back in each order — assert the cloud log doc is the exact
union and both profiles converge to identical `cards`, `log`, `days`.

Things to confirm in the emulator before relying on them, because they are
SDK behavior rather than our code: `getDocs` resolves from the persistent
cache when offline and flags `fromCache`; a queued `arrayUnion` write
survives a page reload and replays on reconnect; `mergeFields` leaves the
unnamed fields untouched.

## 7. Out of scope / open questions for the owner

- The guest (`srs:anon:`) path gets none of this beyond the service worker;
  guests have no cloud to merge with.
- Firestore rules stay the per-uid wildcard; `arrayUnion` and `mergeFields`
  are ordinary writes under it. The "tighten rules with doc-shape
  validation" note in DESIGN.md would need to allow the new stamp fields.
- Should the status line be visible at all when everything is synced, or
  only in the offline/pending states? (Proposal: only when there is something
  to say.)
- The three-way per-card replay (§4.8): keep rejected, or prototype it in
  `merge.js` behind the tests to see how often it changes anything?
