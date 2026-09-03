// Browser harness for the SRS app's sync/state paths (see DESIGN.md,
// "State-loss hardening"). cloud.js is stubbed out and a FAKE window.SRS_CLOUD
// is injected, so auth timing, slow/stale fetches and push traffic are all
// under the test's control — no Firebase project or emulator needed.
//
// Run (needs node, playwright with chromium, and any static server at the
// repo root):
//   npx http-server . -p 8765 -s &
//   node pages/srs/sync.harness.js 8765 new
// `old` instead of `new` asserts the pre-fix behavior — handy when checking
// out a commit from before 2026-09-03 to see the bugs reproduce.
"use strict";
const { chromium } = require("playwright");

const PORT = process.argv[2] || "8765";
const MODE = process.argv[3] || "new";
const URL = `http://127.0.0.1:${PORT}/pages/srs/index.html`;
const T0 = new Date("2026-09-03T10:00:00");

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

async function newPage(browser, seed, { stubCloud = true } = {}) {
  const ctx = await browser.newContext();
  const page = await ctx.newPage();
  page.on("pageerror", e => console.log("   PAGE ERROR:", e.message));
  page.on("console", m => { if (m.type() === "error") console.log("   console.error:", m.text()); });
  if (stubCloud) await page.route("**/pages/srs/cloud.js", r => r.fulfill({ contentType: "text/javascript", body: "// stubbed by harness" }));
  await page.route("**/gstatic.com/**", r => r.abort());
  await page.clock.install({ time: T0 });
  await page.addInitScript(seed => { for (const [k, v] of Object.entries(seed)) localStorage.setItem(k, JSON.stringify(v)); }, seed);
  await page.goto(URL);
  await page.waitForFunction(() => window.SRS_REGISTRY && document.getElementById("statDue"));
  return { ctx, page };
}

const active = page => page.evaluate(() => document.querySelector(".screen.active").id);
const stat = (page, id) => page.evaluate(id => document.getElementById(id).textContent, id);
const ls = (page, key) => page.evaluate(k => JSON.parse(localStorage.getItem(k) || "null"), key);
const startBtn = page => page.evaluate(() => {
  const b = document.getElementById("startBtn");
  // the "syncing" status is a CSS overlay (class), so the caption underneath keeps updating
  return { text: b.textContent, disabled: b.disabled, syncing: b.classList.contains("syncing") };
});
const settle = page => page.evaluate(() => new Promise(r => setTimeout(r, 0)));

async function gradeGood(page, n) {
  for (let i = 0; i < n; i++) {
    await page.click("#card");
    await page.waitForSelector("#grades.show");
    await page.click(".g-good");
  }
}

const PICKS = { groups: { "rad/all": true }, newPerDay: 15 }; // 40 radicals, none seen → 15 new today

(async () => {
  const browser = await chromium.launch();
  console.log(`\n=== ${MODE.toUpperCase()} code on port ${PORT} ===`);

  // ---- 1. Signed-in user taps Start before auth has resolved (the "reviews vanished" report) ----
  console.log("\n[1] Start tapped while auth is still resolving");
  {
    const { ctx, page } = await newPage(browser, {
      "srs:anon:settings": PICKS,               // stale guest picks on the device
      "srs:u1:settings": PICKS, "srs:u1:importOffered": true,
    });
    const b0 = await startBtn(page);
    console.log(`   start button before identity: "${b0.text}" disabled=${b0.disabled}`);
    if (MODE === "new") {
      check("Start is gated until identity settles", b0.disabled && /connecting/.test(b0.text));
    } else {
      check("(old) Start is live in the guest namespace", !b0.disabled && b0.text === "Start review");
      await page.click("#startBtn");
      await page.waitForSelector("#review.active");
      await gradeGood(page, 3);
      check("(old) 3 grades landed in srs:anon:cards", Object.keys((await ls(page, "srs:anon:cards")) || {}).length === 3);
    }
    // the SDK finishes loading and auth answers: user u1, cloud has nothing yet
    await page.evaluate(FAKE_CLOUD);
    await page.evaluate(() => { window.__fetchResult = { cards: {}, settings: { groups: { "rad/all": true }, newPerDay: 15 }, log: {} }; });
    await page.evaluate(() => window.__setUser({ uid: "u1", displayName: "Tester" }));
    await page.waitForFunction(() => window.__fetchCalls >= 1);
    await settle(page);
    const b1 = await startBtn(page);
    console.log(`   after auth: screen=${await active(page)} start="${b1.text}" new today=${await stat(page, "statNew")} pushes=${(await page.evaluate(() => window.__pushes.cards.length))}`);
    if (MODE === "old") {
      check("(old) user yanked to home", (await active(page)) === "home");
      check("(old) account shows everything still TODO", (await stat(page, "statNew")) === "15");
      check("(old) nothing pushed — the 3 reviews are stranded as a guest", (await page.evaluate(() => window.__pushes.cards.length)) === 0);
    } else {
      check("Start opens once identity is known", !b1.disabled && b1.text === "Start review");
      await page.click("#startBtn");
      await page.waitForSelector("#review.active");
      await gradeGood(page, 3);
      await page.click("#quitBtn");
      check("after quitting, home shows 12 new (3 done)", (await stat(page, "statNew")) === "12", await stat(page, "statNew"));
      check("3 grades saved under the account namespace", Object.keys((await ls(page, "srs:u1:cards")) || {}).length === 3);
      check("guest namespace untouched", (await ls(page, "srs:anon:cards")) === null);
      check("3 card pushes reached the cloud", (await page.evaluate(() => window.__pushes.cards.length)) === 3);
      check("log pushed once on quit (debounced), not once per grade", (await page.evaluate(() => window.__pushes.logs.length)) === 1, await page.evaluate(() => JSON.stringify(window.__pushes.logs)));
    }
    await ctx.close();
  }

  // ---- 2. Cloud snapshot older than local state adopted on a focus refresh ----
  console.log("\n[2] Stale cloud snapshot on the next refresh");
  {
    const { ctx, page } = await newPage(browser, { "srs:u1:settings": PICKS, "srs:u1:importOffered": true });
    await page.evaluate(FAKE_CLOUD);
    await page.evaluate(() => { window.__fetchResult = { cards: {}, settings: { groups: { "rad/all": true }, newPerDay: 15 }, log: {} }; });
    await page.evaluate(() => window.__setUser({ uid: "u1", displayName: "Tester" }));
    await page.waitForFunction(() => window.__fetchCalls >= 1);
    await settle(page);
    await page.click("#startBtn");
    await page.waitForSelector("#review.active");
    await gradeGood(page, 3);
    await page.click("#quitBtn");
    check("home shows 12 new right after quitting", (await stat(page, "statNew")) === "12");
    check("3 cards learning right after quitting", (await stat(page, "statSeen")) === "3");
    const pushedBefore = await page.evaluate(() => window.__pushes.cards.length);
    // the cloud still answers with NO cards (writes not landed / second tab / old cache);
    // a minute passes and the tab regains focus → refresh + adopt
    await page.clock.fastForward(61000);
    await page.evaluate(() => window.dispatchEvent(new Event("focus")));
    await page.waitForFunction(() => window.__fetchCalls >= 2);
    await settle(page);
    // "new today" is quota-derived from the log and can't show a state wipe;
    // "cards learning" counts card states directly
    const after = await stat(page, "statSeen");
    const pushedAfter = await page.evaluate(() => window.__pushes.cards.length);
    console.log(`   cards learning after stale adopt: ${after}; card pushes ${pushedBefore} → ${pushedAfter}`);
    if (MODE === "old") {
      check("(old) the 3 reviews are undone locally — 0 cards learning", after === "0", after);
    } else {
      check("local-newer reviews survive the stale snapshot", after === "3", after);
      check("they are re-pushed to heal the cloud", pushedAfter === pushedBefore + 3, `${pushedBefore}→${pushedAfter}`);
      // and a cloud that is genuinely NEWER still wins per card
      const cards = await ls(page, "srs:u1:cards");
      const id = Object.keys(cards)[0];
      const newer = Object.assign({}, cards[id], { lastReview: cards[id].lastReview + 1000, due: cards[id].due + 5 * 86400000, reps: 9 });
      await page.evaluate(([id, s]) => { window.__fetchResult.cards[id] = s; }, [id, newer]);
      await page.clock.fastForward(61000);
      await page.evaluate(() => window.dispatchEvent(new Event("focus")));
      await page.waitForFunction(() => window.__fetchCalls >= 3);
      await settle(page);
      const got = (await ls(page, "srs:u1:cards"))[id];
      check("a newer cloud state for a card is adopted", got.reps === 9, JSON.stringify(got));
    }
    await ctx.close();
  }

  // ---- 3. Slow fetch on Start: feedback, no double start, timeout falls back to local ----
  console.log("\n[3] Slow cloud fetch when starting a session");
  {
    const { ctx, page } = await newPage(browser, { "srs:u1:settings": PICKS, "srs:u1:importOffered": true });
    await page.evaluate(FAKE_CLOUD);
    await page.evaluate(() => { window.__fetchResult = { cards: {}, settings: { groups: { "rad/all": true }, newPerDay: 15 }, log: {} }; });
    await page.evaluate(() => window.__setUser({ uid: "u1", displayName: "Tester" }));
    await page.waitForFunction(() => window.__fetchCalls >= 1);
    await settle(page);
    await page.clock.fastForward(61000);                 // stale again → Start will fetch
    await page.evaluate(() => { window.__fetchDelay = 3000; });
    await page.click("#startBtn");
    await settle(page);
    const during = await startBtn(page);
    console.log(`   during fetch: "${during.text}" syncing=${during.syncing} disabled=${during.disabled} screen=${await active(page)}`);
    if (MODE === "new") check("button shows it is syncing and is disabled", during.disabled && during.syncing);
    else check("(old) button looks idle — no feedback while the fetch runs", !during.disabled && during.text === "Start review");
    await page.click("#startBtn", { force: true });     // the impatient second tap
    await page.click("#startBtn", { force: true });
    await page.clock.fastForward(3100);
    await page.waitForSelector("#review.active");
    check("session opened after the fetch", (await active(page)) === "review");
    check("only one fetch ran for all those taps", (await page.evaluate(() => window.__fetchCalls)) === 2, await page.evaluate(() => window.__fetchCalls));
    await page.click("#quitBtn");

    if (MODE === "new") {
      // a fetch that never comes back within the timeout: the session opens on local state
      await page.clock.fastForward(61000);
      await page.evaluate(() => { window.__fetchDelay = 20000; });
      await page.click("#startBtn");
      await page.clock.fastForward(5200);
      await page.waitForSelector("#review.active", { timeout: 3000 });
      check("past the sync timeout the session opens on local state", (await active(page)) === "review");
      await gradeGood(page, 2);
      // the late fetch lands mid-review with a stale snapshot: no yank, no revert
      await page.clock.fastForward(16000);
      await settle(page);
      check("late fetch does not interrupt the review", (await active(page)) === "review");
      await page.click("#quitBtn");
      check("grades made before the late fetch survive", (await stat(page, "statSeen")) === "2", await stat(page, "statSeen"));
    }
    await ctx.close();
  }

  // ---- 4. Guest mode: SDK unavailable, and the last-resort timer ----
  console.log("\n[4] Guest paths");
  {
    const { ctx, page } = await newPage(browser, { "srs:anon:settings": PICKS });
    if (MODE === "new") {
      check("guest Start gated until the SDK answers", (await startBtn(page)).disabled);
      await page.evaluate(() => window.dispatchEvent(new CustomEvent("srs-cloud-unavailable")));
      await settle(page);
    }
    check("Start live once the cloud is known to be unavailable", !(await startBtn(page)).disabled);
    await page.click("#startBtn");
    await page.waitForSelector("#review.active");
    await gradeGood(page, 4);
    await page.click("#quitBtn");
    check("guest reviews count on the home screen", (await stat(page, "statNew")) === "11");
    await page.reload();
    await page.waitForFunction(() => document.getElementById("statNew").textContent !== "0");
    check("guest reviews persist across reload", (await stat(page, "statNew")) === "11", await stat(page, "statNew"));
    await ctx.close();

    if (MODE === "new") {
      const { ctx: c2, page: p2 } = await newPage(browser, { "srs:anon:settings": PICKS });
      check("no answer from the SDK: still gated at 14 s", (await startBtn(p2)).disabled);
      await p2.clock.fastForward(15500);
      await settle(p2);
      check("…and opened up as a guest by the 15 s fallback", !(await startBtn(p2)).disabled);
      await c2.close();
    }
  }

  // ---- 5. Real cloud.js failing to load (no stub) → unavailable event settles identity ----
  if (MODE === "new") {
    console.log("\n[5] Real cloud.js with the Firebase CDN blocked");
    const { ctx, page } = await newPage(browser, { "srs:anon:settings": PICKS }, { stubCloud: false });
    await page.waitForFunction(() => !document.getElementById("startBtn").disabled, null, { timeout: 5000 }).catch(() => {});
    check("Start opens as a guest when the SDK import fails", !(await startBtn(page)).disabled, JSON.stringify(await startBtn(page)));
    await ctx.close();
  }

  // ---- 6. Chip double-click while a rebase is in flight ----
  if (MODE === "new") {
    console.log("\n[6] Chip toggled twice while its rebase is in flight");
    const { ctx, page } = await newPage(browser, { "srs:u1:settings": PICKS, "srs:u1:importOffered": true });
    await page.evaluate(FAKE_CLOUD);
    await page.evaluate(() => { window.__fetchResult = { cards: {}, settings: { groups: { "rad/all": true }, newPerDay: 15 }, log: {} }; });
    await page.evaluate(() => window.__setUser({ uid: "u1", displayName: "Tester" }));
    await page.waitForFunction(() => window.__fetchCalls >= 1);
    await settle(page);
    await page.clock.fastForward(61000);
    await page.evaluate(() => { window.__fetchDelay = 2000; });
    await page.click(".deck-head:has-text('Kana')");
    const chip = page.locator('.chip[data-key="kana/a-hira"]');
    await chip.click(); // on
    await chip.click(); // off again, before the rebase returns
    await page.clock.fastForward(2500);
    await settle(page);
    const on = await page.evaluate(() => !!JSON.parse(localStorage.getItem("srs:u1:settings")).groups["kana/a-hira"]);
    check("two quick taps net out to 'off'", on === false, `on=${on}`);
    check("chip drawn to match", !(await chip.evaluate(el => el.classList.contains("sel"))));
    await ctx.close();
  }

  await browser.close();
  console.log(failures ? `\n${failures} FAILURE(S)` : "\nAll harness checks passed.");
  process.exit(failures ? 1 : 0);
})().catch(e => { console.error(e); process.exit(2); });
