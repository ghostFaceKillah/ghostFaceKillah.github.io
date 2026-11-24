// Browser harness for the garden soil under the stats heatmap and the
// bloom record behind its gold (see DESIGN.md, "Garden soil"). cloud.js is
// stubbed out, the Firebase CDN is blocked and the clock is fake; signed-in
// scenarios inject a FAKE window.SRS_CLOUD like sync.harness.js, extended
// with pushBlooms and a recording importAll.
//
// Run (needs node, playwright with chromium, and any static server at the
// repo root):
//   npx http-server . -p 8765 -s &
//   node pages/srs/garden.harness.js 8765
"use strict";
const { chromium } = require("playwright");

const PORT = process.argv[2] || "8765";
const URL = `http://127.0.0.1:${PORT}/pages/srs/index.html`;
const T0 = new Date("2026-11-24T10:00:00");          // local time, a Tuesday
const DAY = 86400000;
const TZ = Intl.DateTimeFormat().resolvedOptions().timeZone;

let failures = 0;
function check(name, cond, detail) {
  console.log((cond ? "  ok  " : "FAIL  ") + name + (detail !== undefined ? `  [${detail}]` : ""));
  if (!cond) failures++;
}

const FAKE_CLOUD = `
  window.__pushes = { cards: [], settings: [], logs: [], blooms: [] };
  window.__fetchResult = { cards: {}, settings: null, log: {}, blooms: [] };
  window.__fetchCalls = 0;
  window.__imported = null;
  let user = undefined;
  const listeners = [];
  window.SRS_CLOUD = {
    get user() { return user; },
    onUserChange(cb) { listeners.push(cb); if (user !== undefined) cb(user); },
    async signIn() {}, async signOut() {},
    pushCard(id, s) { window.__pushes.cards.push([id, s]); },
    pushSettings(s) { window.__pushes.settings.push(JSON.parse(JSON.stringify(s))); },
    pushLog(m, e) { window.__pushes.logs.push([m, e.length]); },
    pushBlooms(days) { window.__pushes.blooms.push(days.slice()); },
    async fetchAll() {
      window.__fetchCalls++;
      return JSON.parse(JSON.stringify(window.__fetchResult));
    },
    async importAll(cards, settings, log, blooms) {
      window.__imported = { cards: Object.keys(cards).length, blooms: blooms || null };
    },
  };
  window.__setUser = u => { user = u; listeners.forEach(cb => cb(u)); };
  window.dispatchEvent(new CustomEvent("srs-cloud-ready"));
`;

async function newPage(browser, seed, { reducedMotion = "no-preference" } = {}) {
  const ctx = await browser.newContext({ timezoneId: TZ, reducedMotion });
  const page = await ctx.newPage();
  const errors = [];
  page.on("pageerror", e => { errors.push(e.message); console.log("   PAGE ERROR:", e.message); });
  page.on("console", m => { if (m.type() === "error") console.log("   console.error:", m.text()); });
  await page.route("**/pages/srs/cloud.js", r => r.fulfill({ contentType: "text/javascript", body: "// stubbed by harness" }));
  await page.route("**/gstatic.com/**", r => r.abort());
  await page.clock.install({ time: T0 });
  await page.addInitScript(seed => {
    if (localStorage.getItem("harness:seeded")) return;
    for (const [k, v] of Object.entries(seed)) localStorage.setItem(k, JSON.stringify(v));
    localStorage.setItem("harness:seeded", "1");
  }, seed || {});
  await page.goto(URL);
  await page.waitForFunction(() => window.SRS_REGISTRY && document.getElementById("statDue"));
  return { ctx, page, errors };
}

const text = (page, sel) => page.evaluate(sel => { const el = document.querySelector(sel); return el ? el.textContent : null; }, sel);
const ls = (page, key) => page.evaluate(k => JSON.parse(localStorage.getItem(k) || "null"), key);
const settle = page => page.evaluate(() => new Promise(r => setTimeout(r, 0)));
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
async function openStats(page) {
  await page.click("#statsBtn");
  await page.waitForSelector("#statsScreen.active");
}
// Reviews every card the session serves with Good until it ends.
async function workGood(page) {
  let grades = 0;
  while (await page.evaluate(() => document.getElementById("review").classList.contains("active")) && grades < 100) {
    await page.clock.runFor(3000);
    await page.keyboard.press("Space");
    await page.keyboard.press("2");
    grades++;
  }
  return grades;
}
// The heatmap cell for the day `ago` days before the (fake) today, with the
// clod as the browser computed it.
const cell = (page, ago) => page.evaluate(ago => {
  const d = new Date(Date.now()); d.setHours(0, 0, 0, 0); d.setDate(d.getDate() - ago);
  const el = [...document.querySelectorAll("#heatGrid i")]
    .find(i => (i.dataset.tip || "").startsWith(fmtDay(d) + " —"));
  if (!el) return null;
  const b = getComputedStyle(el, "::before"), a = getComputedStyle(el, "::after");
  return {
    cls: el.className, tip: el.dataset.tip, style: el.getAttribute("style"), plant: el.textContent,
    bg: b.backgroundImage, layers: (b.backgroundImage.match(/gradient\(/g) || []).length,
    shadow: b.boxShadow, radius: b.borderTopLeftRadius, glint: a.animationName, opacity: getComputedStyle(el.firstChild).opacity,
  };
}, ago);

// ----- Fixture --------------------------------------------------------------------
// One picked group, 3 of its cards seen and overdue, no new cards a day — so
// today's plot is those 3 cards and grading them Good puts it in full bloom.
// A few past days of history for the heatmap, some of them bloomed.
const keyOf = ago => { const d = new Date(+T0 - ago * DAY); return d.getFullYear() + "-" + String(d.getMonth() + 1).padStart(2, "0") + "-" + String(d.getDate()).padStart(2, "0"); };
const monthOf = ts => { const d = new Date(ts); return d.getFullYear() + "-" + String(d.getMonth() + 1).padStart(2, "0"); };
function fixture(ids, ns, { blooms = [] } = {}) {
  const t = +T0;
  const cards = {};
  const seed = {
    [ns + "cards"]: cards,
    [ns + "settings"]: { groups: { "recall/hsk1-1-50": true }, newPerDay: 0, minutesPerDay: 0 },
    [ns + "days"]: {
      [keyOf(1)]: { introduced: 0, reviews: 40 },
      [keyOf(2)]: { introduced: 0, reviews: 12 },
      // 3 days ago: a rest day
      [keyOf(4)]: { introduced: 0, reviews: 80 },
      [keyOf(5)]: { introduced: 0, reviews: 5 },
    },
    [ns + "blooms"]: blooms,
  };
  ids.slice(0, 3).forEach((id, i) => {
    const due = t - (1 + i) * DAY;
    const lastReview = due - 5 * DAY;
    cards[id] = { stability: 5, difficulty: 5, due, lastReview, reps: 3, lapses: 0 };
    const k = ns + "log:" + monthOf(lastReview);
    (seed[k] = seed[k] || []).push(`${lastReview},3,3,${id},20000`);
  });
  for (const k of Object.keys(seed)) if (k.includes(":log:")) seed[k].sort((a, b) => +a.split(",")[0] - +b.split(",")[0]);
  return seed;
}

(async () => {
  const browser = await chromium.launch();
  console.log(`\n=== garden soil harness on port ${PORT} (tz ${TZ}) ===`);

  let ids;
  {
    const { ctx, page } = await newPage(browser, {});
    ids = await page.evaluate(() =>
      window.SRS_REGISTRY.deckById.recall.groups.find(g => g.id === "hsk1-1-50").cards.map(c => c.id));
    await ctx.close();
  }

  // ---- A. The soil under each day, both themes ----------------------------------
  console.log("\n[A] Heatmap soil (guest, seeded history)");
  {
    const { ctx, page, errors } = await newPage(browser, fixture(ids, "srs:anon:", { blooms: [keyOf(1), keyOf(4)] }));
    await asGuest(page);

    // the scatter refactor must not move a single plant
    const moved = await page.evaluate(() => {
      const orig = (arr, n, salt) => {
        let h = Math.imul(n ^ salt, 2654435761);
        h = Math.imul(h ^ (h >>> 16), 2246822507);
        h = Math.imul(h ^ (h >>> 13), 3266489909);
        return arr[((h ^ (h >>> 16)) >>> 0) % arr.length];
      };
      let bad = 0;
      for (let n = 19000; n < 23000; n++) if (plantOfDay(n) !== orig(GARDEN, n, 0) || groundOfDay(n) !== orig(GROUND, n, 7)) bad++;
      return bad;
    });
    check("every day still grows the same plant and ground (4000 days)", moved === 0, moved);

    await openStats(page);
    const bloomed = await cell(page, 1), grown = await cell(page, 2), rest = await cell(page, 3), big = await cell(page, 4);
    check("a bloomed day sits on gold", /\bclod\b/.test(bloomed.cls) && /\bmet\b/.test(bloomed.cls), bloomed.cls);
    check("a practiced day sits on moss", /\bgrown\b/.test(grown.cls) && !/\bmet\b/.test(grown.cls), grown.cls);
    check("a rest day sits on bare earth", /\bclod bare\b/.test(rest.cls), rest.cls);
    check("the plant keeps its size class", /\bs4\b/.test(big.cls) && /\bs1\b|\bs2\b/.test(grown.cls), `${big.cls} / ${grown.cls}`);
    check("rest-day earth resolves: grains, crack layer, dome", rest.layers === 5 && /oklch|rgb|color\(/.test(rest.bg), rest.layers);
    check("practiced moss resolves: tufts + dome", grown.layers === 4 && /oklch|rgb|color\(/.test(grown.bg), grown.layers);
    check("gold resolves with its glow", bloomed.layers === 1 && bloomed.shadow !== "none", bloomed.shadow);
    check("clods are hand-shaped (hashed radius)", /%/.test(rest.radius) && rest.radius !== grown.radius, `${rest.radius} vs ${grown.radius}`);
    check("gold days glint", bloomed.glint === "glint", bloomed.glint);
    check("only the ground emoji is faded, not its clod", rest.opacity === "0.6", rest.opacity);
    check("tooltip says the plot bloomed", /· plot in bloom 🌻$/.test(bloomed.tip), bloomed.tip);
    check("…and only on bloomed days", !/bloom/.test(grown.tip) && !/bloom/.test(rest.tip), grown.tip);
    check("today unpracticed: bare", /\bclod bare\b/.test((await cell(page, 0)).cls), (await cell(page, 0)).cls);
    const hidden = await page.evaluate(() => [...document.querySelectorAll("#heatGrid i")]
      .filter(i => i.style.visibility === "hidden").map(i => i.className));
    check("future days stay hidden with no clod", hidden.length > 0 && hidden.every(c => c === ""), `${hidden.length} hidden`);

    const legend = await page.evaluate(() => ({
      kinds: [...document.querySelectorAll("#heatLegend i.clod")].map(i => i.classList[1]),
      groups: [...document.querySelectorAll("#heatLegend > span")].map(s => s.textContent.replace(/\s+/g, " ").trim()),
    }));
    check("legend: moss for four sizes, earth, gold", legend.kinds.join() === "grown,grown,grown,grown,bare,met", legend.kinds.join());
    check("legend text", legend.groups.join(" | ") === "fewer 🌱🌿🌷🌻 more | 🪨 rest day | 🌼 plot in bloom", legend.groups.join(" | "));

    // the theme switch recolors the same nodes — no re-render needed
    const mossLight = grown.bg;
    await page.evaluate(() => { document.querySelector("#heatGrid i.grown").__keep = 1; });
    await page.click("#themeToggle");
    const after = await cell(page, 2);
    const kept = await page.evaluate(() => document.querySelector("#heatGrid i.grown").__keep === 1);
    check("dark mode recolors the moss", after.bg !== mossLight && after.layers === 4, after.bg.slice(0, 60));
    check("…on the same DOM nodes", kept);
    const goldDark = (await cell(page, 1)).bg;
    check("…and the gold", goldDark !== bloomed.bg, goldDark.slice(0, 60));

    // a day's clod never reshuffles between visits
    const before = [await cell(page, 2), await cell(page, 3)].map(c => c.style);
    await page.reload();
    await page.waitForFunction(() => window.SRS_REGISTRY && document.getElementById("statDue"));
    await asGuest(page);
    await openStats(page);
    const again = [await cell(page, 2), await cell(page, 3)].map(c => c.style);
    check("clods are stable across reloads", before[0] === again[0] && before[1] === again[1]);

    // a browser that can't do math inside oklch() skips the guarded rules —
    // simulated by dropping the @supports block
    const flat = await page.evaluate(() => {
      let dropped = 0;
      for (const sheet of document.styleSheets) {
        const i = [...sheet.cssRules].findIndex(r => r instanceof CSSSupportsRule && /oklch/.test(r.conditionText));
        if (i >= 0) { sheet.deleteRule(i); dropped++; }
      }
      const b = getComputedStyle(document.querySelector("#heatGrid i.bare"), "::before");
      const g = getComputedStyle(document.querySelector("#heatGrid i.grown"), "::before");
      return { dropped, bare: [b.backgroundImage, b.backgroundColor], grown: [g.backgroundImage, g.backgroundColor] };
    });
    check("without math in oklch(): flat pastel clods instead of none",
      flat.dropped === 1 && flat.bare[0] === "none" && flat.grown[0] === "none" &&
      flat.bare[1] !== "rgba(0, 0, 0, 0)" && flat.grown[1] !== "rgba(0, 0, 0, 0)", JSON.stringify(flat));
    check("[A] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  console.log("\n[A2] Reduced motion");
  {
    const { ctx, page, errors } = await newPage(browser, fixture(ids, "srs:anon:", { blooms: [keyOf(1)] }), { reducedMotion: "reduce" });
    await asGuest(page);
    await openStats(page);
    const c = await cell(page, 1);
    check("no glint under prefers-reduced-motion", c.glint === "none", c.glint);
    check("[A2] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  // ---- B, F, C. Finishing the plot turns today gold, and it sticks ----------------
  console.log("\n[B] A session that puts the plot in full bloom (guest)");
  {
    const { ctx, page, errors } = await newPage(browser, fixture(ids, "srs:anon:"));
    await asGuest(page);
    check("no bloom yet", (await ls(page, "srs:anon:blooms")).length === 0);
    await page.click("#startBtn");
    await page.waitForSelector("#review.active");
    const n = await workGood(page);
    check("session ends after the 3 due cards", n === 3, n);
    check("done screen in full bloom", (await text(page, "#doneBig")) === "Today's plot is in full bloom", await text(page, "#doneBig"));
    check("today recorded as bloomed", (await ls(page, "srs:anon:blooms")).includes(keyOf(0)), JSON.stringify(await ls(page, "srs:anon:blooms")));
    await page.click("#homeBtn");
    await openStats(page);
    const today = await cell(page, 0);
    check("today's clod is gold", /\bmet\b/.test(today.cls), today.cls);
    check("today's tooltip", /^\w+ \d+ — 3 reviews · .*plot in bloom 🌻$/.test(today.tip), today.tip);
    await page.click("#statsBack");

    console.log("\n[F] Leaving the tab writes a bloom down");
    await page.evaluate(() => { blooms.clear(); saveBlooms(); });
    await page.evaluate(() => window.dispatchEvent(new Event("pagehide")));
    check("pagehide re-records today's bloom", (await ls(page, "srs:anon:blooms")).includes(keyOf(0)), JSON.stringify(await ls(page, "srs:anon:blooms")));

    console.log("\n[C] The bed grows after the bloom");
    await page.evaluate(() => { settings.newPerDay = 15; saveSettings(); refreshStats(); });
    const bed = await page.evaluate(() => document.getElementById("plotBed").className);
    check("the plot is no longer in bloom (15 new cards to plant)", !/\bbloom\b/.test(bed), bed);
    check("the day stays bloomed", (await ls(page, "srs:anon:blooms")).includes(keyOf(0)));
    await openStats(page);
    check("today's clod stays gold", /\bmet\b/.test((await cell(page, 0)).cls), (await cell(page, 0)).cls);
    check("[B/F/C] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  // ---- D. Signed in: union with the cloud, push-back, push on bloom ---------------
  console.log("\n[D] Signed in: blooms merge with the cloud and go up");
  {
    const seed = fixture(ids, "srs:u1:", { blooms: [keyOf(4)] });
    seed["srs:u1:importOffered"] = true;
    const { ctx, page, errors } = await newPage(browser, seed);
    await signIn(page, {
      cards: seed["srs:u1:cards"], settings: seed["srs:u1:settings"], log: {},
      blooms: [keyOf(1), keyOf(9)],
    });
    const local = await ls(page, "srs:u1:blooms");
    check("local = union of device and cloud", JSON.stringify(local) === JSON.stringify([keyOf(9), keyOf(4), keyOf(1)]), JSON.stringify(local));
    const pushed = await page.evaluate(() => window.__pushes.blooms);
    check("only the day the cloud lacked goes up", JSON.stringify(pushed) === JSON.stringify([[keyOf(4)]]), JSON.stringify(pushed));

    await page.click("#startBtn");
    await page.waitForSelector("#review.active");
    await workGood(page);
    const after = await page.evaluate(() => window.__pushes.blooms);
    check("blooming today pushes today", JSON.stringify(after[after.length - 1]) === JSON.stringify([keyOf(0)]), JSON.stringify(after));
    check("…once", after.filter(p => p.includes(keyOf(0))).length === 1, JSON.stringify(after));
    check("[D] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  console.log("\n[D2] The plot was finished on another device");
  {
    const seed = fixture(ids, "srs:u1:");
    seed["srs:u1:importOffered"] = true;
    const { ctx, page, errors } = await newPage(browser, seed);
    // the cloud holds today's three reviews (states and log), but its blooms
    // doc hasn't heard about today yet
    const t = +T0 - 3600000;
    const cards = {}, entries = [];
    ids.slice(0, 3).forEach((id, i) => {
      const s = seed["srs:u1:cards"][id];
      cards[id] = { stability: 12, difficulty: 5, due: t + 12 * DAY, lastReview: t + i * 1000, reps: 4, lapses: 0 };
      entries.push(`${t + i * 1000},3,${Math.round((s.due - s.lastReview) / DAY)},${id},9000`);
    });
    // (the log merges by union, so the cloud's month needs only today's lines)
    await signIn(page, { cards, settings: seed["srs:u1:settings"], log: { [monthOf(t)]: entries }, blooms: [] });
    check("the merged plot blooms here", (await page.evaluate(() => todaysPlot(Date.now()).met)) === true);
    check("…and this device records and pushes it", (await ls(page, "srs:u1:blooms")).includes(keyOf(0)) &&
      JSON.stringify(await page.evaluate(() => window.__pushes.blooms)).includes(keyOf(0)),
      JSON.stringify(await page.evaluate(() => window.__pushes.blooms)));
    check("[D2] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  console.log("\n[D3] A cloud answer with no blooms field (cloud.js from before this change)");
  {
    const seed = fixture(ids, "srs:u1:", { blooms: [keyOf(4)] });
    seed["srs:u1:importOffered"] = true;
    const { ctx, page, errors } = await newPage(browser, seed);
    await signIn(page, { cards: seed["srs:u1:cards"], settings: seed["srs:u1:settings"], log: {} });
    check("local blooms kept", JSON.stringify(await ls(page, "srs:u1:blooms")) === JSON.stringify([keyOf(4)]));
    check("[D3] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  // ---- E. A guest's blooms come along on the first sign-in import -----------------
  console.log("\n[E] Guest import carries the bloomed days");
  {
    const { ctx, page, errors } = await newPage(browser, fixture(ids, "srs:anon:", { blooms: [keyOf(2), keyOf(1)] }));
    await signIn(page, { cards: {}, settings: null, log: {}, blooms: [] });
    await page.waitForSelector("#importModal.show");
    await page.click("#importYes");
    await page.waitForFunction(() => window.__imported !== null);
    const mine = await ls(page, "srs:u1:blooms");
    check("the account's blooms are the guest's", JSON.stringify(mine) === JSON.stringify([keyOf(2), keyOf(1)]), JSON.stringify(mine));
    const imp = await page.evaluate(() => window.__imported);
    check("importAll uploads them", JSON.stringify(imp.blooms) === JSON.stringify([keyOf(2), keyOf(1)]), JSON.stringify(imp));
    check("[E] no page errors", errors.length === 0, errors.join(" | "));
    await ctx.close();
  }

  await browser.close();
  console.log(failures ? `\n${failures} FAILURE(S)` : "\nAll harness checks passed.");
  process.exit(failures ? 1 : 0);
})().catch(e => { console.error(e); process.exit(2); });
