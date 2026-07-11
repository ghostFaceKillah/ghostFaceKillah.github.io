// SRS card registry — enumerates every reviewable card on the site with a
// stable ID, grouped into decks and opt-in groups (see DESIGN.md).
//
// Load order: the shared data files must be loaded first, e.g.
//   <script src="../words_data.js"></script>     (window.WORDS_DATA)
//   <script src="../sentences_data.js"></script> (window.SENTENCE_DATA)
//   <script src="../colloquial_data.js"></script>(window.COLLOQUIAL_DATA)
//   <script src="../kana_data.js"></script>      (window.KANA_DATA)
//   <script src="../radicals_data.js"></script>  (window.RADICALS_DATA)
//   <script src="../numbers_data.js"></script>   (window.NUMBERS_DATA)
//   <script src="../usage_data.js"></script>     (window.USAGE_DATA)
//   <script src="registry.js"></script>          (this file)
//
// Card IDs are stable and derived from the data itself — never from array
// position, except usage drills which have no natural key (there the ID is
// the 1-based position within the drill set). Never rename an ID: review
// history in Firestore/localStorage is keyed by it.
//
// Shapes:
//   deck  = { id, name, emoji, page, groups: [group…],
//             renderFront(data) → html, renderBack(data) → html }
//   group = { id, name, cards: [card…] }   // opt-in unit for intake
//   card  = { id, deckId, groupId, data }  // data = the original data object
window.SRS_REGISTRY = (function () {
  "use strict";

  function need(name) {
    const v = window[name];
    if (!v) throw new Error("SRS registry: missing " + name + " — load the *_data.js files before registry.js");
    return v;
  }
  const WORDS = need("WORDS_DATA");
  const SENT = need("SENTENCE_DATA");
  const COLL = need("COLLOQUIAL_DATA");
  const KANA = need("KANA_DATA");
  const RADICALS = need("RADICALS_DATA");
  const NUM = need("NUMBERS_DATA");
  const USAGE = need("USAGE_DATA");

  const esc = s => String(s == null ? "" : s)
    .replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  // A line of the card back: label + content. `html: true` marks fields that
  // intentionally contain markup (mnemonics use <b>…</b>).
  const line = (cls, content, html) =>
    content ? `<div class="srs-${cls}">${html ? content : esc(content)}</div>` : "";

  const decks = [];

  // ----- words: Integrated Chinese vocabulary, opt-in per lesson -----------
  {
    const groups = [];
    const byGroup = {};
    for (const l of WORDS.lessons) {
      const g = { id: "L" + l.num, name: `Lesson ${l.num} — ${l.theme}`, cards: [] };
      groups.push(g); byGroup[g.id] = g;
    }
    for (const t of WORDS.topics) {
      const g = { id: t.id, name: t.theme, cards: [] };
      groups.push(g); byGroup[g.id] = g;
    }
    for (const w of WORDS.words) {
      const groupId = w.lesson.replace(/D\d+$/, ""); // L1D2 → L1; FOOD → FOOD
      byGroup[groupId].cards.push({ id: `words/${w.lesson}/${w.w}`, data: w });
    }
    decks.push({
      id: "words", name: "Words (Integrated Chinese)", emoji: "📚", page: "../words.html",
      groups,
      renderFront: d => `<div class="srs-zh srs-big">${esc(d.w)}</div>`,
      renderBack: d =>
        line("py", d.pinyin) +
        line("pos", d.pos) +
        line("en", d.def) +
        line("parts", d.parts, true) +
        line("mnem", d.mnem, true),
    });
  }

  // ----- sent: sentence decks, opt-in per deck ------------------------------
  {
    const DECKS = [
      { id: "hsk1", name: "HSK 1 sentences" },
      { id: "hsk2", name: "HSK 2 sentences" },
      { id: "ic1", name: "Integrated Chinese 1 sentences" },
    ];
    decks.push({
      id: "sent", name: "Sentences", emoji: "💬", page: "../sentences.html",
      groups: DECKS.map(g => ({
        id: g.id, name: g.name,
        cards: (SENT[g.id] || []).map(s => ({ id: `sent/${g.id}/${s.n}`, data: s })),
      })),
      renderFront: d => `<div class="srs-zh">${esc(d.zh)}</div>`,
      renderBack: d => line("py", d.py) + line("en", d.en),
    });
  }

  // ----- coll: colloquial characters, opt-in in bands of 50 -----------------
  {
    const groups = [];
    for (let lo = 1; lo <= 151; lo += 50) {
      const hi = lo + 49;
      groups.push({
        id: `${lo}-${hi}`, name: `Characters ${lo}–${hi}`,
        cards: COLL.filter(c => c.rank >= lo && c.rank <= hi)
          .map(c => ({ id: `coll/${c.rank}`, data: c })),
      });
    }
    decks.push({
      id: "coll", name: "Colloquial Characters", emoji: "🗣️", page: "../colloquial.html",
      groups,
      renderFront: d => `<div class="srs-zh srs-big">${esc(d.c)}</div>`,
      renderBack: d =>
        line("py", d.py) +
        line("en", d.en) +
        line("parts", d.parts, true) +
        line("mnem", d.mnem, true) +
        line("ex", d.ex ? `${esc(d.ex)} — ${esc(d.exPy)} — ${esc(d.exEn)}` : "", true),
    });
  }

  // ----- kana: one card per glyph (hira + kata), opt-in per row -------------
  {
    const kanaCard = (k, script) => ({
      id: `kana/${script === "kata" ? k.k : k.h}`,
      data: Object.assign({ script }, k),
    });
    decks.push({
      id: "kana", name: "Kana", emoji: "🌸", page: "../kana.html",
      groups: KANA.rows.map(row => ({
        id: row.id,
        name: `${row.hira[0]} · ${row.kata[0]} row${row.basic ? "" : " (voiced)"}`,
        cards: KANA.kana.filter(k => k.row === row.id)
          .flatMap(k => [kanaCard(k, "hira"), kanaCard(k, "kata")]),
      })),
      renderFront: d => `<div class="srs-kana srs-big">${esc(d.script === "kata" ? d.k : d.h)}</div>`,
      renderBack: d => {
        const kata = d.script === "kata";
        return line("py", d.r) +
          line("ex", `${esc(kata ? d.wk : d.wh)} — ${esc(d.wr)} — ${esc(d.en)} ${esc(d.e)}`, true) +
          line("note", d.note) +
          (kata ? line("note", d.noteK) : "");
      },
    });
  }

  // ----- rad: the 40 radicals, one opt-in group -----------------------------
  decks.push({
    id: "rad", name: "Radicals", emoji: "🧩", page: "../radicals.html",
    groups: [{
      id: "all", name: "The 40 radicals",
      cards: RADICALS.map(r => ({ id: `rad/${r.base}`, data: r })),
    }],
    renderFront: d =>
      `<div class="srs-zh srs-big">${esc(d.base)}${d.variant ? `<span class="srs-variant"> · ${esc(d.variant)}</span>` : ""}</div>`,
    renderBack: d =>
      line("py", d.pinyin) +
      line("en", d.english) +
      line("ex", "e.g. " + d.examples),
  });

  // ----- num: numbers & clock times (generated, fixed curated list) ---------
  {
    // Fixed lists so the deck is deterministic and IDs stay stable.
    const NUMBERS = [
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
      21, 25, 30, 40, 50, 60, 70, 80, 90, 99,
      100, 101, 110, 115, 200, 250, 302, 555, 999,
      1000, 1001, 1100, 1500, 2000, 2222, 5000, 8888,
      10000, 10001, 10250, 20000, 55555,
      100000, 123456, 1000000,
    ];
    const TIMES = [
      [1, 0], [2, 0], [3, 15], [4, 45], [5, 30], [6, 5],
      [7, 50], [8, 10], [9, 15], [10, 30], [11, 45], [12, 0],
    ];
    decks.push({
      id: "num", name: "Numbers & Time", emoji: "🔢", page: "../numbers.html",
      groups: [
        {
          id: "numbers", name: "Numbers",
          cards: NUMBERS.map(n => ({
            id: `num/n/${n}`,
            data: Object.assign({ kind: "number", n }, NUM.numToChinese(n)),
          })),
        },
        {
          id: "time", name: "Clock times",
          cards: TIMES.map(([h, m]) => ({
            id: `num/t/${h}:${String(m).padStart(2, "0")}`,
            data: Object.assign({ kind: "time", h, m }, NUM.timeToChinese(h, m)),
          })),
        },
      ],
      renderFront: d => d.kind === "time"
        ? `<div class="srs-num srs-big">${d.h}:${String(d.m).padStart(2, "0")}</div>`
        : `<div class="srs-num srs-big">${d.n.toLocaleString("en-US")}</div>`,
      renderBack: d =>
        line("zh", d.hanzi) +
        line("py", d.pinyin) +
        line("alt", d.alt),
    });
  }

  // ----- usage: which-word drills, opt-in per drill set ----------------------
  {
    const setById = {};
    for (const s of USAGE.sets) setById[s.id] = s;
    const counters = {};
    const groups = USAGE.sets.map(s => ({ id: s.id, name: s.title, cards: [] }));
    const groupById = {};
    for (const g of groups) groupById[g.id] = g;
    for (const it of USAGE.items) {
      const n = (counters[it.set] = (counters[it.set] || 0) + 1);
      groupById[it.set].cards.push({ id: `usage/${it.set}/${n}`, data: it });
    }
    decks.push({
      id: "usage", name: "Usage Drills", emoji: "🎯", page: "../usage.html",
      groups,
      renderFront: d =>
        line("frame", d.frame) +
        (d.q ? `<div class="srs-zh">${esc(d.q)}</div>` : "") +
        `<ol class="srs-opts">${d.opts.map(o => `<li>${esc(o)}</li>`).join("")}</ol>`,
      renderBack: d =>
        line("zh", d.zh) +
        line("py", d.py) +
        line("en", d.en) +
        line("why", d.why) +
        line("rule", setById[d.set].rule),
    });
  }

  // ----- finalize: stamp deckId/groupId, index, sanity-check ----------------
  const cards = [];
  const byId = new Map();
  const deckById = {};
  for (const deck of decks) {
    deckById[deck.id] = deck;
    for (const group of deck.groups) {
      for (const card of group.cards) {
        card.deckId = deck.id;
        card.groupId = group.id;
        if (byId.has(card.id)) throw new Error("SRS registry: duplicate card id " + card.id);
        byId.set(card.id, card);
        cards.push(card);
      }
    }
  }

  return {
    decks,
    deckById,
    cards,
    cardById: id => byId.get(id),
    render: card => {
      const deck = deckById[card.deckId];
      return { front: deck.renderFront(card.data), back: deck.renderBack(card.data) };
    },
  };
})();
