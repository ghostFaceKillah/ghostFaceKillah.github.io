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
//   <script src="../word_lessons_data.js"></script> (window.WORD_LESSONS)
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
  const LESSONS = need("WORD_LESSONS");

  const esc = s => String(s == null ? "" : s)
    .replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  // A line of the card back: label + content. `html: true` marks fields that
  // intentionally contain markup (mnemonics use <b>…</b>).
  const line = (cls, content, html) =>
    content ? `<div class="srs-${cls}">${html ? content : esc(content)}</div>` : "";

  // Tone-change notes and "mini lesson" usage patterns live in two places:
  // on the colloquial entries (rendered on colloquial cards and, via this
  // index, on matching single-character Integrated Chinese word cards — 不,
  // 了, 的, …) and in WORD_LESSONS (word_lessons_data.js), hand-written for
  // function words the colloquial 200 can't cover — multi-character ones
  // like 还是/可是 and missing single characters like 才/再. On word cards a
  // WORD_LESSONS entry wins over the colloquial one.
  const collByChar = {};
  for (const c of COLL) collByChar[c.c] = c;

  // `stripToneRef` drops the tone note's trailing "— here …" clause, which
  // points at the colloquial example sentence — not shown on word cards.
  function miniLessonHtml(entry, stripToneRef) {
    if (!entry) return "";
    let tone = entry.tone;
    if (tone && stripToneRef) tone = tone.replace(/\s*— here .*$/u, ".");
    let out = line("tone", tone);
    const l = entry.lesson;
    if (l) {
      let body = "<small>Mini lesson</small>";
      if (l.intro) body += `<p>${esc(l.intro)}</p>`;
      for (const u of l.uses || []) {
        body += `<p>${u.h
          ? `<b>${esc(u.h)}</b>${u.hPy ? ` <span class="py">${esc(u.hPy)}</span>` : ""} — ${esc(u.g)}`
          : esc(u.g)}</p>`;
        if (u.ex) {
          body += `<p class="use-ex">${esc(u.ex)}${u.exPy ? " — " + esc(u.exPy) : ""}${u.exEn ? " — " + esc(u.exEn) : ""}</p>`;
        }
      }
      out += `<div class="srs-lesson">${body}</div>`;
    }
    return out;
  }

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
    for (const w of Object.keys(LESSONS)) {
      if (!WORDS.words.some(x => x.w === w))
        throw new Error("SRS registry: WORD_LESSONS entry for unknown word " + w);
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
        line("mnem", d.mnem, true) +
        miniLessonHtml(LESSONS[d.w] || collByChar[d.w], true),
    });
  }

  // ----- sent: Integrated Chinese sentences, split per chapter ---------------
  // Recognition direction: shown in Chinese, recall the English meaning.
  {
    const groups = [];
    for (const ch of SENT.ic1_chapters || []) {
      groups.push({
        id: `ic1-${ch.n}`, name: `IC1 Lesson ${ch.n} — ${ch.title}`,
        cards: (SENT.ic1 || []).filter(s => s.n >= ch.from && s.n <= ch.to)
          .map(s => ({ id: `sent/ic1/${s.n}`, data: s })),
      });
    }
    decks.push({
      id: "sent", name: "Sentences (Integrated Chinese)", emoji: "💬", page: "../sentences.html",
      groups,
      renderFront: d => `<div class="srs-zh">${esc(d.zh)}</div>`,
      renderBack: d =>
        line("py", d.py) +
        line("en", d.en) +
        (d.notes || []).map(n => line("tone", n)).join(""),
    });
  }

  // ----- recall: HSK sentences flipped, opt-in in sets of 50 -----------------
  // Production direction: shown in English, recall the Chinese sentence.
  // These used to live in the sent deck as two 500-card groups shown
  // Chinese-first. Fresh card IDs on purpose: the flipped direction is a
  // different exercise, so old recognition history must not schedule it.
  {
    const groups = [];
    for (const [key, label] of [["hsk1", "HSK 1"], ["hsk2", "HSK 2"]]) {
      const all = SENT[key] || [];
      for (let lo = 1; lo <= all.length; lo += 50) {
        const hi = Math.min(lo + 49, all.length);
        groups.push({
          id: `${key}-${lo}-${hi}`, name: `${label} · ${lo}–${hi}`,
          cards: all.filter(s => s.n >= lo && s.n <= hi)
            .map(s => ({ id: `recall/${key}/${s.n}`, data: s })),
        });
      }
    }
    decks.push({
      id: "recall", name: "Sentence Recall (HSK)", emoji: "🧠", page: "../sentences.html",
      groups,
      renderFront: d => `<div class="srs-en-front">${esc(d.en)}</div>`,
      renderBack: d =>
        line("zh", d.zh) +
        line("py", d.py) +
        (d.notes || []).map(n => line("tone", n)).join(""),
    });
  }

  // ----- coll: colloquial characters, opt-in in sets of 20 ------------------
  {
    const groups = [];
    for (let lo = 1; lo <= 181; lo += 20) {
      const hi = lo + 19;
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
        line("ex", d.ex ? `${esc(d.ex)} — ${esc(d.exPy)} — ${esc(d.exEn)}` : "", true) +
        miniLessonHtml(d),
    });
  }

  // ----- kana: one card per glyph, opt-in per row × script ------------------
  // Each row is two groups (`a-hira`, `a-kata`) so the scripts can be picked
  // up independently. Rows used to mix both scripts in one group (`a`) —
  // migrateGroupKeys in index.html rewrites those saved keys.
  {
    const kanaCard = (k, script) => ({
      id: `kana/${script === "kata" ? k.k : k.h}`,
      data: Object.assign({ script }, k),
    });
    const kanaGroups = script => KANA.rows.map(row => ({
      id: `${row.id}-${script}`,
      name: `${script === "kata" ? "Katakana" : "Hiragana"} ${(script === "kata" ? row.kata : row.hira)[0]} row${row.basic ? "" : " (voiced)"}`,
      cards: KANA.kana.filter(k => k.row === row.id).map(k => kanaCard(k, script)),
    }));
    decks.push({
      id: "kana", name: "Kana", emoji: "🌸", page: "../kana.html",
      groups: kanaGroups("hira").concat(kanaGroups("kata")),
      renderFront: d => `<div class="srs-kana srs-big">${esc(d.script === "kata" ? d.k : d.h)}</div>`,
      renderBack: d => {
        const kata = d.script === "kata";
        return line("py", d.r) +
          line("ex", `${esc(kata ? d.wk : d.wh)} — ${esc(d.wr)} — ${esc(d.en)} ${esc(d.e)}`, true) +
          line("mnem", kata ? d.mk : d.mh) +
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
