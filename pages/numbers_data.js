// Shared number/time → hanzi conversion for numbers.html and the SRS app
// (pages/srs/). Extracted verbatim from numbers.html.
(function () {
  // ===== Chinese number / time generation =====================================

  const PINYIN = {
    "零":"líng","一":"yī","二":"èr","两":"liǎng","三":"sān","四":"sì","五":"wǔ",
    "六":"liù","七":"qī","八":"bā","九":"jiǔ","十":"shí","百":"bǎi","千":"qiān",
    "万":"wàn","亿":"yì","点":"diǎn","分":"fēn","半":"bàn","刻":"kè","钟":"zhōng"
  };
  const DIGIT = ["零","一","二","三","四","五","六","七","八","九"];

  function pinyinOf(hanzi) {
    return hanzi.split("").map(c => PINYIN[c] || "").filter(Boolean).join(" ");
  }

  // Convert 0..9999 to characters. `bigUnit` is the 万/亿 suffix that will follow
  // this group (so a lone "2" before 万 reads 两, not 二).
  function under10000(n, lead) {
    if (n === 0) return "";
    const d = [Math.floor(n / 1000) % 10, Math.floor(n / 100) % 10, Math.floor(n / 10) % 10, n % 10];
    const units = ["千", "百", "十", ""];
    let out = "";
    let started = false;
    let zeroPending = false;
    for (let i = 0; i < 4; i++) {
      const v = d[i];
      if (v === 0) {
        if (started) zeroPending = true;
        continue;
      }
      if (zeroPending) { out += "零"; zeroPending = false; }
      // Bare 十 for 10–19, but only at the very front of the whole number
      // (e.g. 一万零一十, not 一万零十).
      if (i === 2 && v === 1 && !started && lead) {
        out += "十";
      } else if (v === 2 && (i === 0 || i === 1)) {
        // 2 before 百 or 千 reads 两.
        out += "两" + units[i];
      } else {
        out += DIGIT[v] + units[i];
      }
      started = true;
    }
    return out;
  }

  // Convert 0..99,999,999 to characters (covers numbers up to a million easily).
  function numToHanzi(n) {
    if (n === 0) return "零";
    const groups = [];
    let t = n, idx = 0;
    while (t > 0) {
      groups.unshift({ val: t % 10000, unit: idx });
      t = Math.floor(t / 10000);
      idx++;
    }
    const bigUnit = ["", "万", "亿"];
    let out = "";
    for (let i = 0; i < groups.length; i++) {
      const g = groups[i];
      if (g.val === 0) continue;
      // Insert 零 for an internal gap (group below 1000 that isn't the lead group).
      if (out !== "" && g.val < 1000) out += "零";
      // A lone "2" multiplying 万/亿 reads 两 (两万, 两亿).
      const body = (g.val === 2 && g.unit >= 1) ? "两" : under10000(g.val, out === "");
      out += body + bigUnit[g.unit];
    }
    return out;
  }

  function numToChinese(n) {
    const hanzi = numToHanzi(n);
    return { hanzi, pinyin: pinyinOf(hanzi) };
  }

  // Time, hours 1–12, minutes 0–59. For :15/:45 the everyday 十五分/四十五分
  // reading is primary; textbook 刻 is kept only as an alt — people rarely
  // say 刻 in real life.
  function timeToChinese(h, m) {
    const hourHanzi = (h === 2 ? "两" : numToHanzi(h)) + "点";
    let hanzi, alt = "", altNote = "";
    if (m === 0) {
      hanzi = hourHanzi;
      alt = hourHanzi + "钟";
    } else if (m === 15) {
      hanzi = hourHanzi + "十五分";
      alt = hourHanzi + "一刻";
      altNote = " (textbook — 刻 is rarely said)";
    } else if (m === 30) {
      hanzi = hourHanzi + "半";
      alt = hourHanzi + "三十分";
    } else if (m === 45) {
      hanzi = hourHanzi + "四十五分";
      alt = hourHanzi + "三刻";
      altNote = " (textbook — 刻 is rarely said)";
    } else {
      // After 点 the minutes need at least two syllables: 2:05 is 两点零五分,
      // never 两点五分.
      const minHanzi = (m < 10 ? "零" + DIGIT[m] : numToHanzi(m)) + "分";
      hanzi = hourHanzi + minHanzi;
    }
    return {
      hanzi,
      pinyin: pinyinOf(hanzi),
      alt: alt ? alt + "  ·  " + pinyinOf(alt) + altNote : ""
    };
  }

  window.NUMBERS_DATA = {
    PINYIN, DIGIT, pinyinOf, under10000, numToHanzi, numToChinese, timeToChinese,
  };
})();
