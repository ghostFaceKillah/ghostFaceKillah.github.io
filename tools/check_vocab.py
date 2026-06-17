# -*- coding: utf-8 -*-
"""
Validate that every sentence uses only characters that appear in the
HSK 1 + HSK 2 (2.0) vocabulary. Flags out-of-scope characters and
duplicate sentences so they can be fixed.

Run: python3 tools/check_vocab.py
"""
import os
import sys
import types
from collections import Counter

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from hsk12_words import ALLOWED_CHARS as ALLOWED  # canonical HSK 1+2 chars

# Punctuation, digits and a tiny whitelist (proper names) that are fine.
PUNCT = set("，。？！：、；…—　 0123456789")
NAME_WHITELIST = set("李王月")  # common given/surnames used in examples
OK = ALLOWED | PUNCT | NAME_WHITELIST


def load_data():
    stub = types.ModuleType("pypinyin")
    stub.pinyin = lambda *a, **k: []

    class S:
        TONE = None
    stub.Style = S
    sys.modules["pypinyin"] = stub
    ns = {"__file__": os.path.join(HERE, "gen_sentences.py"), "__name__": "x"}
    exec(open(os.path.join(HERE, "gen_sentences.py"), encoding="utf-8").read(), ns)
    return ns["HSK1"], ns["HSK2"]


def check(name, pairs):
    problems = 0
    bad_chars = Counter()
    for zh, en in pairs:
        offenders = sorted({c for c in zh if c not in OK})
        if offenders:
            problems += 1
            for c in offenders:
                bad_chars[c] += 1
            print(f"[{name}] OUT-OF-SCOPE {''.join(offenders)}  ->  {zh}  ({en})")
    # duplicates
    c = Counter(zh for zh, en in pairs)
    for zh, cnt in c.items():
        if cnt > 1:
            problems += 1
            print(f"[{name}] DUPLICATE x{cnt}: {zh}")
    return problems, bad_chars


def main():
    h1, h2 = load_data()
    print(f"Counts: HSK1={len(h1)} HSK2={len(h2)}")
    p1, b1 = check("HSK1", h1)
    p2, b2 = check("HSK2", h2)
    total = p1 + p2
    allbad = b1 + b2
    print("-" * 50)
    if allbad:
        print("Out-of-scope characters by frequency:")
        for ch, n in allbad.most_common():
            print(f"  {ch}  x{n}")
    print(f"TOTAL PROBLEMS: {total}")
    sys.exit(1 if total else 0)


if __name__ == "__main__":
    main()
