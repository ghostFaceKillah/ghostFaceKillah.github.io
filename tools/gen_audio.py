#!/usr/bin/env python3
"""
Generate MP3 audio files for all Chinese words in the Word Trainer.

Requires:  pip install edge-tts
Output:    audio/words/<word>.mp3  (one file per unique Chinese word)
Voice:     zh-CN-XiaoxiaoNeural   (Microsoft neural TTS, handles tone sandhi)

Run from the repo root:
    python3 tools/gen_audio.py
"""

import asyncio
import os
import re
import sys

try:
    import edge_tts
except ImportError:
    sys.exit("edge-tts not installed. Run: pip install edge-tts")

WORDS_HTML = os.path.join(os.path.dirname(__file__), "..", "pages", "words.html")
OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "..", "audio", "words")
VOICE = "zh-CN-XiaoxiaoNeural"


def extract_words(html_path: str) -> list[str]:
    with open(html_path, encoding="utf-8") as f:
        text = f.read()
    # Match  { w: "…"  entries
    words = re.findall(r'\{ w:\s*"([^"]+)"', text)
    seen: set[str] = set()
    unique = []
    for w in words:
        if w not in seen:
            seen.add(w)
            unique.append(w)
    return unique


async def gen_one(word: str, path: str) -> None:
    communicate = edge_tts.Communicate(word, VOICE)
    await communicate.save(path)


async def main() -> None:
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    words = extract_words(WORDS_HTML)
    print(f"Found {len(words)} unique words. Generating audio into {OUTPUT_DIR}/")

    done = 0
    skipped = 0
    for word in words:
        dest = os.path.join(OUTPUT_DIR, word + ".mp3")
        if os.path.exists(dest) and os.path.getsize(dest) > 0:
            skipped += 1
            continue
        try:
            await gen_one(word, dest)
            done += 1
            print(f"  [{done:>3}] {word}")
        except Exception as e:
            print(f"  ERROR {word}: {e}", file=sys.stderr)

    print(f"\nDone. Generated: {done}  Skipped (already existed): {skipped}")


if __name__ == "__main__":
    asyncio.run(main())
