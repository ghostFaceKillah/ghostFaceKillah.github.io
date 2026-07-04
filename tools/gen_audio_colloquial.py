#!/usr/bin/env python3
"""
Generate MP3 audio files for the Colloquial Character Trainer.

Requires:  pip install edge-tts
Output:    audio/colloquial/<character>.mp3       (one file per character)
           audio/colloquial/sentence_<rank>.mp3    (one file per example sentence)
Voice:     zh-CN-XiaoxiaoNeural   (Microsoft neural TTS, handles tone sandhi)

Note: edge-tts needs a raw WebSocket connection to Microsoft's speech
service, which most sandboxed/CI environments block. Run this locally
(where you have normal internet access), then commit the generated MP3s.

Run from the repo root:
    python3 tools/gen_audio_colloquial.py
"""

import asyncio
import json
import os
import sys

try:
    import edge_tts
except ImportError:
    sys.exit("edge-tts not installed. Run: pip install edge-tts")

DATA_JS = os.path.join(os.path.dirname(__file__), "..", "pages", "colloquial_data.js")
OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "..", "audio", "colloquial")
VOICE = "zh-CN-XiaoxiaoNeural"


def load_data(path: str) -> list[dict]:
    with open(path, encoding="utf-8") as f:
        text = f.read()
    json_text = text.split("window.COLLOQUIAL_DATA = ", 1)[1].rstrip("\n").rstrip(";")
    return json.loads(json_text)


async def gen_one(text: str, path: str) -> None:
    communicate = edge_tts.Communicate(text, VOICE)
    await communicate.save(path)


async def main() -> None:
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    data = load_data(DATA_JS)
    print(f"Loaded {len(data)} entries. Generating audio into {OUTPUT_DIR}/")

    done = 0
    skipped = 0

    # Unique characters (many are single hanzi, safe to dedupe by text).
    seen_chars = set()
    for entry in data:
        char = entry["c"]
        if char in seen_chars:
            continue
        seen_chars.add(char)
        dest = os.path.join(OUTPUT_DIR, char + ".mp3")
        if os.path.exists(dest) and os.path.getsize(dest) > 0:
            skipped += 1
            continue
        try:
            await gen_one(char, dest)
            done += 1
            print(f"  [char {done:>3}] {char}")
        except Exception as e:
            print(f"  ERROR {char}: {e}", file=sys.stderr)

    # One file per example sentence, keyed by rank (sentences aren't unique).
    for entry in data:
        rank = entry["rank"]
        dest = os.path.join(OUTPUT_DIR, f"sentence_{rank}.mp3")
        if os.path.exists(dest) and os.path.getsize(dest) > 0:
            skipped += 1
            continue
        try:
            await gen_one(entry["ex"], dest)
            done += 1
            print(f"  [sentence {done:>3}] #{rank} {entry['ex']}")
        except Exception as e:
            print(f"  ERROR sentence #{rank}: {e}", file=sys.stderr)

    print(f"\nDone. Generated: {done}  Skipped (already existed): {skipped}")


if __name__ == "__main__":
    asyncio.run(main())
