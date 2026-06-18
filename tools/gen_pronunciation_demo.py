#!/usr/bin/env python3
"""Generate the Option B (pre-generated neural TTS) audio for the
pronunciation comparison demo page.

This renders the first 10 HSK 1 sentences to MP3 files that get committed
to the repo and played by pages/pronunciation_compare.html.

It uses sherpa-onnx with the MeloTTS zh_en model — a fully offline neural
voice — so it can run in restricted environments where the usual cloud TTS
endpoints (Google / Azure / Microsoft Edge) are not reachable. If you have
normal network access, edge-tts or Google Cloud TTS will sound even better;
swap the synth() call accordingly.

Setup:
    pip install sherpa-onnx soundfile lameenc
    curl -sSL -o melo.tar.bz2 \
      https://github.com/k2-fsa/sherpa-onnx/releases/download/tts-models/vits-melo-tts-zh_en.tar.bz2
    tar xjf melo.tar.bz2
    python3 tools/gen_pronunciation_demo.py --model-dir vits-melo-tts-zh_en
"""
import argparse
import json
import os
import re

import lameenc
import numpy as np
import sherpa_onnx

# The 10 sentences shown on the demo page (HSK 1 #1–10).
SENTENCES = [
    {"n": 1, "zh": "你好！"},
    {"n": 2, "zh": "你好吗？"},
    {"n": 3, "zh": "我很好，谢谢。"},
    {"n": 4, "zh": "你叫什么名字？"},
    {"n": 5, "zh": "我叫李明。"},
    {"n": 6, "zh": "他叫什么名字？"},
    {"n": 7, "zh": "她的名字很漂亮。"},
    {"n": 8, "zh": "认识你很高兴。"},
    {"n": 9, "zh": "这是我的朋友。"},
    {"n": 10, "zh": "那是我的老师。"},
]


def build_tts(model_dir: str) -> sherpa_onnx.OfflineTts:
    cfg = sherpa_onnx.OfflineTtsConfig(
        model=sherpa_onnx.OfflineTtsModelConfig(
            vits=sherpa_onnx.OfflineTtsVitsModelConfig(
                model=os.path.join(model_dir, "model.onnx"),
                lexicon=os.path.join(model_dir, "lexicon.txt"),
                tokens=os.path.join(model_dir, "tokens.txt"),
                dict_dir=os.path.join(model_dir, "dict"),
            ),
            num_threads=2,
        ),
        rule_fsts=",".join(
            os.path.join(model_dir, f)
            for f in ("date.fst", "number.fst", "phone.fst")
            if os.path.exists(os.path.join(model_dir, f))
        ),
    )
    return sherpa_onnx.OfflineTts(cfg)


def wav_to_mp3(samples: np.ndarray, sample_rate: int, path: str) -> None:
    pcm16 = np.clip(samples, -1.0, 1.0)
    pcm16 = (pcm16 * 32767).astype("<i2").tobytes()
    enc = lameenc.Encoder()
    enc.set_bit_rate(96)
    enc.set_in_sample_rate(sample_rate)
    enc.set_channels(1)
    enc.set_quality(2)
    mp3 = enc.encode(pcm16) + enc.flush()
    with open(path, "wb") as f:
        f.write(mp3)


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--model-dir", default="vits-melo-tts-zh_en")
    ap.add_argument(
        "--out-dir",
        default=os.path.join(os.path.dirname(__file__), "..", "pages", "audio_demo"),
    )
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    tts = build_tts(args.model_dir)

    for s in SENTENCES:
        audio = tts.generate(s["zh"], sid=0, speed=1.0)
        out = os.path.join(args.out_dir, f"hsk1_{s['n']:02d}.mp3")
        wav_to_mp3(np.array(audio.samples), audio.sample_rate, out)
        print(f"wrote {out}  ({s['zh']})")


if __name__ == "__main__":
    main()
