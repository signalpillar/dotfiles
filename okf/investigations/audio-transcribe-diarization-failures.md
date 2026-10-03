---
type: Investigation
title: "audio-transcribe: six diarization failures and their fixes"
description: "Troubleshooting log for the audio-transcribe diarization path: missing native lib, torch pins, hub flag rename, weights-only globals, mkv input, gated models."
tags: [audio, transcription, pyannote, torch, huggingface, troubleshooting]
status: stable
sources:
  - id: proj-audio-transcribe-pyproject-toml
    resource: /proj/audio-transcribe/pyproject.toml
  - id: proj-audio-transcribe-src-audio-transcribe-diarize-py
    resource: /proj/audio-transcribe/src/audio_transcribe/diarize.py
---

# audio-transcribe: six diarization failures and their fixes

Each failure below broke `audio-transcribe --backend openai --diarize` in turn.
Symptom first, then cause, then the fix that shipped.

## 1. `libonnxruntime.dylib` missing at import

Symptom: `import sherpa_onnx` fails with `Library not loaded: @rpath/libonnxruntime.dylib`.
Cause: the native lib moved to the split `sherpa-onnx-core` wheel, but the `sherpa-onnx` sdist metadata omits the dependency, so `uv lock` never pulls it.
Fix: pin `sherpa-onnx-core==1.13.8` explicitly in `pyproject.toml`.

## 2. `torchaudio has no attribute AudioMetaData`

Symptom: `from pyannote.audio import Pipeline` crashes at import.
Cause: the resolver picked pyannote 3.4.0 with newest torch/torchaudio, which removed the attribute pyannote 3.4.0 needs. Latest pyannote 4.x is out because it needs `numpy>=2` and yapsnap pins `numpy<2`.
Fix: pin `torch==2.8.*` with `torchaudio==2.8.*` in the `diarize` extra.

## 3. `hf_hub_download() got an unexpected keyword argument use_auth_token`

Symptom: pipeline download fails on the kwarg.
Cause: pyannote 3.4.0 passes `use_auth_token`, which modern huggingface-hub removed. No hub version has both the new API and the old flag.
Fix: `diarize.py` wraps `hf_hub_download` before pyannote binds it, translating `use_auth_token` to `token`. The extra pins `huggingface-hub>=2`.

## 4. `Weights only load failed` on pipeline checkpoints

Symptom: torch 2.8 refuses pyannote 3.x checkpoints naming `TorchVersion`, then `Specifications`, then `Problem`.
Cause: torch>=2.6 defaults `torch.load` to weights-only and rejects those globals.
Fix: load inside `torch.serialization.safe_globals` with the torch stamp plus every class in `pyannote.audio.core.task`. Scoped to the call, no global mutation.

## 5. `Format not recognised` on the cached download

Symptom: diarization fails opening the cached `.mkv`.
Cause: torchaudio opens few containers; yt-dlp saved Matroska.
Fix: `diarize.py` transcodes any input to 16kHz mono wav with ffmpeg first. Same timeline, so turns map back one to one.

## 6. Gated models return None instead of raising
Symptom: `Pipeline.from_pretrained` prints a warning and returns `None`; the next line crashes with `'NoneType' object is not callable` (or `has no attribute 'eval'` for sub-models).
Cause: pyannote treats auth failure as a soft miss.
Fix: `get_turns` raises a `RuntimeError` naming the token and the exact terms page. Note the pipeline and `segmentation-3.0` are gated separately; both pages need acceptance.

## 7. Gemini returns `SPEAKER_??` for every segment

Symptom: diarized Gemini output labels all lines `SPEAKER_??` despite 134 transcription parts arriving.
Cause: live labels use the `spk:0` form (colon, zero-based), while the SDK docs describe `spk_1` (underscore, one-based). The parser only read the documented form, so every label missed.
Fix: `gemini_backend.py` accepts both forms. A stderr summary (`N transcription parts, speaker labels: [...]`) plus `--debug` raw-response dump made the dialect visible in one run.
