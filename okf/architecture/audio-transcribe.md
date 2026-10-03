---
type: Architecture
title: "audio-transcribe: local and cloud transcription"
description: "Two-backend transcriber (yapsnap English offline, OpenAI whisper-1 for the rest) with settings, cache, and layout modules."
tags: [audio, transcription, whisper, yapsnap, pyannote, uv]
status: stable
sources:
  - id: proj-audio-transcribe-pyproject-toml
    resource: /proj/audio-transcribe/pyproject.toml
  - id: proj-audio-transcribe-src-audio-transcribe-init-py
    resource: /proj/audio-transcribe/src/audio_transcribe/__init__.py
  - id: proj-audio-transcribe-src-audio-transcribe-config-py
    resource: /proj/audio-transcribe/src/audio_transcribe/config.py
  - id: proj-audio-transcribe-src-audio-transcribe-cache-py
    resource: /proj/audio-transcribe/src/audio_transcribe/cache.py
  - id: proj-audio-transcribe-src-audio-transcribe-layout-py
    resource: /proj/audio-transcribe/src/audio_transcribe/layout.py
  - id: proj-audio-transcribe-src-audio-transcribe-openai-backend-py
    resource: /proj/audio-transcribe/src/audio_transcribe/openai_backend.py
  - id: proj-audio-transcribe-src-audio-transcribe-gemini-backend-py
    resource: /proj/audio-transcribe/src/audio_transcribe/gemini_backend.py
  - id: proj-audio-transcribe-src-audio-transcribe-diarize-py
    resource: /proj/audio-transcribe/src/audio_transcribe/diarize.py
  - id: proj-audio-transcribe-src-audio-transcribe-setup-py
    resource: /proj/audio-transcribe/src/audio_transcribe/setup.py
---

# audio-transcribe: local and cloud transcription

One CLI transcribes files or URLs through three backends.
`yapsnap` runs offline and handles English only.
OpenAI `whisper-1` handles any language; long files split into 10-minute chunks.
Gemini `gemini-3.5-transcribe` handles any language and diarizes natively, with no local GPU pass.
`--diarize` adds speaker labels: Gemini and pyannote for the cloud backends, yapsnap's native diarizer otherwise.

## Module roles

* `config.py` is the only module that reads env (`Settings` via pydantic-settings).
* `layout.py` computes every path as frozen instances (`ProjectLayout`, `CacheLayout`, `RunLayout`).
* `cache.py` does cache I/O against layout paths: downloads keyed by URL, transcripts and turns keyed by audio content hash.
* `openai_backend.py` and `diarize.py` take secrets as arguments and read no env.
* `setup.py` checks tools and reports key status. No model fetch step exists.

## Dependency notes

* `sherpa-onnx-core` is pinned explicitly: the `sherpa-onnx` sdist metadata omits it, so `uv lock` never pulls it otherwise.
* `pyannote.audio` lives in the optional `diarize` extra with pinned `torch`/`torchaudio`/`huggingface-hub` floors (see the diarization troubleshooting note).
* `diarize.py` shims pyannote 3.x `use_auth_token` to the modern hub `token` before import.
