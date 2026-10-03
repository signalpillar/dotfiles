# Example

`uv run audio-transcribe '<URL>' --backend gemini --timestamps --diarize`

# audio-transcribe

Transcribe local files or URLs with three backends: local `yapsnap`, cloud OpenAI `whisper-1`, or cloud Gemini `gemini-3.5-transcribe` (native diarization, no local GPU pass).

New here? Read `docs/how-it-works.md` for concepts and pipeline diagrams.

## Setup

Run one command. It checks tools, fetches models, verifies checksums.

```bash
uv sync
uv run setup
```

For speaker labels with the OpenAI backend, also install the diarization extra:

```bash
uv sync --extra diarize
export HF_TOKEN=<token>
```

The pyannote model is gated. Accept its terms once at `huggingface.co/pyannote/speaker-diarization-3.1`.

Required tools: `ffmpeg`, `ffprobe`, `yt-dlp`.
`yt-dlp` ships with this project through `uv sync`.
Install `ffmpeg` with `brew install ffmpeg` when setup reports it missing.

Local models need no fetch step.
`yapsnap` downloads its English model to the user cache on first use.

## Config

`config.py` is the only module that reads env. All settings with defaults:

| Env | Default | Use |
| --- | --- | --- |
| `OPENAI_API_KEY` | - | openai backend |
| `GEMINI_API_KEY` | - | gemini backend |
| `HF_TOKEN` / `HUGGINGFACE_HUB_TOKEN` | - | pyannote diarization |
| `OPENAI_MODEL` | `whisper-1` | transcription model |
| `GEMINI_MODEL` | `gemini-3.5-transcribe` | transcription model |
| `DIARIZATION_PIPELINE` | `pyannote/speaker-diarization-3.1` | diarization model |
| `CACHE_DIR` | OS cache dir | download/transcript/turn cache |
| `TRANSCRIPTS_DIR` | `transcripts` | default output dir |

## Cache

Downloads, transcripts, and speaker turns persist under the cache dir, keyed by URL or audio content hash. Repeat runs reuse them without network or API calls. Pass `--no-cache` to bypass for one run.

## Layout

`layout.py` computes every path as frozen instances: `ProjectLayout`, `CacheLayout`, `RunLayout`. Callers receive instances and query methods. No other module joins path segments.

## Use

Default backend is OpenAI. It needs `OPENAI_API_KEY`.

```bash
export OPENAI_API_KEY=<key>
uv run audio-transcribe INPUT [-o OUT.txt] [--lang CODE] [--timestamps]
```

Run the local backend instead:

```bash
uv run audio-transcribe INPUT --backend yapsnap [--lang CODE] [--model DIR]
```

Run the Gemini backend (needs `GEMINI_API_KEY`, diarizes natively):

```bash
uv run audio-transcribe INPUT --backend gemini [--lang CODE] [--timestamps] [--diarize]
```

## Diarization

Label speakers with `--diarize [--num-speakers N]`.
Output lines look like `SPEAKER_00 [MM:SS]: text`.
The OpenAI backend diarizes with pyannote (needs the `diarize` extra and `HF_TOKEN`).
The Gemini backend diarizes natively (needs `GEMINI_API_KEY`, no extra install).
The yapsnap backend uses its native offline diarizer (no extra install, no token).

## Languages

Local transcription is English only: run `--backend yapsnap --lang en`.
Route every other language through `--backend openai`.
It accepts `ru`, `uk`, and all Whisper language codes.
