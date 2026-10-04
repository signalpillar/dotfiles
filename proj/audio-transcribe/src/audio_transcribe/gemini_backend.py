"""Gemini transcription backend with native diarization.

One API call returns text, speaker labels, and word timestamps together,
so the slow local pyannote pass is unnecessary here. Takes all secrets
and model names as arguments; reads no env itself.
"""

from __future__ import annotations

import mimetypes
from pathlib import Path

from . import audio as audio_mod
from .config import GEMINI_MODEL

# Model budget: 98304 input tokens at ~32 tokens per audio second.
# 45-minute chunks stay under it with margin.
CHUNK_SECONDS = 2700

# yapsnap uses "iw" for Hebrew; BCP-47 expects "he".
LANG_ALIASES = {"iw": "he"}


def _client(api_key: str | None):
    if not api_key:
        raise RuntimeError("GEMINI_API_KEY is not set; export it to use the gemini backend")
    from google import genai

    return genai.Client(api_key=api_key)


def normalize_lang(lang: str | None) -> str | None:
    if not lang or lang.lower() == "auto":
        return None
    return LANG_ALIASES.get(lang.lower(), lang.lower())


def _offset(value: str | float | None) -> float:
    if value is None:
        return 0.0
    if isinstance(value, (int, float)):
        return float(value)
    return float(str(value).rstrip("s") or 0.0)


def _words_start(words) -> float | None:
    for word in words or []:
        get = (lambda k: word.get(k)) if isinstance(word, dict) else (lambda k: getattr(word, k, None))
        start = _offset(get("start_offset") or get("start"))
        if start > 0 or (get("word") or "").strip():
            return start
    return None


def _parse_transcription(entry) -> tuple[int | None, float, str]:
    """Map one API transcription to (speaker_index, start, text)."""
    get = (lambda k: entry.get(k)) if isinstance(entry, dict) else (lambda k: getattr(entry, k, None))
    label = (get("speaker_label") or "").strip()
    speaker = None
    if label.startswith("spk_") and label[4:].isdigit():
        speaker = int(label[4:]) - 1
    elif label.startswith("spk:") and label[4:].isdigit():
        speaker = int(label[4:])
    text = (get("text") or "").strip()
    start = _words_start(get("words"))
    return speaker, start if start is not None else 0.0, text


def _iter_transcriptions(response):
    for candidate in getattr(response, "candidates", None) or []:
        content = getattr(candidate, "content", None)
        for part in getattr(content, "parts", None) or []:
            entry = getattr(part, "audio_transcription", None)
            if entry is not None:
                yield entry


def _transcribe_remote(client, model: str, audio_uri: str, mime_type: str,
                       lang: str | None, timestamps: bool, diarize: bool) -> list:
    from google.genai import types

    language = normalize_lang(lang)
    audio_config = None
    if timestamps or diarize:
        audio_config = {"word_timestamp": True, "diarization": diarize, "mode": "VERBATIM"}
        if language:
            audio_config["language_codes"] = [language]
    response = client.models.generate_content(
        model=model,
        contents=[types.Part.from_uri(file_uri=audio_uri, mime_type=mime_type)],
        config=types.GenerateContentConfig(audio_transcription_config=audio_config),
    )
    return response


def _upload(client, path: Path):
    return client.files.upload(file=str(path))


def _delete(client, name: str) -> None:
    try:
        client.files.delete(name=name)
    except Exception:
        pass


def _parts(path: Path, workdir: Path) -> list[tuple[Path, float]]:
    """Split into API-sized mp3 parts, transcoding lone mkv files to wav."""
    parts = audio_mod.chunk_mp3(path, workdir, CHUNK_SECONDS)
    if len(parts) == 1 and parts[0][0].suffix.lower() == ".mkv":
        return [(audio_mod.to_wav(path, workdir, "upload.wav"), 0.0)]
    return parts


def dump_response(response, dest: Path) -> None:
    """Write the raw API response as JSON for debugging label/timestamp gaps."""
    try:
        text = response.model_dump_json(indent=2)
    except Exception:
        text = repr(response)
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_text(text, encoding="utf-8")


def transcribe_labeled(
    path: Path, api_key: str | None, lang: str | None = None,
    timestamps: bool = False, diarize: bool = False, model: str = GEMINI_MODEL,
    debug_path: Path | None = None,
) -> list[tuple[int | None, float, str]]:
    """Transcribe and return (speaker_index, start, text) in file time.

    Files past the token budget split into chunked requests; segment times
    shift back by each chunk offset. Speaker labels restart per chunk, so
    multi-chunk diarization needs a manual identity pass.
    """
    import sys
    import tempfile
    import warnings

    warnings.filterwarnings("ignore", message=".*automatic function calling.*")

    client = _client(api_key)
    with tempfile.TemporaryDirectory(prefix="gemini-transcribe-") as tmp:
        parts = _parts(path, Path(tmp))
        if len(parts) > 1 and diarize:
            print("note: speaker labels restart each chunk; check identities across chunks",
                  file=sys.stderr)
        labeled: list[tuple[int | None, float, str]] = []
        for index, (part, offset) in enumerate(parts):
            print(f"gemini: part {index + 1}/{len(parts)} (+{offset:.0f}s)", file=sys.stderr)
            mime_type = mimetypes.guess_type(str(part))[0] or "audio/mpeg"
            uploaded = _upload(client, part)
            try:
                response = _transcribe_remote(client, model, uploaded.uri, mime_type, lang, timestamps, diarize)
            finally:
                _delete(client, uploaded.name)
            if debug_path is not None and index == 0:
                dump_response(response, debug_path)
                print(f"debug: raw response at {debug_path}", file=sys.stderr)
            entries = list(_iter_transcriptions(response))
            labels = set()
            for entry in entries:
                get = (lambda k: entry.get(k)) if isinstance(entry, dict) else (lambda k: getattr(entry, k, None))
                labels.add((get("speaker_label") or "").strip() or "<none>")
            print(f"gemini: {len(entries)} transcription parts, speaker labels: {sorted(labels)}", file=sys.stderr)
            for spk, t, s in (_parse_transcription(e) for e in entries):
                if s:
                    labeled.append((spk, t + offset, s))
            if not entries:
                text = (getattr(response, "text", None) or "").strip()
                if text:
                    labeled.append((None, offset, text))
    return labeled


def transcribe_segments(
    path: Path, api_key: str | None, lang: str | None = None, model: str = GEMINI_MODEL
) -> list[tuple[float, str]]:
    """Transcribe and return (start_seconds, text) segments in file time."""
    return [(t, s) for _, t, s in transcribe_labeled(path, api_key, lang=lang, model=model)]


def transcribe_file(
    path: Path, api_key: str | None, lang: str | None = None,
    timestamps: bool = False, model: str = GEMINI_MODEL,
) -> str:
    """Transcribe a local audio/video file. Returns text."""
    from .openai_backend import _mmss

    labeled = transcribe_labeled(path, api_key, lang=lang, timestamps=timestamps, model=model)
    if timestamps:
        return "\n".join(f"[{_mmss(t)}] {s}" for _, t, s in labeled)
    return " ".join(s for _, _, s in labeled)
