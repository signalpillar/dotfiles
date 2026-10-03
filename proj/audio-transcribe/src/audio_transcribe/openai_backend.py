"""OpenAI transcription backend.

Sends audio to the OpenAI API. Handles the 25 MB per-request limit by
splitting larger inputs into 10-minute mp3 chunks with ffmpeg. Takes all
secrets and model names as arguments; reads no env itself.
"""

from __future__ import annotations

import shutil
import subprocess
import tempfile
from pathlib import Path

from .config import OPENAI_MODEL

MAX_BYTES = 24 * 1024 * 1024
CHUNK_SECONDS = 600

# yapsnap uses "iw" for Hebrew; the API expects "he".
LANG_ALIASES = {"iw": "he"}


def _require_tool(name: str) -> None:
    if shutil.which(name) is None:
        raise RuntimeError(f"required tool '{name}' not found in PATH")


def _client(api_key: str | None):
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY is not set; export it to use the openai backend")
    from openai import OpenAI

    return OpenAI(api_key=api_key)


def normalize_lang(lang: str | None) -> str | None:
    if not lang or lang.lower() == "auto":
        return None
    return LANG_ALIASES.get(lang.lower(), lang.lower())


def _duration(path: Path) -> float | None:
    _require_tool("ffprobe")
    try:
        proc = subprocess.run(
            ["ffprobe", "-v", "error", "-show_entries", "format=duration",
             "-of", "default=noprint_wrappers=1:nokey=1", str(path)],
            capture_output=True, check=True, text=True,
        )
        return float(proc.stdout.strip())
    except (subprocess.CalledProcessError, ValueError):
        return None


def _chunk(path: Path, workdir: Path) -> list[tuple[Path, float]]:
    """Split audio into <=CHUNK_SECONDS mp3 parts. Returns (file, offset) pairs."""
    _require_tool("ffmpeg")
    duration = _duration(path)
    if duration is None or duration <= CHUNK_SECONDS:
        return [(path, 0.0)]
    parts: list[tuple[Path, float]] = []
    start = 0.0
    index = 0
    while start < duration:
        out = workdir / f"chunk_{index:03d}.mp3"
        subprocess.run(
            ["ffmpeg", "-nostdin", "-loglevel", "error", "-ss", f"{start:.1f}",
             "-t", str(CHUNK_SECONDS), "-i", str(path),
             "-vn", "-ac", "1", "-ar", "16000", "-b:a", "64k", str(out)],
            check=True,
        )
        parts.append((out, start))
        start += CHUNK_SECONDS
        index += 1
    return parts


def _segments_one(client, model: str, path: Path, lang: str | None, offset: float = 0.0) -> list[tuple[float, str]]:
    resp = client.audio.transcriptions.create(
        model=model, file=open(path, "rb"), language=lang,
        response_format="verbose_json", timestamp_granularities=["segment"],
    )
    out = []
    for seg in resp.segments:
        start = float(seg["start"] if isinstance(seg, dict) else seg.start) + offset
        text = (seg["text"] if isinstance(seg, dict) else seg.text).strip()
        out.append((start, text))
    return out


def _transcribe_one(client, model: str, path: Path, lang: str | None, timestamps: bool, offset: float = 0.0) -> str:
    if timestamps:
        return "\n".join(f"[{_mmss(t)}] {s}" for t, s in _segments_one(client, model, path, lang, offset))
    resp = client.audio.transcriptions.create(model=model, file=open(path, "rb"), language=lang)
    return resp.text.strip()


def _mmss(seconds: float) -> str:
    seconds = max(0.0, float(seconds))
    m, s = divmod(int(seconds), 60)
    return f"{m:02d}:{s:02d}"


def transcribe_segments(
    path: Path, api_key: str | None, lang: str | None = None, model: str = OPENAI_MODEL
) -> list[tuple[float, str]]:
    """Transcribe and return (start_seconds, text) segments in file time."""
    client = _client(api_key)
    language = normalize_lang(lang)
    if path.stat().st_size <= MAX_BYTES:
        duration = _duration(path)
        if duration is None or duration <= CHUNK_SECONDS:
            return _segments_one(client, model, path, language)
    with tempfile.TemporaryDirectory(prefix="oai-transcribe-") as tmp:
        out: list[tuple[float, str]] = []
        for part, offset in _chunk(path, Path(tmp)):
            out.extend(_segments_one(client, model, part, language, offset))
        return out


def transcribe_file(
    path: Path, api_key: str | None, lang: str | None = None,
    timestamps: bool = False, model: str = OPENAI_MODEL,
) -> str:
    """Transcribe a local audio/video file. Returns text."""
    client = _client(api_key)
    language = normalize_lang(lang)
    if path.stat().st_size <= MAX_BYTES:
        duration = _duration(path)
        if duration is None or duration <= CHUNK_SECONDS:
            return _transcribe_one(client, model, path, language, timestamps)
    with tempfile.TemporaryDirectory(prefix="oai-transcribe-") as tmp:
        parts = _chunk(path, Path(tmp))
        return "\n".join(_transcribe_one(client, model, p, language, timestamps, off) for p, off in parts)
