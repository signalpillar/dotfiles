"""Content-addressed on-disk cache. I/O only; all paths come from a CacheLayout.

Entries tolerate corruption: a bad file counts as a miss and gets
overwritten on the next store. Writes are atomic (temp file + rename).
"""

from __future__ import annotations

import hashlib
import json
import shutil
from pathlib import Path

from .layout import META_NAME, CacheLayout


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def _write_json(dest: Path, payload: dict) -> None:
    dest.parent.mkdir(parents=True, exist_ok=True)
    tmp = dest.with_suffix(dest.suffix + ".part")
    tmp.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")
    tmp.replace(dest)


def _read_json(dest: Path) -> dict | None:
    try:
        return json.loads(dest.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None


def get_download(cache: CacheLayout, url: str) -> Path | None:
    """Return the cached media file for a URL, or None on a miss."""
    entry = _read_json(cache.download_dir(url) / META_NAME)
    if not entry:
        return None
    media = cache.download_dir(url) / entry.get("file", "")
    if not media.is_file():
        return None
    return media


def put_download(cache: CacheLayout, url: str, media: Path) -> Path:
    """Store a downloaded file under the URL key. Returns the cached path."""
    dest_dir = cache.download_dir(url)
    dest_dir.mkdir(parents=True, exist_ok=True)
    dest = dest_dir / f"media{media.suffix}"
    if not (dest.is_file() and sha256_file(dest) == sha256_file(media)):
        tmp = dest.with_suffix(dest.suffix + ".part")
        shutil.copyfile(media, tmp)
        tmp.replace(dest)
    _write_json(dest_dir / META_NAME, {"url": url, "file": dest.name})
    return dest


def get_transcript(cache: CacheLayout, audio_hash: str, lang: str | None, model: str) -> dict | None:
    return _read_json(cache.transcript_path(audio_hash, lang, model))


def put_transcript(
    cache: CacheLayout, audio_hash: str, lang: str | None, model: str,
    text: str, segments: list[tuple[float, str]],
) -> None:
    _write_json(
        cache.transcript_path(audio_hash, lang, model),
        {"text": text, "segments": [[t, s] for t, s in segments]},
    )


def get_gemini(cache: CacheLayout, audio_hash: str, lang: str | None, model: str, variant: str) -> dict | None:
    return _read_json(cache.gemini_transcript_path(audio_hash, lang, model, variant))


def put_gemini(
    cache: CacheLayout, audio_hash: str, lang: str | None, model: str, variant: str,
    text: str, labeled: list[tuple],
) -> None:
    _write_json(
        cache.gemini_transcript_path(audio_hash, lang, model, variant),
        {"text": text, "labeled": [[spk, t, s] for spk, t, s in labeled]},
    )


def get_turns(cache: CacheLayout, audio_hash: str, num_speakers: int) -> list | None:
    entry = _read_json(cache.turns_path(audio_hash, num_speakers))
    if not entry or "turns" not in entry:
        return None
    return entry["turns"]


def put_turns(cache: CacheLayout, audio_hash: str, num_speakers: int, turns: list) -> None:
    _write_json(
        cache.turns_path(audio_hash, num_speakers),
        {"turns": [{"start": t.start, "end": t.end, "speaker": t.speaker} for t in turns]},
    )
