"""Shared ffmpeg helpers. Pure I/O utilities; paths come from callers."""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path


def require_tool(name: str) -> None:
    if shutil.which(name) is None:
        raise RuntimeError(f"required tool '{name}' not found in PATH")


def duration(path: Path) -> float | None:
    require_tool("ffprobe")
    try:
        proc = subprocess.run(
            ["ffprobe", "-v", "error", "-show_entries", "format=duration",
             "-of", "default=noprint_wrappers=1:nokey=1", str(path)],
            capture_output=True, check=True, text=True,
        )
        return float(proc.stdout.strip())
    except (subprocess.CalledProcessError, ValueError):
        return None


def chunk_mp3(path: Path, workdir: Path, chunk_seconds: float, prefix: str = "chunk") -> list[tuple[Path, float]]:
    """Split audio into <=chunk_seconds mp3 parts. Returns (file, offset) pairs."""
    require_tool("ffmpeg")
    total = duration(path)
    if total is None or total <= chunk_seconds:
        return [(path, 0.0)]
    parts: list[tuple[Path, float]] = []
    start = 0.0
    index = 0
    while start < total:
        out = workdir / f"{prefix}_{index:03d}.mp3"
        subprocess.run(
            ["ffmpeg", "-nostdin", "-loglevel", "error", "-ss", f"{start:.1f}",
             "-t", str(chunk_seconds), "-i", str(path),
             "-vn", "-ac", "1", "-ar", "16000", "-b:a", "64k", str(out)],
            check=True,
        )
        parts.append((out, start))
        start += chunk_seconds
        index += 1
    return parts


def to_wav(path: Path, workdir: Path, name: str = "audio.wav") -> Path:
    """Transcode any ffmpeg-readable media to 16kHz mono wav."""
    require_tool("ffmpeg")
    out = workdir / name
    subprocess.run(
        ["ffmpeg", "-nostdin", "-loglevel", "error", "-i", str(path),
         "-vn", "-ac", "1", "-ar", "16000", "-c:a", "pcm_s16le", str(out)],
        check=True,
    )
    return out
