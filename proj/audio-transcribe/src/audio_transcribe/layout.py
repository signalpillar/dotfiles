"""Every path in the project, computed in one place.

Frozen dataclass instances, passed around and queried through methods.
Same inputs always give same outputs: no I/O, no env, no globals here.
Callers perform I/O against layout paths but never join segments
themselves. Segment literals below appear nowhere else in src
(enforced by tests/test_layout.py).
"""

from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass
from pathlib import Path

TRANSCRIPTS_DIRNAME = "transcripts"
DOWNLOADS_DIRNAME = "downloads"
OPENAI_DIRNAME = "openai"
GEMINI_DIRNAME = "gemini"
DIARIZATION_DIRNAME = "diarization"
PYANNOTE_DIRNAME = "pyannote"
META_NAME = "meta.json"


@dataclass(frozen=True)
class ProjectLayout:
    """Paths under the checkout root holding src/ and tests/."""

    root: Path

    @classmethod
    def discover(cls) -> ProjectLayout:
        return cls(Path(__file__).resolve().parents[2])


@dataclass(frozen=True)
class CacheLayout:
    """Content-addressed cache: downloads, transcripts, turns."""

    root: Path

    def download_dir(self, url: str) -> Path:
        return self.root / DOWNLOADS_DIRNAME / hashlib.sha1(url.encode("utf-8")).hexdigest()

    def transcript_path(self, audio_hash: str, lang: str | None, model: str) -> Path:
        key = f"{audio_hash}-{(lang or 'auto').lower()}-{model}.json"
        return self.root / TRANSCRIPTS_DIRNAME / OPENAI_DIRNAME / key

    def gemini_transcript_path(self, audio_hash: str, lang: str | None, model: str, variant: str) -> Path:
        key = f"{audio_hash}-{(lang or 'auto').lower()}-{model}-{variant}.json"
        return self.root / TRANSCRIPTS_DIRNAME / GEMINI_DIRNAME / key

    def turns_path(self, audio_hash: str, num_speakers: int) -> Path:
        return self.root / DIARIZATION_DIRNAME / PYANNOTE_DIRNAME / f"{audio_hash}-{num_speakers}.json"


@dataclass(frozen=True)
class RunLayout:
    """Paths for one CLI run, anchored at the invoking directory."""

    cwd: Path

    def transcripts_dir(self) -> Path:
        return self.cwd / TRANSCRIPTS_DIRNAME

    def output_name(self, input_arg: str) -> str:
        if "://" not in input_arg:
            stem = Path(input_arg).stem
        else:
            stem = re.sub(r"[^A-Za-z0-9._-]+", "_", input_arg).strip("_")
        return f"{stem or 'transcript'}_transcript.txt"

    def default_output(self, input_arg: str) -> Path:
        return self.transcripts_dir() / self.output_name(input_arg)
