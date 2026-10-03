"""Application settings. This is the ONLY module that reads process env.

All configuration flows through Settings. Every other module receives plain
values as function arguments. Nothing else touches os.environ.
"""

from __future__ import annotations

import os
import sys
from functools import lru_cache
from pathlib import Path

from pydantic import AliasChoices, Field
from pydantic_settings import BaseSettings, SettingsConfigDict

OPENAI_MODEL = "whisper-1"
GEMINI_MODEL = "gemini-3.5-transcribe"
DIARIZATION_PIPELINE = "pyannote/speaker-diarization-3.1"


def _default_cache_dir() -> Path:
    if sys.platform == "darwin":
        return Path.home() / "Library" / "Caches" / "audio-transcribe"
    if sys.platform == "win32":
        base = os.environ.get("LOCALAPPDATA") or str(Path.home() / "AppData" / "Local")
        return Path(base) / "audio-transcribe"
    base = os.environ.get("XDG_CACHE_HOME") or str(Path.home() / ".cache")
    return Path(base) / "audio-transcribe"


class Settings(BaseSettings):
    """Every tunable of the app. Field names map to env vars case-insensitively."""

    model_config = SettingsConfigDict(extra="ignore")

    openai_api_key: str | None = None
    gemini_api_key: str | None = None
    hf_token: str | None = Field(
        default=None, validation_alias=AliasChoices("hf_token", "huggingface_hub_token")
    )
    openai_model: str = OPENAI_MODEL
    gemini_model: str = GEMINI_MODEL
    diarization_pipeline: str = DIARIZATION_PIPELINE
    cache_dir: Path = Field(default_factory=_default_cache_dir)
    transcripts_dir: Path = Path("transcripts")


@lru_cache(maxsize=1)
def get_settings() -> Settings:
    return Settings()
