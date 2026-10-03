"""Reproducible setup: check tools and report key status.

Run with: uv run setup
Local models need no fetch step: yapsnap downloads its English model to
the user cache on first use. Anything else goes through OpenAI.
"""

from __future__ import annotations

import shutil
import sys

from .config import DIARIZATION_PIPELINE, get_settings
from .layout import ProjectLayout

REQUIRED_TOOLS = ("ffmpeg", "ffprobe", "yt-dlp")


def check_tools() -> list[str]:
    return [t for t in REQUIRED_TOOLS if shutil.which(t) is None]


def main() -> int:
    settings = get_settings()
    project = ProjectLayout.discover()
    root = project.root
    print(f"project root: {root}")
    print(f"cache dir: {settings.cache_dir}")

    missing = check_tools()
    if missing:
        print(f"missing tools: {', '.join(missing)}", file=sys.stderr)
        print("install with: brew install ffmpeg  (yt-dlp ships with this project)", file=sys.stderr)
        return 2
    print("tools ok: ffmpeg, ffprobe, yt-dlp")

    if settings.openai_api_key:
        print("OPENAI_API_KEY: set (openai backend ready)")
    else:
        print("OPENAI_API_KEY: missing (openai backend needs it)")

    if settings.gemini_api_key:
        print("GEMINI_API_KEY: set (gemini backend ready)")
    else:
        print("GEMINI_API_KEY: missing (gemini backend needs it; yapsnap backend works offline)")

    try:
        import pyannote.audio  # noqa: F401

        has_pyannote = True
    except ImportError:
        has_pyannote = False
    if has_pyannote:
        if settings.hf_token:
            print(f"HF_TOKEN: set (pyannote ready; accept the {DIARIZATION_PIPELINE} terms once on HuggingFace)")
        else:
            print("HF_TOKEN: missing (--diarize with the openai backend needs it; get it at huggingface.co/settings/tokens)")
    else:
        print("pyannote: not installed (optional; run: uv sync --extra diarize)")
    print("setup ok")
    return 0


if __name__ == "__main__":
    sys.exit(main())
