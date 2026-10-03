"""Layout is pure and owns every path segment.

Same inputs give same outputs. Every computed path stays under the root
it was derived from. Segment literals live in layout.py only, so no other
module can build project paths behind its back.
"""

import re
from pathlib import Path

from audio_transcribe.layout import CacheLayout, ProjectLayout, RunLayout

SRC = Path(__file__).resolve().parents[1] / "src" / "audio_transcribe"
# `/ "segment"` joins outside layout.py. Mere mentions (arg choices, dict
# keys, setting defaults) do not build paths and stay allowed.
OWNED_JOINS = re.compile(r'/\s*"(transcripts|downloads|openai|gemini|diarization|pyannote)"')


def test_same_inputs_same_outputs():
    first = CacheLayout(Path("/cache"))
    second = CacheLayout(Path("/cache"))
    assert first.transcript_path("h", "en", "m") == second.transcript_path("h", "en", "m")
    assert first.turns_path("h", -1) == second.turns_path("h", -1)
    assert first.download_dir("https://x") == second.download_dir("https://x")


def test_paths_stay_under_roots(tmp_path):
    cache = CacheLayout(tmp_path / "cache")
    run = RunLayout(tmp_path / "cwd")
    for path, root in (
        (cache.download_dir("https://x"), cache.root),
        (cache.transcript_path("h", "en", "m"), cache.root),
        (cache.turns_path("h", -1), cache.root),
        (run.default_output("a.mp3"), run.cwd),
    ):
        assert root in path.parents, f"{path} escapes {root}"


def test_discover_anchors_checkout():
    from audio_transcribe.layout import ProjectLayout

    root = ProjectLayout.discover().root
    assert (root / "src").is_dir()
    assert (root / "pyproject.toml").is_file()


def test_default_output_names():
    run = RunLayout(Path("/cwd"))
    assert run.default_output("clip.mp4").name == "clip_transcript.txt"
    assert run.default_output("https://x.test/v?id=1").name.endswith("_transcript.txt")
    assert run.transcripts_dir() == Path("/cwd/transcripts")


def test_segments_owned_by_layout():
    offenders = []
    for path in sorted(SRC.glob("*.py")):
        if path.name == "layout.py":
            continue
        for lineno, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
            if OWNED_JOINS.search(line):
                offenders.append(f"{path.name}:{lineno}: {line.strip()}")
    assert not offenders, "path segments outside layout.py:\n" + "\n".join(offenders)
