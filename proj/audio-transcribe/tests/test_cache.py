"""Cache round-trips, key separation, and corruption tolerance."""

from types import SimpleNamespace

from audio_transcribe import cache as cache_mod
from audio_transcribe.layout import CacheLayout


def _blob(tmp_path, name="a.bin", content=b"audio-bytes"):
    path = tmp_path / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(content)
    return path


def test_download_roundtrip(tmp_path):
    cache = CacheLayout(tmp_path / "cache")
    src = _blob(tmp_path, "clip.mp3")
    cached = cache_mod.put_download(cache, "https://x.test/v", src)
    assert cache_mod.get_download(cache, "https://x.test/v") == cached
    assert cache_mod.get_download(cache, "https://other.test/v") is None


def test_transcript_key_separates_lang(tmp_path):
    cache = CacheLayout(tmp_path / "cache")
    cache_mod.put_transcript(cache, "abc123", "en", "whisper-1", "hello", [(0.0, "hello")])
    assert cache_mod.get_transcript(cache, "abc123", "ru", "whisper-1") is None
    hit = cache_mod.get_transcript(cache, "abc123", "en", "whisper-1")
    assert hit["segments"] == [[0.0, "hello"]]


def test_gemini_roundtrip_and_variants(tmp_path):
    cache = CacheLayout(tmp_path / "cache")
    labeled = [(0, 1.0, "one")]
    cache_mod.put_gemini(cache, "abc123", "en", "gemini-3.5-transcribe", "diarized", "t", labeled)
    hit = cache_mod.get_gemini(cache, "abc123", "en", "gemini-3.5-transcribe", "diarized")
    assert hit["labeled"] == [[0, 1.0, "one"]]
    assert cache_mod.get_gemini(cache, "abc123", "en", "gemini-3.5-transcribe", "plain") is None


def test_turns_roundtrip(tmp_path):
    cache = CacheLayout(tmp_path / "cache")
    turns = [SimpleNamespace(start=0.0, end=1.5, speaker=0)]
    cache_mod.put_turns(cache, "abc123", -1, turns)
    assert cache_mod.get_turns(cache, "abc123", -1) == [{"start": 0.0, "end": 1.5, "speaker": 0}]
    assert cache_mod.get_turns(cache, "abc123", 2) is None


def test_corrupt_entry_is_a_miss(tmp_path):
    cache = CacheLayout(tmp_path / "cache")
    dest = cache.transcript_path("abc123", "en", "whisper-1")
    dest.parent.mkdir(parents=True)
    dest.write_text("{not json", encoding="utf-8")
    assert cache_mod.get_transcript(cache, "abc123", "en", "whisper-1") is None
