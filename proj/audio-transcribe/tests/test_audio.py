"""Shared ffmpeg helpers: duration, chunking, wav conversion."""

import shutil
import wave

import pytest

from audio_transcribe import audio as audio_mod


def _wav(path, seconds=3):
    with wave.open(str(path), "wb") as wav:
        wav.setnchannels(1)
        wav.setsampwidth(2)
        wav.setframerate(16000)
        wav.writeframes(b"\x00\x00" * 16000 * seconds)
    return path


@pytest.fixture(autouse=True)
def _ffmpeg():
    if shutil.which("ffmpeg") is None or shutil.which("ffprobe") is None:
        pytest.skip("ffmpeg missing")


def test_duration_reads_seconds(tmp_path):
    assert audio_mod.duration(_wav(tmp_path / "a.wav")) == pytest.approx(3.0, abs=0.1)


def test_chunk_splits_with_offsets(tmp_path):
    parts = audio_mod.chunk_mp3(_wav(tmp_path / "a.wav"), tmp_path, 1.0)
    assert [offset for _, offset in parts] == [0.0, 1.0, 2.0]
    assert all(p.suffix == ".mp3" and p.is_file() for p, _ in parts)


def test_short_file_stays_whole(tmp_path):
    src = _wav(tmp_path / "a.wav")
    assert audio_mod.chunk_mp3(src, tmp_path, 600.0) == [(src, 0.0)]


def test_to_wav_normalizes(tmp_path):
    out = audio_mod.to_wav(_wav(tmp_path / "a.wav"), tmp_path)
    assert out.suffix == ".wav" and out.stat().st_size > 0
