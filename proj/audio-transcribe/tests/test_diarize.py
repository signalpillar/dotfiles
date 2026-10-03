"""Diarization failures stay loud and guided; wav conversion is container-proof."""

from pathlib import Path

import pytest

from audio_transcribe import diarize as diar


def test_none_pipeline_raises_helpful_error(monkeypatch):
    pytest.importorskip("pyannote.audio")
    from pyannote.audio import Pipeline

    monkeypatch.setattr(Pipeline, "from_pretrained", classmethod(lambda cls, *a, **k: None))
    with pytest.raises(RuntimeError, match="gated"):
        diar.get_turns(Path("/tmp/probe.wav"), token="hf-test")


def test_broken_submodel_raises_helpful_error(monkeypatch):
    pytest.importorskip("pyannote.audio")
    from pyannote.audio import Pipeline

    def boom(cls, *a, **k):
        raise AttributeError("'NoneType' object has no attribute 'eval'")

    monkeypatch.setattr(Pipeline, "from_pretrained", classmethod(boom))
    with pytest.raises(RuntimeError, match="could not load"):
        diar.get_turns(Path("/tmp/probe.wav"), token="hf-test")


def test_error_hints_split_auth_from_load_failures():
    auth = diar._load_error("pipe/id", RuntimeError("403 Forbidden"))
    assert "gated" in str(auth)
    weights = diar._load_error("pipe/id", RuntimeError("Weights only load failed"))
    assert "gated" not in str(weights)
    assert "Weights only load failed" in str(weights)


def test_allowlist_covers_task_module(monkeypatch):
    pytest.importorskip("pyannote.audio")
    from pyannote.audio import Pipeline

    import torch.serialization as ser

    seen = {}
    orig = ser.safe_globals

    def spy(globals_list):
        seen["globals"] = list(globals_list)
        return orig(globals_list)

    monkeypatch.setattr(ser, "safe_globals", spy)
    monkeypatch.setattr(Pipeline, "from_pretrained", classmethod(lambda cls, *a, **k: object()))
    diar._load_pipeline("pipe/id", "tok")
    names = {g.__name__ for g in seen["globals"]}
    assert {"TorchVersion", "Specifications", "Problem"} <= names


def test_to_wav_transcodes_any_container(tmp_path):
    import shutil
    import wave

    if shutil.which("ffmpeg") is None:
        pytest.skip("ffmpeg missing")
    wav_path = tmp_path / "clip.wav"
    with wave.open(str(wav_path), "wb") as wav:
        wav.setnchannels(1)
        wav.setsampwidth(2)
        wav.setframerate(16000)
        wav.writeframes(b"\x00\x00" * 16000)
    src = tmp_path / "clip.mkv"
    shutil.copy(wav_path, src)
    out = diar._to_wav(src, tmp_path)
    assert out.suffix == ".wav" and out.stat().st_size > 0
