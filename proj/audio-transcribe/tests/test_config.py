"""Settings map env vars; every other module receives plain values."""

import pytest

from audio_transcribe.config import get_settings


@pytest.fixture(autouse=True)
def _clean_env(monkeypatch):
    for var in ("OPENAI_API_KEY", "GEMINI_API_KEY", "HF_TOKEN", "HUGGINGFACE_HUB_TOKEN"):
        monkeypatch.delenv(var, raising=False)
    get_settings.cache_clear()
    yield
    get_settings.cache_clear()


def test_empty_env_gives_nones(monkeypatch):
    settings = get_settings()
    assert settings.openai_api_key is None
    assert settings.hf_token is None
    assert settings.gemini_api_key is None


def test_keys_map(monkeypatch):
    monkeypatch.setenv("OPENAI_API_KEY", "sk-test")
    monkeypatch.setenv("HF_TOKEN", "hf-test")
    monkeypatch.setenv("GEMINI_API_KEY", "g-test")
    settings = get_settings()
    assert settings.openai_api_key == "sk-test"
    assert settings.hf_token == "hf-test"
    assert settings.gemini_api_key == "g-test"


def test_hub_token_fallback(monkeypatch):
    monkeypatch.setenv("HUGGINGFACE_HUB_TOKEN", "hf-fallback")
    assert get_settings().hf_token == "hf-fallback"


def test_defaults_present():
    settings = get_settings()
    assert settings.openai_model == "whisper-1"
    assert settings.gemini_model == "gemini-3.5-transcribe"
    assert "pyannote" in settings.diarization_pipeline
    assert settings.cache_dir.name == "audio-transcribe"
