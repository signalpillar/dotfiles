"""Gemini response mapping without API calls."""

from types import SimpleNamespace

import pytest

from audio_transcribe import gemini_backend as gem


def _part(entry):
    return SimpleNamespace(audio_transcription=entry)


def _response(entries=None, text=""):
    parts = [_part(e) for e in (entries or [])]
    content = SimpleNamespace(parts=parts)
    return SimpleNamespace(candidates=[SimpleNamespace(content=content)], text=text)


def test_parse_dict_entry():
    entry = {"speaker_label": "spk_3", "text": "hello",
             "words": [{"word": "hello", "start_offset": "12.5s"}]}
    assert gem._parse_transcription(entry) == (2, 12.5, "hello")


def test_parse_colon_zero_based_label():
    entry = {"speaker_label": "spk:0", "text": "hello",
             "words": [{"word": "hello", "start_offset": "1.0s"}]}
    assert gem._parse_transcription(entry) == (0, 1.0, "hello")


def test_parse_object_entry_without_speaker():
    entry = SimpleNamespace(speaker_label=None, text="hi",
                            words=[SimpleNamespace(word="hi", start_offset="0.0s")])
    assert gem._parse_transcription(entry) == (None, 0.0, "hi")


def test_transcribe_labeled_reads_parts(monkeypatch, tmp_path):
    entries = [
        {"speaker_label": "spk_1", "text": "one", "words": [{"word": "one", "start_offset": "1.0s"}]},
        {"speaker_label": "spk_2", "text": "two", "words": [{"word": "two", "start_offset": "5.0s"}]},
    ]
    monkeypatch.setattr(gem, "_parts", lambda p, w: [(p, 0.0)])
    monkeypatch.setattr(gem, "_upload", lambda c, p: SimpleNamespace(uri="u", name="n"))
    monkeypatch.setattr(gem, "_delete", lambda c, n: None)
    monkeypatch.setattr(gem, "_transcribe_remote", lambda *a, **k: _response(entries))
    src = tmp_path / "a.mp3"
    src.write_bytes(b"x")
    assert gem.transcribe_labeled(src, "key", diarize=True) == [(0, 1.0, "one"), (1, 5.0, "two")]


def test_empty_entries_fall_back_to_text(monkeypatch, tmp_path):
    monkeypatch.setattr(gem, "_parts", lambda p, w: [(p, 0.0)])
    monkeypatch.setattr(gem, "_upload", lambda c, p: SimpleNamespace(uri="u", name="n"))
    monkeypatch.setattr(gem, "_delete", lambda c, n: None)
    monkeypatch.setattr(gem, "_transcribe_remote", lambda *a, **k: _response(text="plain"))
    src = tmp_path / "a.mp3"
    src.write_bytes(b"x")
    assert gem.transcribe_labeled(src, "key") == [(None, 0.0, "plain")]


def test_summary_reports_parts_and_labels(monkeypatch, tmp_path, capsys):
    entries = [
        {"speaker_label": "spk_1", "text": "one", "words": [{"word": "one", "start_offset": "1.0s"}]},
        {"speaker_label": "", "text": "two", "words": [{"word": "two", "start_offset": "5.0s"}]},
    ]
    monkeypatch.setattr(gem, "_parts", lambda p, w: [(p, 0.0)])
    monkeypatch.setattr(gem, "_upload", lambda c, p: SimpleNamespace(uri="u", name="n"))
    monkeypatch.setattr(gem, "_delete", lambda c, n: None)
    monkeypatch.setattr(gem, "_transcribe_remote", lambda *a, **k: _response(entries))
    src = tmp_path / "a.mp3"
    src.write_bytes(b"x")
    assert gem.transcribe_labeled(src, "key", diarize=True) == [(0, 1.0, "one"), (None, 5.0, "two")]
    err = capsys.readouterr().err
    assert "2 transcription parts" in err
    assert "spk_1" in err


def test_chunked_parts_stitch_with_offsets(monkeypatch, tmp_path):
    first = [{"speaker_label": "spk:0", "text": "one", "words": [{"word": "one", "start_offset": "1.0s"}]}]
    second = [{"speaker_label": "spk:1", "text": "two", "words": [{"word": "two", "start_offset": "2.0s"}]}]
    src = tmp_path / "a.mp3"
    src.write_bytes(b"x")
    monkeypatch.setattr(gem, "_parts", lambda p, w: [(src, 0.0), (src, 2700.0)])
    monkeypatch.setattr(gem, "_upload", lambda c, p: SimpleNamespace(uri="u", name="n"))
    monkeypatch.setattr(gem, "_delete", lambda c, n: None)
    calls = {"n": 0}

    def remote(*a, **k):
        calls["n"] += 1
        return _response(first if calls["n"] == 1 else second)

    monkeypatch.setattr(gem, "_transcribe_remote", remote)
    assert gem.transcribe_labeled(src, "key", diarize=True) == [(0, 1.0, "one"), (1, 2702.0, "two")]


def test_parts_transcode_lone_mkv(tmp_path):
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
    mkv = tmp_path / "clip.mkv"
    shutil.copy(wav_path, mkv)
    part, offset = gem._parts(mkv, tmp_path)[0]
    assert part.suffix == ".wav" and offset == 0.0


def test_missing_key_fails_fast(tmp_path):
    src = tmp_path / "a.mp3"
    src.write_bytes(b"x")
    with pytest.raises(RuntimeError, match="GEMINI_API_KEY"):
        gem.transcribe_labeled(src, None)


def test_lang_alias():
    assert gem.normalize_lang("iw") == "he"
    assert gem.normalize_lang("auto") is None
