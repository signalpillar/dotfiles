"""Speaker labeling maps segments onto turns without model inference."""

from yapsnap.diarize import SpeakerTurn

from audio_transcribe.diarize import label_segments


def test_label_segments():
    turns = [SpeakerTurn(0.0, 5.0, 0), SpeakerTurn(5.0, 10.0, 1)]
    labeled = label_segments([(1.0, "hello"), (6.0, "world")], turns)
    assert labeled == [(0, 1.0, "hello"), (1, 6.0, "world")]
