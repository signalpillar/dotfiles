"""Speaker diarization with pyannote.audio (openai backend companion).

OpenAI transcription returns text with no speaker labels. This module runs
the pyannote speaker-diarization pipeline over the same audio and maps each
transcript segment to a speaker. Turn storage and sentence labeling reuse
yapsnap's SpeakerTurn/label_sentences, so both backends share one format:
"SPEAKER_00 [MM:SS]: text".

Takes the Hub token and pipeline id as arguments; reads no env itself.
The token needs access to the gated pipeline model, whose terms the user
accepts once on HuggingFace.
"""

from __future__ import annotations

from pathlib import Path

from .config import DIARIZATION_PIPELINE


def _patch_hub_auth() -> None:
    """Translate pyannote 3.x `use_auth_token` to the modern hub `token`.

    pyannote.audio 3.4.0 calls hf_hub_download(..., use_auth_token=...), a
    keyword huggingface-hub removed. Patch before pyannote binds the
    function, so both its call sites pick up the wrapper.
    """
    import functools

    import huggingface_hub

    orig = huggingface_hub.hf_hub_download
    if getattr(orig, "_audio_transcribe_compat", False):
        return

    @functools.wraps(orig)
    def patched(*args, use_auth_token=None, **kwargs):
        if use_auth_token is not None:
            kwargs.setdefault("token", use_auth_token)
        return orig(*args, **kwargs)

    patched._audio_transcribe_compat = True  # type: ignore[attr-defined]
    huggingface_hub.hf_hub_download = patched


def _load_pipeline(pipeline: str, token: str):
    """Load the pipeline, allowlisting the checkpoint metadata types.

    torch>=2.6 loads weights-only by default and rejects globals stored in
    pyannote 3.x checkpoints. Allowlisted are the torch version stamp plus
    every class in pyannote's task-spec module (Problem, Specifications, and
    kin): metadata types from the trusted gated repo, not code. Scoped to
    this call through safe_globals; older torch without it loads as before.
    """
    import contextlib
    import inspect

    import pyannote.audio.core.task as task_module
    from pyannote.audio import Pipeline

    try:
        import torch.torch_version
        from torch.serialization import safe_globals

        guard = safe_globals(
            [torch.torch_version.TorchVersion]
            + [obj for _, obj in inspect.getmembers(task_module, inspect.isclass)]
        )
    except (ImportError, AttributeError):
        guard = contextlib.nullcontext()
    with guard:
        return Pipeline.from_pretrained(pipeline, use_auth_token=token)


_AUTH_HINTS = ("401", "403", "unauthorized", "repositorynotfound", "gated", "permission")


def _load_error(pipeline: str, error: Exception) -> RuntimeError:
    text = str(error)
    hint = ""
    if any(mark in text.lower() for mark in _AUTH_HINTS):
        hint = (
            "; check HF_TOKEN and the gated terms at "
            f"https://huggingface.co/{pipeline} (sub-models such as "
            "pyannote/segmentation-3.0 are gated separately)"
        )
    return RuntimeError(f"could not load '{pipeline}': {text}{hint}")


def _to_wav(path: Path, workdir: Path) -> Path:
    """Transcode any ffmpeg-readable media to 16kHz mono wav.

    torchaudio opens only a few containers (mkv fails). The timeline is
    unchanged, so turns map back onto the original file one to one.
    """
    from . import audio as audio_mod

    return audio_mod.to_wav(path, workdir, "diarize.wav")


def get_turns(path: Path, token: str | None, num_speakers: int | None = None,
              pipeline: str = DIARIZATION_PIPELINE) -> list:
    """Run pyannote diarization. Returns sorted yapsnap SpeakerTurns."""
    import warnings

    warnings.filterwarnings("ignore", message=".*list_audio_backends.*", category=UserWarning)
    warnings.filterwarnings("ignore", message=".*TorchCodec.*", category=UserWarning)

    from yapsnap.diarize import SpeakerTurn

    if not token:
        raise RuntimeError(
            "HF_TOKEN is not set; export a HuggingFace token with access to "
            f"{pipeline} to use --diarize with the openai backend"
        )
    _patch_hub_auth()
    try:
        import pyannote.audio  # noqa: F401
    except ImportError:
        raise RuntimeError("pyannote.audio is missing; install with: uv sync --extra diarize")

    try:
        pipe = _load_pipeline(pipeline, token)
    except Exception as e:
        raise _load_error(pipeline, e) from e
    if pipe is None:
        raise RuntimeError(
            f"could not load '{pipeline}'; the model is gated. Export a token "
            "with access (HF_TOKEN) and accept the terms once at "
            f"https://huggingface.co/{pipeline}"
        )
    kwargs = {"num_speakers": num_speakers} if num_speakers and num_speakers > 0 else {}
    try:
        import tempfile

        with tempfile.TemporaryDirectory(prefix="diarize-") as tmp:
            diarization = pipe({"audio": str(_to_wav(path, Path(tmp)))}, **kwargs)
    except Exception as e:
        raise RuntimeError(f"diarization failed on {path.name}: {e}") from e

    speakers: dict[str, int] = {}
    turns: list = []
    for segment, _, label in diarization.itertracks(yield_label=True):
        if label not in speakers:
            speakers[label] = len(speakers)
        turns.append(SpeakerTurn(start=float(segment.start), end=float(segment.end), speaker=speakers[label]))
    turns.sort(key=lambda t: (t.start, t.end))
    return turns


def label_segments(segments: list[tuple[float, str]], turns: list) -> list[tuple[int | None, float, str]]:
    """Attach a speaker index to each (start_time, text) segment."""
    from yapsnap.diarize import label_sentences

    return label_sentences(list(segments), list(turns))
