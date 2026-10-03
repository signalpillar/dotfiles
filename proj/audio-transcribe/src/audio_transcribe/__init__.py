"""audio-transcribe: local (yapsnap) and cloud (OpenAI, Gemini) transcription."""

from __future__ import annotations

import argparse
import shutil
import sys
import tempfile
from pathlib import Path
from typing import Optional

from . import cache as cache_mod
from .config import get_settings
from .layout import CacheLayout, RunLayout

DEFAULT_BACKEND = "openai"


def _resolve_input(arg: str, cache: CacheLayout, use_cache: bool) -> tuple[Path, Optional[Path]]:
    """Return (media_path, cleanup_dir). URLs reuse the download cache."""
    import yapsnap

    if not yapsnap.is_url(arg):
        media = Path(arg)
        if not media.is_file():
            raise FileNotFoundError(f"file not found: {media}")
        return media, None
    if use_cache:
        hit = cache_mod.get_download(cache, arg)
        if hit is not None:
            print(f"cache hit: download {hit}", file=sys.stderr)
            return hit, None
    tmp = Path(tempfile.mkdtemp(prefix="transcribe-"))
    media = yapsnap.download_url(arg, tmp)
    if use_cache:
        cached = cache_mod.put_download(cache, arg, media)
        shutil.rmtree(tmp, ignore_errors=True)
        return cached, None
    return media, tmp


def _mmss(seconds: float) -> str:
    seconds = max(0.0, float(seconds))
    m, s = divmod(int(seconds), 60)
    return f"{m:02d}:{s:02d}"


def _run_yapsnap(args: argparse.Namespace) -> int:
    import yapsnap

    argv = [args.input, "--lang", args.lang]
    if args.output:
        argv += ["-o", str(args.output)]
    if args.timestamps or args.diarize:
        argv += ["--timestamps"]
    if args.diarize:
        argv += ["--diarize", "--num-speakers", str(args.num_speakers)]
    if args.model:
        argv += ["--model", str(args.model)]
    return yapsnap.main(argv)


def _openai_segments(
    media: Path, audio_hash: str, args: argparse.Namespace, settings, cache: CacheLayout
) -> list[tuple[float, str]]:
    """Segments from cache or API. Same audio bytes, lang, and model share one entry."""
    from . import openai_backend

    if args.no_cache:
        return openai_backend.transcribe_segments(
            media, settings.openai_api_key, lang=args.lang, model=settings.openai_model
        )
    hit = cache_mod.get_transcript(cache, audio_hash, args.lang, settings.openai_model)
    if hit is not None:
        print("cache hit: transcript", file=sys.stderr)
        return [(float(t), s) for t, s in hit["segments"]]
    segments = openai_backend.transcribe_segments(
        media, settings.openai_api_key, lang=args.lang, model=settings.openai_model
    )
    cache_mod.put_transcript(cache, audio_hash, args.lang, settings.openai_model, "", segments)
    return segments


def _openai_turns(
    media: Path, audio_hash: str, args: argparse.Namespace, settings, cache: CacheLayout
) -> list:
    """Speaker turns from cache or pyannote."""
    from . import diarize as diar

    if args.no_cache:
        return diar.get_turns(media, settings.hf_token, args.num_speakers, settings.diarization_pipeline)
    hit = cache_mod.get_turns(cache, audio_hash, args.num_speakers)
    if hit is not None:
        from yapsnap.diarize import SpeakerTurn

        print("cache hit: diarization", file=sys.stderr)
        return [SpeakerTurn(start=t["start"], end=t["end"], speaker=t["speaker"]) for t in hit]
    turns = diar.get_turns(media, settings.hf_token, args.num_speakers, settings.diarization_pipeline)
    cache_mod.put_turns(cache, audio_hash, args.num_speakers, turns)
    return turns


def _gemini_variant(args: argparse.Namespace) -> str:
    if args.diarize:
        return "diarized"
    if args.timestamps:
        return "timed"
    return "plain"


def _format_labeled(labeled: list, diarize: bool, timestamps: bool) -> str:
    """Render (speaker, start, text) in the shared SPEAKER_00 [MM:SS]: text shape."""
    if diarize:
        lines = []
        for spk, t, s in labeled:
            label = f"SPEAKER_{spk:02d}" if spk is not None else "SPEAKER_??"
            lines.append(f"{label} [{_mmss(t)}]: {s}")
        return "\n".join(lines)
    if timestamps:
        return "\n".join(f"[{_mmss(t)}] {s}" for _, t, s in labeled)
    return " ".join(s for _, _, s in labeled)


def _run_gemini(args: argparse.Namespace) -> int:
    from . import gemini_backend

    settings = get_settings()
    cache = CacheLayout(settings.cache_dir)
    run = RunLayout(Path.cwd())
    cleanup: Optional[Path] = None
    try:
        try:
            media, cleanup = _resolve_input(args.input, cache, not args.no_cache)
        except Exception as e:
            print(f"input error: {e}", file=sys.stderr)
            return 1
        try:
            variant = _gemini_variant(args)
            labeled = None
            if not args.no_cache:
                audio_hash = cache_mod.sha256_file(media)
                hit = cache_mod.get_gemini(cache, audio_hash, args.lang, settings.gemini_model, variant)
                if hit is not None:
                    print("cache hit: gemini transcript", file=sys.stderr)
                    labeled = [(spk, float(t), s) for spk, t, s in hit["labeled"]]
            if labeled is None:
                debug_path = None
                if args.debug:
                    out_base = args.output or run.default_output(args.input)
                    debug_path = out_base.with_suffix(".raw.json")
                labeled = gemini_backend.transcribe_labeled(
                    media, settings.gemini_api_key, lang=args.lang,
                    timestamps=args.timestamps or args.diarize,
                    diarize=args.diarize, model=settings.gemini_model,
                    debug_path=debug_path,
                )
                if not args.no_cache:
                    audio_hash = cache_mod.sha256_file(media)
                    cache_mod.put_gemini(
                        cache, audio_hash, args.lang, settings.gemini_model, variant,
                        _format_labeled(labeled, args.diarize, args.timestamps), labeled,
                    )
            text = _format_labeled(labeled, args.diarize, args.timestamps)
        except Exception as e:
            print(f"transcription error: {e}", file=sys.stderr)
            return 1
        out = args.output or run.default_output(args.input)
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(text + "\n", encoding="utf-8")
        print(out)
        return 0
    finally:
        if cleanup is not None and not args.keep_audio:
            shutil.rmtree(cleanup, ignore_errors=True)
        if cleanup is not None and args.keep_audio:
            print(f"audio kept at: {cleanup}", file=sys.stderr)


def _run_openai(args: argparse.Namespace) -> int:
    from . import diarize as diar
    from . import openai_backend

    settings = get_settings()
    cache = CacheLayout(settings.cache_dir)
    run = RunLayout(Path.cwd())
    cleanup: Optional[Path] = None
    try:
        try:
            media, cleanup = _resolve_input(args.input, cache, not args.no_cache)
        except Exception as e:
            print(f"input error: {e}", file=sys.stderr)
            return 1
        try:
            if args.diarize:
                audio_hash = cache_mod.sha256_file(media)
                segments = _openai_segments(media, audio_hash, args, settings, cache)
                turns = _openai_turns(media, audio_hash, args, settings, cache)
                text = _format_labeled(diar.label_segments(segments, turns), True, False)
            else:
                text = openai_backend.transcribe_file(
                    media, settings.openai_api_key, lang=args.lang,
                    timestamps=args.timestamps, model=settings.openai_model,
                )
        except Exception as e:
            print(f"transcription error: {e}", file=sys.stderr)
            return 1
        out = args.output or run.default_output(args.input)
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(text + "\n", encoding="utf-8")
        print(out)
        return 0
    finally:
        if cleanup is not None and not args.keep_audio:
            shutil.rmtree(cleanup, ignore_errors=True)
        if cleanup is not None and args.keep_audio:
            print(f"audio kept at: {cleanup}", file=sys.stderr)


def main(argv: Optional[list[str]] = None) -> int:
    ap = argparse.ArgumentParser(prog="audio-transcribe", description="Transcribe audio/video with yapsnap (local), OpenAI whisper-1, or Gemini (cloud).")
    ap.add_argument("input", help="Local file path or URL.")
    ap.add_argument("-o", "--output", type=Path, default=None)
    ap.add_argument("--backend", choices=("openai", "yapsnap", "gemini"), default=DEFAULT_BACKEND)
    ap.add_argument("--lang", default="auto", help="'auto' or a language code (e.g. en, ru, uk).")
    ap.add_argument("--model", type=Path, default=None, help="yapsnap only: local transducer model dir.")
    ap.add_argument("--timestamps", action="store_true")
    ap.add_argument("--diarize", action="store_true", help="Label speakers. openai uses pyannote; yapsnap uses its native diarizer.")
    ap.add_argument("--num-speakers", type=int, default=-1, help="Known speaker count for --diarize (-1 = auto).")
    ap.add_argument("--keep-audio", action="store_true")
    ap.add_argument("--no-cache", action="store_true", help="Bypass the cache for this run.")
    ap.add_argument("--debug", action="store_true", help="Dump the raw Gemini response as .raw.json next to the output.")
    args = ap.parse_args(argv)

    if args.backend == "yapsnap":
        if args.model is None and args.lang.lower() not in ("auto", "de", "en", "es", "fr", "it", "iw", "nl", "pt", "sv", "tr"):
            print(f"note: yapsnap has no bundled model for '{args.lang}'; use --model DIR or a cloud backend", file=sys.stderr)
        return _run_yapsnap(args)
    if args.backend == "gemini":
        if args.model is not None:
            print("note: --model applies to yapsnap only; gemini ignores it", file=sys.stderr)
        return _run_gemini(args)
    if args.model is not None:
        print("note: --model applies to yapsnap only; openai ignores it", file=sys.stderr)
    return _run_openai(args)


if __name__ == "__main__":
    sys.exit(main())
