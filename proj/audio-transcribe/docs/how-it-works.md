# How audio-transcribe works

Concepts first, then pipeline diagrams.
All paths below come from `layout.py` instances.

## Concepts

**Backend** is the transcription engine.
`yapsnap` runs offline on your CPU and handles English only.
`openai` calls Whisper-1 in the cloud and handles any language.
`gemini` calls Gemini Transcribe in the cloud and diarizes natively, with no local GPU pass.

**Transcription** turns audio into text.
**Timestamps** attach a start time to each segment.
**Diarization** splits audio into speaker turns, with no text.
**Labeling** joins the two: each text segment takes the speaker active at its start time.

**Cache** has three stores under one root.
Downloads map URL to media file.
Transcripts map audio content hash plus lang plus model to segments.
Turns map audio content hash plus speaker count to speaker turns.
A repeat run reuses all three and makes no network or API calls.

**Settings** is the only module that reads env.
**Layout** computes every path from explicit roots.
**Cache** does I/O against layout paths.
Backends take secrets as arguments and read no env.

## Run flow

```mermaid
flowchart TD
    input[CLI input: file or URL] --> resolve[Resolve input]
    resolve -->|URL, cache miss| fetch[yt-dlp download]
    resolve -->|URL, cache hit| media[Cached media]
    resolve -->|Local file| media
    fetch --> store[Store in download cache]
    store --> media
    media --> backend{Backend}
    backend -->|yapsnap| local[Local transducer]
    backend -->|openai| cloud[Whisper-1 API]
    backend -->|gemini| gem[Gemini Transcribe API]
    local --> text[Plain or timestamped text]
    cloud --> text
    gem --> text
    text --> out[Write transcript file]
```

## OpenAI with diarization

```mermaid
flowchart TD
    media[Media file] --> hash[SHA-256 of audio bytes]
    hash --> tkey[Transcript lookup]
    tkey -->|miss| whisper[Whisper verbose JSON]
    whisper --> seg[Segments: start plus text]
    tkey -->|hit| seg
    hash --> dkey[Turns lookup]
    dkey -->|miss| wav[ffmpeg to 16k mono wav]
    wav --> pyannote[pyannote pipeline]
    pyannote --> turns[Turns: start, end, speaker]
    dkey -->|hit| turns
    seg --> label[Label each segment by speaker at start]
    turns --> label
    label --> lines[SPEAKER lines with timestamps]
```

## Gemini with native diarization

```mermaid
flowchart TD
    media2[Media file] --> hash2[SHA-256 of audio bytes]
    hash2 --> gkey[Gemini cache lookup]
    gkey -->|miss| budget{Over 45 minutes}
    budget -->|yes| chunks[Split to 45-minute mp3 parts]
    budget -->|no| mkv{MKV container}
    chunks --> upload[Files API upload per part]
    mkv -->|yes| wav2[ffmpeg to wav]
    mkv -->|no| upload
    wav2 --> upload
    upload --> gen[Generate with diarization config]
    gen --> parts[Parts with speaker labels plus word times]
    parts --> del[Delete remote file]
    del --> store2[Store labeled segments]
    gkey -->|hit| store2
    store2 --> lines2[SPEAKER lines with timestamps]
```

## Module dependencies

```mermaid
flowchart TD
    cli[CLI] --> settings[Settings]
    cli --> runlayout[RunLayout]
    cli --> cache[Cache]
    cache --> layout[Layout]
    cli --> backend[Backend]
    backend --> settings
    setup[Setup] --> settings
    setup --> projectlayout[ProjectLayout]
    tests[Test suite] --> layout
```

## Diarization loading chain

```mermaid
flowchart TD
    token[HF token from settings] --> patch[Patch hub flag name]
    patch --> pipe[Load pipeline config]
    pipe --> segmodel[Load segmentation model]
    segmodel --> embmodel[Load embedding model]
    embmodel --> guard[Allowlist checkpoint types]
    guard --> ready[Pipeline ready]
    ready --> wav2[Transcode input to wav]
    wav2 --> infer[Run inference]
    infer --> turns2[Sorted speaker turns]
```
