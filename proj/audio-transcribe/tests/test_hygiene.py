"""Env hygiene: config.py is the only module allowed to read process env."""

import re
from pathlib import Path

SRC = Path(__file__).resolve().parents[1] / "src" / "audio_transcribe"
BANNED = re.compile(r"os\.environ|os\.getenv|getenv\(|load_dotenv|dotenv_values")


def test_only_config_reads_env():
    offenders = []
    for path in sorted(SRC.glob("*.py")):
        if path.name == "config.py":
            continue
        for lineno, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
            if BANNED.search(line):
                offenders.append(f"{path.name}:{lineno}: {line.strip()}")
    assert not offenders, "env access outside config.py:\n" + "\n".join(offenders)
