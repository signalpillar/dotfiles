"""Process-exit helpers shared by CLI and stores."""

from __future__ import annotations

import sys
from typing import NoReturn


def die(message: str) -> NoReturn:
    print(f"error: {message}", file=sys.stderr)
    sys.exit(1)

