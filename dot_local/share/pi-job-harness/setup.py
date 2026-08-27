"""Build helpers for pi-job-harness.

Copies the tree-root profile.yaml into the wheel so a non-editable install
can load the contract without a second source file.
"""

from __future__ import annotations

from pathlib import Path

from setuptools import setup
from setuptools.command.build_py import build_py as _build_py

PROFILE_SRC = Path(__file__).resolve().parent / "profile.yaml"


def copy_profile_into_build_lib(build_lib: str | Path) -> Path:
    dest = Path(build_lib) / "pi_job_harness" / "profile.yaml"
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_bytes(PROFILE_SRC.read_bytes())
    return dest


class build_py(_build_py):
    def run(self) -> None:
        super().run()
        copy_profile_into_build_lib(self.build_lib)


setup(cmdclass={"build_py": build_py})
