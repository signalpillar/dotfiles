"""Host and install filesystem locations for pi-job.

Task-bundle path arithmetic stays on `YamlTaskLayout` / `BundleTaskLayout`.
Mailbox path arithmetic stays on `MailboxPaths`.
This layout owns XDG dirs, `PI_JOB_*` homes, shipped `profile.yaml`, overlay, and lock files.
"""

from __future__ import annotations

import hashlib
import os
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path

from pi_job_harness.errors import die

PACKAGE_DIR = Path(__file__).resolve().parent


@dataclass(frozen=True)
class PiJobLayout:
    """Resolved locations for one environment mapping and one install package dir."""

    APP_NAME = "pi-job"
    PROFILE_OVERLAY_ENV = "PI_JOB_PROFILE_OVERLAY"
    TASKS_ENV = "PI_JOB_TASKS"
    ARCHIVE_ENV = "PI_JOB_ARCHIVE"
    WORKTREES_ENV = "PI_JOB_WORKTREES"
    DEFAULT_DATA_HOME = "~/.local/share/pi-job"

    # Process or test environment (`PI_JOB_*`, `XDG_*`).
    env: Mapping[str, str]
    # Install package directory (`pi_job_harness/`); locates shipped `profile.yaml`.
    package_dir: Path

    @classmethod
    def from_environ(
        cls,
        env: Mapping[str, str] | None = None,
        *,
        package_dir: Path | None = None,
    ) -> PiJobLayout:
        return cls(
            env=os.environ if env is None else env,
            package_dir=package_dir or PACKAGE_DIR,
        )

    def _expand(self, raw: str) -> Path:
        return Path(raw).expanduser().resolve()

    @property
    def xdg_config_home(self) -> Path:
        raw = self.env.get("XDG_CONFIG_HOME")
        if raw:
            return Path(raw)
        return Path.home() / ".config"

    @property
    def xdg_cache_home(self) -> Path:
        raw = self.env.get("XDG_CACHE_HOME")
        if raw:
            return Path(raw)
        return Path.home() / ".cache"

    @property
    def config_dir(self) -> Path:
        return self.xdg_config_home / self.APP_NAME

    @property
    def cache_dir(self) -> Path:
        return self.xdg_cache_home / self.APP_NAME

    @property
    def locks_dir(self) -> Path:
        return self.cache_dir / "locks"

    @property
    def data_home(self) -> Path:
        return self._expand(self.DEFAULT_DATA_HOME)

    @property
    def tasks_home(self) -> Path:
        raw = self.env.get(self.TASKS_ENV, f"{self.DEFAULT_DATA_HOME}/tasks")
        return self._expand(raw)

    @property
    def archive_home(self) -> Path:
        raw = self.env.get(self.ARCHIVE_ENV)
        if raw:
            return self._expand(raw)
        return self.tasks_home.parent / "archive"

    @property
    def worktrees_home(self) -> Path:
        raw = self.env.get(self.WORKTREES_ENV, f"{self.DEFAULT_DATA_HOME}/worktrees")
        return self._expand(raw)

    @property
    def profile_yaml(self) -> Path:
        tree = self.package_dir.parent / "profile.yaml"
        if tree.is_file():
            return tree
        packaged = self.package_dir / "profile.yaml"
        if packaged.is_file():
            return packaged
        die(f"execution profile not found: looked for {tree} and {packaged}")

    @property
    def default_profile_overlay_yaml(self) -> Path:
        return self.config_dir / "profile.overlay.yaml"

    def profile_overlay_to_load(self) -> Path | None:
        """Return the overlay file to merge, or None when overlay is off or absent.

        Empty PI_JOB_PROFILE_OVERLAY disables overlay (does not read the default path).
        A set path that exists and is not a regular file fails closed.
        A missing file is a no-op.
        """

        if self.PROFILE_OVERLAY_ENV in self.env:
            raw = self.env[self.PROFILE_OVERLAY_ENV]
            if raw == "":
                return None
            path = Path(raw).expanduser()
            if path.exists() and not path.is_file():
                die(f"profile overlay is not a file: {path}")
            if not path.is_file():
                return None
            return path
        path = self.default_profile_overlay_yaml
        if not path.is_file():
            return None
        return path

    def yaml_task_lock_path(self, task_path: Path) -> Path:
        digest = hashlib.sha256(os.fsencode(task_path.resolve())).hexdigest()
        return self.locks_dir / f"{digest}.lock"

    def worktree_path(self, *, slug: str | None, slice_key: str, repo: str) -> Path:
        parts: list[Path | str] = [self.worktrees_home]
        if slug:
            parts.append(slug)
        parts.append(slice_key)
        parts.append(repo)
        return Path(*parts)


def pi_job_layout(
    env: Mapping[str, str] | None = None,
    *,
    package_dir: Path | None = None,
) -> PiJobLayout:
    return PiJobLayout.from_environ(env, package_dir=package_dir)
