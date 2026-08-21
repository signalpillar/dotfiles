"""Directory-backed TaskLayout and experimental FsTaskStore."""

from __future__ import annotations

import os
import shutil
from collections.abc import Mapping
from functools import lru_cache
from pathlib import Path
from typing import Any
from uuid import uuid4

from pi_job_harness.errors import die
from pi_job_harness.store.yaml_io import validate_task_mapping
from pi_job_harness.task import ARTIFACT_STATUSES, PULL_REQUEST_STATUSES, TASK_STATUSES

_PREFIX_WIDTH = 4
_PREFIX_GAP = 10


def _prefix_of(name: str) -> int:
    return int(name.split("-", 1)[0])


def _format_prefix(n: int) -> str:
    return str(n).zfill(_PREFIX_WIDTH)


class TaskLayout:
    """Computes filesystem paths for a directory-backed task. Pure path
    arithmetic plus the directory listing needed to resolve keys/order -
    no file reads/writes here; FsTaskStore owns all I/O."""

    def __init__(self, base: Path) -> None:
        self.base = base

    # ---- top-level scalar/record files ----
    def title_file(self) -> Path: return self.base / "title"
    def status_file(self) -> Path: return self.base / "status"
    def source_file(self) -> Path: return self.base / "source"
    def project_file(self) -> Path: return self.base / "project"
    def context_file(self) -> Path: return self.base / "context"

    # ---- orchestration ----
    def orchestration_dir(self) -> Path: return self.base / "orchestration"
    def policy_file(self) -> Path: return self.orchestration_dir() / "policy"
    def artifacts_dir(self) -> Path: return self.orchestration_dir() / "artifacts"

    def artifact_dir(self, key: str) -> Path:
        return self.artifacts_dir() / key  # map: stable key, no ordering needed

    def artifact_keys(self) -> list[str]:
        """Existing artifact keys (unordered map: dir names, no .order file)."""
        d = self.artifacts_dir()
        if not d.is_dir():
            return []
        return sorted(p.name for p in d.iterdir() if p.is_dir())

    def artifact_status_file(self, key: str) -> Path:
        return self.artifact_dir(key) / "status"

    def artifact_path_file(self, key: str) -> Path:
        return self.artifact_dir(key) / "path"

    def artifact_note_file(self, key: str) -> Path:
        return self.artifact_dir(key) / "note"

    # ---- decisions: ordered, no natural key, nothing external references them ----
    def decisions_dir(self) -> Path: return self.base / "decisions"

    def decision_dirs(self) -> list[Path]:
        return self._numbered_children(self.decisions_dir())

    def new_decision_dir(self) -> Path:
        return self._append_numbered(self.decisions_dir())

    def decision_date_file(self, decision_dir: Path) -> Path:
        return decision_dir / "date"

    def decision_note_file(self, decision_dir: Path) -> Path:
        return decision_dir / "note"

    def decision_source_file(self, decision_dir: Path) -> Path:
        return decision_dir / "source"

    def findings_file(self) -> Path:
        """Append-only RCA/evidence log for directory-backed tasks."""
        return self.base / "findings.md"

    # ---- plan ----
    def plan_note_file(self) -> Path: return self.base / "plan" / "note"
    def slices_dir(self) -> Path: return self.base / "plan" / "slices"
    def slices_order_file(self) -> Path: return self.slices_dir() / ".order"

    def slice_dir(self, key: str) -> Path:
        """Stable: dirname *is* the key, so depends_on symlinks never dangle."""
        return self.slices_dir() / key

    def slice_dirs(self) -> list[Path]:
        """Existing slices in .order sequence (lexical fallback if .order is missing)."""
        order_file = self.slices_order_file()
        if order_file.exists():
            keys = [ln.strip() for ln in order_file.read_text().splitlines() if ln.strip()]
            return [self.slice_dir(k) for k in keys if self.slice_dir(k).is_dir()]
        return sorted(p for p in self.slices_dir().iterdir() if p.is_dir())

    # ---- per-slice fields ----
    def slice_title_file(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "title"

    def slice_goal_file(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "goal"

    def slice_status_file(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "status"

    def slice_note_file(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "note"

    def slice_kind_file(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "kind"

    def slice_execution_file(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "execution"

    def slice_repos_file(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "repos"

    def slice_depends_on_dir(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "depends_on"

    def slice_dependency_link(self, slice_key: str, depends_on_key: str) -> Path:
        return self.slice_depends_on_dir(slice_key) / depends_on_key

    def slice_dependency_keys(self, slice_key: str) -> list[str]:
        """Depended-on slice keys, taken from the depends_on symlink dir's entry NAMES
        (not resolved link targets) - absent/empty dir means no dependencies."""
        d = self.slice_depends_on_dir(slice_key)
        if not d.is_dir():
            return []
        return sorted(p.name for p in d.iterdir())

    def slice_repo_work_root(self, slice_key: str) -> Path:
        return self.slice_dir(slice_key) / "repo_work"

    def slice_repo_names(self, slice_key: str) -> list[str]:
        """Existing repo_work repo names (unordered map) - absent/empty dir means none."""
        root = self.slice_repo_work_root(slice_key)
        if not root.is_dir():
            return []
        return sorted(p.name for p in root.iterdir() if p.is_dir())

    def slice_repo_work_dir(self, slice_key: str, repo: str) -> Path:
        return self.slice_dir(slice_key) / "repo_work" / repo

    def slice_repo_worktree_file(self, slice_key: str, repo: str) -> Path:
        return self.slice_repo_work_dir(slice_key, repo) / "worktree"

    def slice_repo_prs_dir(self, slice_key: str, repo: str) -> Path:
        return self.slice_repo_work_dir(slice_key, repo) / "prs"

    def slice_pr_dirs(self, slice_key: str, repo: str) -> list[Path]:
        return self._numbered_children(self.slice_repo_prs_dir(slice_key, repo))

    def new_pr_dir(self, slice_key: str, repo: str) -> Path:
        return self._append_numbered(self.slice_repo_prs_dir(slice_key, repo))

    def pr_url_file(self, pr_dir: Path) -> Path:
        return pr_dir / "url"

    def pr_status_file(self, pr_dir: Path) -> Path:
        return pr_dir / "status"

    def pr_note_file(self, pr_dir: Path) -> Path:
        return pr_dir / "note"

    # ---- steps / final_steps: ordered, keyed, nothing external references them ----
    def steps_dir(self, slice_key: str, *, terminal: bool) -> Path:
        return self.slice_dir(slice_key) / ("final_steps" if terminal else "steps")

    def step_dirs(self, slice_key: str, *, terminal: bool) -> list[Path]:
        return self._numbered_children(self.steps_dir(slice_key, terminal=terminal))

    def step_dir(self, slice_key: str, step_key: str, *, terminal: bool) -> Path:
        for d in self.step_dirs(slice_key, terminal=terminal):
            if d.name.split("-", 1)[-1] == step_key:
                return d
        raise FileNotFoundError(f"step {step_key!r} not found under {self.steps_dir(slice_key, terminal=terminal)}")

    def new_step_dir(self, slice_key: str, step_key: str, *, terminal: bool, after: str | None) -> Path:
        parent = self.steps_dir(slice_key, terminal=terminal)
        if after is None:
            return self._append_numbered(parent, suffix=step_key)
        anchor = self.step_dir(slice_key, after, terminal=terminal)
        return self._insert_numbered_after(parent, anchor, suffix=step_key)

    def step_title_file(self, step_dir: Path) -> Path:
        return step_dir / "title"

    def step_status_file(self, step_dir: Path) -> Path:
        return step_dir / "status"

    def step_note_file(self, step_dir: Path) -> Path:
        return step_dir / "note"

    def step_execution_file(self, step_dir: Path) -> Path:
        return step_dir / "execution"

    # ---- private: gapped numeric-prefix ordering, shared by decisions/prs/steps ----
    def _numbered_children(self, parent: Path) -> list[Path]:
        if not parent.is_dir():
            return []
        return sorted((p for p in parent.iterdir() if p.is_dir()), key=lambda p: _prefix_of(p.name))

    def _append_numbered(self, parent: Path, *, suffix: str | None = None) -> Path:
        existing = self._numbered_children(parent)
        next_prefix = _prefix_of(existing[-1].name) + _PREFIX_GAP if existing else _PREFIX_GAP
        name = _format_prefix(next_prefix) + (f"-{suffix}" if suffix else "")
        return parent / name

    def _insert_numbered_after(self, parent: Path, anchor: Path, *, suffix: str | None = None) -> Path:
        existing = self._numbered_children(parent)
        idx = existing.index(anchor)
        before = _prefix_of(anchor.name)
        after = _prefix_of(existing[idx + 1].name) if idx + 1 < len(existing) else before + _PREFIX_GAP
        mid = (before + after) // 2
        if mid == before:
            raise RuntimeError(f"no numbering room between {before} and {after} under {parent}; re-space required")
        name = _format_prefix(mid) + (f"-{suffix}" if suffix else "")
        return parent / name


@lru_cache(maxsize=1)
def _fs_valid_statuses() -> tuple[str, ...]:
    return TASK_STATUSES


@lru_cache(maxsize=1)
def _fs_valid_artifact_statuses() -> tuple[str, ...]:
    return ARTIFACT_STATUSES


@lru_cache(maxsize=1)
def _fs_valid_pr_statuses() -> tuple[str, ...]:
    return PULL_REQUEST_STATUSES


class FsTaskStore:
    """Experimental directory-backed TaskStore ("everything is a file", /proc-flavored).
    Every real filesystem path is resolved via self.layout (a TaskLayout); this class only
    does file I/O (read/write/mkdir/symlink/rename) on paths it is handed, plus building
    `.tmp-*` staging names as siblings of a layout-resolved path for atomic create/replace
    operations - the one place it names a path segment itself."""

    def __init__(self, base: Path, layout: TaskLayout | None = None) -> None:
        self.base = base
        self.layout = layout or TaskLayout(base)

    def describe(self) -> str:
        return f"directory task store at {self.base}"

    # ---- low-level file/record helpers (content parsing/serialization only) ----

    def _atomic_write_text(self, path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        tmp = path.parent / f".{path.name}.tmp-{uuid4().hex}"
        tmp.write_text(content)
        os.replace(tmp, path)

    def _write_text(self, path: Path, value: str) -> None:
        self._atomic_write_text(path, (value or "") + "\n")

    def _read_text(self, path: Path) -> str:
        if not path.exists():
            return ""
        text = path.read_text()
        return text.removesuffix("\n")

    def _read_optional_text(self, path: Path) -> str | None:
        if not path.exists():
            return None
        return self._read_text(path)

    def _read_lines(self, path: Path) -> list[str]:
        if not path.exists():
            return []
        return [ln.strip() for ln in path.read_text().splitlines() if ln.strip()]

    def _read_record(self, path: Path) -> dict[str, str]:
        """Parse a 'key: value' per-line record file. Single-line values only - a known,
        deliberate limitation of this experimental backend (no embedded-newline support)."""
        if not path.exists():
            return {}
        record: dict[str, str] = {}
        for line in path.read_text().splitlines():
            if not line.strip():
                continue
            if ":" not in line:
                die(f"malformed record line in {path}: {line!r}")
            key, _, value = line.partition(":")
            record[key.strip()] = value.strip()
        return record

    def _write_record(self, path: Path, fields: Mapping[str, str | None]) -> None:
        content = "".join(f"{k}: {v}\n" for k, v in fields.items() if v is not None)
        self._atomic_write_text(path, content)

    def _read_bool_field(self, value: str | None, field_desc: str) -> bool:
        if value == "true":
            return True
        if value == "false":
            return False
        die(f"{field_desc}: expected literal 'true' or 'false', got {value!r}")

    def _read_status(self, path: Path, field_desc: str) -> str:
        value = self._read_text(path)
        valid = _fs_valid_statuses()
        if value not in valid:
            die(f"{field_desc}: invalid status {value!r} in {path}; expected one of: {', '.join(valid)}")
        return value

    def _read_artifact_status(self, path: Path, field_desc: str) -> str:
        value = self._read_text(path)
        valid = _fs_valid_artifact_statuses()
        if value not in valid:
            die(f"{field_desc}: invalid artifact status {value!r} in {path}; expected one of: {', '.join(valid)}")
        return value

    def _read_pr_status(self, path: Path, field_desc: str) -> str:
        value = self._read_text(path)
        valid = _fs_valid_pr_statuses()
        if value not in valid:
            die(f"{field_desc}: invalid PR status {value!r} in {path}; expected one of: {', '.join(valid)}")
        return value

    # ---- read() ----

    def read(self) -> dict[str, Any]:
        L = self.layout
        result: dict[str, Any] = {
            "title": self._read_text(L.title_file()),
            "status": self._read_status(L.status_file(), "task.status"),
            "source": self._read_record(L.source_file()),
            "project": self._read_record(L.project_file()),
            "context": self._read_text(L.context_file()),
            "decisions": self._read_decisions(),
            "plan": self._read_plan(),
        }
        if L.orchestration_dir().is_dir():
            result["orchestration"] = self._read_orchestration()
        validate_task_mapping(result, source=str(self.base))
        return result

    def _read_decisions(self) -> list[dict[str, Any]]:
        L = self.layout
        return [
            {
                "date": self._read_text(L.decision_date_file(d)),
                "note": self._read_text(L.decision_note_file(d)),
                "source": self._read_text(L.decision_source_file(d)),
            }
            for d in L.decision_dirs()
        ]

    def _read_orchestration(self) -> dict[str, Any]:
        L = self.layout
        policy_rec = self._read_record(L.policy_file())
        artifacts: dict[str, Any] = {}
        for key in L.artifact_keys():
            entry: dict[str, Any] = {
                "status": self._read_artifact_status(
                    L.artifact_status_file(key),
                    f"orchestration.artifacts[{key}].status",
                ),
            }
            path_val = self._read_optional_text(L.artifact_path_file(key))
            if path_val is not None:
                entry["path"] = path_val
            entry["note"] = self._read_text(L.artifact_note_file(key))
            artifacts[key] = entry
        result: dict[str, Any] = {
            # FsTaskStore is an experimental read/write backend and does not support the
            # owned-cursor claim layer (Yaml-only, like block-slice/add-finding); always empty.
            "cursors": [],
            "policy": {
                "coding_execution": {
                    "subagent_required": self._read_bool_field(
                        policy_rec.get("subagent_required"), "policy.subagent_required"
                    ),
                    "lower_power_model_preferred": self._read_bool_field(
                        policy_rec.get("lower_power_model_preferred"), "policy.lower_power_model_preferred"
                    ),
                    "orchestrator_reviews_subagent": self._read_bool_field(
                        policy_rec.get("orchestrator_reviews_subagent"), "policy.orchestrator_reviews_subagent"
                    ),
                }
            },
            "artifacts": artifacts,
        }
        return result

    def _read_plan(self) -> dict[str, Any]:
        L = self.layout
        return {
            "note": self._read_text(L.plan_note_file()),
            "slices": [self._read_slice(d) for d in L.slice_dirs()],
        }

    def _read_slice(self, slice_dir: Path) -> dict[str, Any]:
        L = self.layout
        key = slice_dir.name
        entry: dict[str, Any] = {
            "key": key,
            "kind": self._read_text(L.slice_kind_file(key)),
            "title": self._read_text(L.slice_title_file(key)),
            "goal": self._read_text(L.slice_goal_file(key)),
            "status": self._read_status(L.slice_status_file(key), f"plan.slices[{key}].status"),
            "note": self._read_text(L.slice_note_file(key)),
        }
        execution = self._read_record(L.slice_execution_file(key))
        if execution:
            entry["execution"] = execution
        repos = self._read_lines(L.slice_repos_file(key))
        if repos:
            entry["repos"] = repos
        deps = L.slice_dependency_keys(key)
        if deps:
            entry["depends_on"] = deps
        layer_val = self._read_optional_text(L.slice_dir(key) / "layer")
        if layer_val:
            entry["layer"] = layer_val
        repo_work = self._read_repo_work(key)
        if repo_work:
            entry["repo_work"] = repo_work
        entry["steps"] = [self._read_step(d) for d in L.step_dirs(key, terminal=False)]
        entry["final_steps"] = [self._read_step(d) for d in L.step_dirs(key, terminal=True)]
        return entry

    def _read_repo_work(self, slice_key: str) -> dict[str, Any]:
        L = self.layout
        result: dict[str, Any] = {}
        for repo in L.slice_repo_names(slice_key):
            entry: dict[str, Any] = {}
            wt = self._read_optional_text(L.slice_repo_worktree_file(slice_key, repo))
            if wt is not None:
                entry["worktree"] = wt
            pr_dirs = L.slice_pr_dirs(slice_key, repo)
            if pr_dirs:
                entry["prs"] = [self._read_pr(d) for d in pr_dirs]
            result[repo] = entry
        return result

    def _read_pr(self, pr_dir: Path) -> dict[str, Any]:
        L = self.layout
        return {
            "url": self._read_text(L.pr_url_file(pr_dir)),
            "status": self._read_pr_status(L.pr_status_file(pr_dir), f"pr[{pr_dir.name}].status"),
            "note": self._read_text(L.pr_note_file(pr_dir)),
        }

    def _read_step(self, step_dir: Path) -> dict[str, Any]:
        L = self.layout
        key = step_dir.name.split("-", 1)[-1]
        entry: dict[str, Any] = {
            "key": key,
            "title": self._read_text(L.step_title_file(step_dir)),
            "status": self._read_status(L.step_status_file(step_dir), f"step[{key}].status"),
            "note": self._read_text(L.step_note_file(step_dir)),
        }
        execution = self._read_record(L.step_execution_file(step_dir))
        if execution:
            entry["execution"] = execution
        return entry

    # ---- mutations ----

    def init_orchestration(self) -> None:
        L = self.layout
        L.orchestration_dir().mkdir(parents=True, exist_ok=True)
        self._write_record(
            L.policy_file(),
            {
                "subagent_required": "true",
                "lower_power_model_preferred": "true",
                "orchestrator_reviews_subagent": "true",
            },
        )

    def init_task(
        self,
        *,
        title: str,
        status: str,
        source: dict[str, str],
        project_info: dict[str, str],
        context: str,
    ) -> None:
        L = self.layout
        self._write_text(L.title_file(), title)
        self._write_text(L.status_file(), status)
        self._write_record(L.source_file(), dict(source))
        self._write_record(L.project_file(), dict(project_info))
        self._write_text(L.context_file(), context)

    def set_plan_note(self, note: str) -> None:
        self._write_text(self.layout.plan_note_file(), note)

    def add_decision(self, *, date: str, note: str, source: str) -> None:
        target = self.layout.new_decision_dir()
        staging = target.parent / f".tmp-{uuid4().hex}"
        staging.mkdir(parents=True)
        try:
            (staging / "date").write_text(date + "\n")
            (staging / "note").write_text((note or "") + "\n")
            (staging / "source").write_text(source + "\n")
            os.rename(staging, target)
        except Exception:
            shutil.rmtree(staging, ignore_errors=True)
            raise

    def add_slice(
        self,
        *,
        key: str,
        kind: str,
        title: str,
        goal: str,
        extra_fields: dict[str, list[str]],
        steps: list[tuple[str, str]],
        final_steps: list[tuple[str, str]],
        after: str | None,
        status: str = "planned",
        note: str = "",
        layer: str | None = None,
    ) -> None:
        L = self.layout

        repos: list[str] = []
        depends_on: list[str] = []
        for field_name, values in extra_fields.items():
            if field_name == "repos":
                repos = values
            elif field_name == "depends_on":
                depends_on = values
            else:
                die(
                    f"FsTaskStore.add_slice: unsupported extra field {field_name!r}; "
                    "only 'repos'/'depends_on' are supported by this backend"
                )

        # 1. Stage the slice's own subtree (everything but depends_on, which needs the
        #    slice's real post-rename path) and rename it into place as the LAST step.
        staging = L.slices_dir() / f".tmp-{uuid4().hex}"
        staging.mkdir(parents=True)
        try:
            (staging / "kind").write_text(kind + "\n")
            (staging / "title").write_text(title + "\n")
            (staging / "goal").write_text(goal + "\n")
            (staging / "status").write_text(status + "\n")
            (staging / "note").write_text((note or "") + "\n")
            if layer:
                (staging / "layer").write_text(layer + "\n")
            if repos:
                (staging / "repos").write_text("".join(f"{r}\n" for r in repos))
            if steps:
                steps_dir = staging / "steps"
                steps_dir.mkdir()
                for i, (step_key, step_title) in enumerate(steps, start=1):
                    step_dir = steps_dir / f"{_format_prefix(_PREFIX_GAP * i)}-{step_key}"
                    step_dir.mkdir()
                    (step_dir / "title").write_text(step_title + "\n")
                    (step_dir / "status").write_text("planned\n")
                    (step_dir / "note").write_text("\n")
            if final_steps:
                final_steps_dir = staging / "final_steps"
                final_steps_dir.mkdir()
                for i, (step_key, step_title) in enumerate(final_steps, start=1):
                    step_dir = final_steps_dir / f"{_format_prefix(_PREFIX_GAP * i)}-{step_key}"
                    step_dir.mkdir()
                    (step_dir / "title").write_text(step_title + "\n")
                    (step_dir / "status").write_text("planned\n")
                    (step_dir / "note").write_text("\n")
            os.rename(staging, L.slice_dir(key))
        except Exception:
            shutil.rmtree(staging, ignore_errors=True)
            raise

        # 2. depends_on symlinks, now that the slice's real layout-resolved path exists.
        if depends_on:
            L.slice_depends_on_dir(key).mkdir(parents=True, exist_ok=True)
            for dep_key in depends_on:
                L.slice_dependency_link(key, dep_key).symlink_to(f"../../{dep_key}")

        # 3. .order, as a separate final atomic write.
        self._insert_slice_key_in_order(key, after)

    def _insert_slice_key_in_order(self, key: str, after: str | None) -> None:
        order_file = self.layout.slices_order_file()
        existing = (
            [ln.strip() for ln in order_file.read_text().splitlines() if ln.strip()]
            if order_file.exists()
            else []
        )
        if after is not None:
            if after not in existing:
                die(f"--after slice {after!r} not found in slices order (unexpected)")
            existing.insert(existing.index(after) + 1, key)
        else:
            existing.append(key)
        self._atomic_write_text(order_file, "".join(f"{k}\n" for k in existing))

    def add_step(
        self,
        *,
        slice_key: str,
        key: str,
        title: str,
        note: str,
        terminal: bool,
        after: str | None,
        status: str = "planned",
    ) -> None:
        target = self.layout.new_step_dir(slice_key, key, terminal=terminal, after=after)
        staging = target.parent / f".tmp-{uuid4().hex}"
        staging.mkdir(parents=True)
        try:
            (staging / "title").write_text(title + "\n")
            (staging / "status").write_text(status + "\n")
            (staging / "note").write_text((note or "") + "\n")
            os.rename(staging, target)
        except Exception:
            shutil.rmtree(staging, ignore_errors=True)
            raise

    def set_step_status(
        self, *, slice_key: str, step_key: str, status: str, note: str
    ) -> None:
        L = self.layout
        for terminal in (False, True):
            for step_dir in L.step_dirs(slice_key, terminal=terminal):
                key = step_dir.name.split("-", 1)[-1]
                if key == step_key:
                    self._write_text(L.step_status_file(step_dir), status)
                    self._write_text(L.step_note_file(step_dir), note)
                    return
        die(f"could not find step {step_key!r} in slice {slice_key!r} to update status")

    def set_execution(
        self,
        *,
        slice_key: str,
        step_key: str | None,
        status: str,
        note: str,
        execution: dict[str, str],
    ) -> None:
        L = self.layout
        if step_key is None:
            self._write_text(L.slice_status_file(slice_key), status)
            self._write_text(L.slice_note_file(slice_key), note)
            self._write_record(L.slice_execution_file(slice_key), execution)
            return
        for terminal in (False, True):
            for step_dir in L.step_dirs(slice_key, terminal=terminal):
                if step_dir.name.split("-", 1)[-1] != step_key:
                    continue
                self._write_text(L.step_status_file(step_dir), status)
                self._write_text(L.step_note_file(step_dir), note)
                self._write_record(L.step_execution_file(step_dir), execution)
                return
        die(f"could not find step {step_key!r} in slice {slice_key!r} to update execution")

    def set_worktree(self, *, slice_key: str, repo: str, path: str) -> None:
        self._write_text(self.layout.slice_repo_worktree_file(slice_key, repo), path)

    def clear_worktree(self, *, slice_key: str, repo: str) -> None:
        L = self.layout
        repo_dir = L.slice_repo_work_dir(slice_key, repo)
        if not repo_dir.is_dir():
            die(f"repo work not found: {slice_key}/{repo}")
        worktree_file = L.slice_repo_worktree_file(slice_key, repo)
        if worktree_file.exists():
            worktree_file.unlink()

    def add_pr(self, *, slice_key: str, repo: str, url: str, status: str, note: str) -> str:
        L = self.layout
        for pr_dir in L.slice_pr_dirs(slice_key, repo):
            if self._read_text(L.pr_url_file(pr_dir)) == url:
                self._write_text(L.pr_status_file(pr_dir), status)
                self._write_text(L.pr_note_file(pr_dir), note)
                return "updated"

        target = L.new_pr_dir(slice_key, repo)
        staging = target.parent / f".tmp-{uuid4().hex}"
        staging.mkdir(parents=True)
        try:
            (staging / "url").write_text(url + "\n")
            (staging / "status").write_text(status + "\n")
            (staging / "note").write_text((note or "") + "\n")
            os.rename(staging, target)
        except Exception:
            shutil.rmtree(staging, ignore_errors=True)
            raise
        return "added"

    def write_artifact(self, key: str, *, status: str, path: str | None, note: str) -> None:
        L = self.layout
        self._write_text(L.artifact_status_file(key), status)
        if path is not None:
            self._write_text(L.artifact_path_file(key), path)
        else:
            path_file = L.artifact_path_file(key)
            if path_file.exists():
                path_file.unlink()
        self._write_text(L.artifact_note_file(key), note or "")

    def set_project(self, fields: Mapping[str, str]) -> None:
        L = self.layout
        existing = self._read_record(L.project_file())
        existing.update(fields)
        self._write_record(L.project_file(), existing)

    def set_title(self, title: str) -> None:
        cleaned = title.strip()
        if not cleaned:
            die("title must be non-empty")
        self._write_text(self.layout.title_file(), cleaned)

    def set_context(self, context: str) -> None:
        self._write_text(self.layout.context_file(), context)

    def remove_slice(self, *, key: str) -> None:
        L = self.layout
        slice_dir = L.slice_dir(key)
        if not slice_dir.is_dir():
            die(f"slice not found: {key!r}")
        shutil.rmtree(slice_dir)
        order_file = L.slices_order_file()
        if order_file.exists():
            keys = [ln.strip() for ln in order_file.read_text().splitlines() if ln.strip()]
            if key in keys:
                keys.remove(key)
                self._atomic_write_text(order_file, "".join(f"{k}\n" for k in keys))

    def set_slice_fields(
        self, *, slice_key: str, title: str | None = None, goal: str | None = None
    ) -> None:
        raise NotImplementedError(
            "directory task storage does not support set-slice; migrate to YAML with "
            "`pi-job --task <dir> project --to <file>.yaml`"
        )

    def set_slice_status(
        self, *, slice_key: str, status: str, note: str | None = None
    ) -> None:
        raise NotImplementedError(
            "directory task storage does not support slice status mutations; migrate to YAML with "
            "`pi-job --task <dir> project --to <file>.yaml`"
        )

    def acknowledge_edit(self, *, reason: str, slice_key: str) -> None:
        raise NotImplementedError(
            "directory task storage does not support acknowledge-edit; migrate to YAML with "
            "`pi-job --task <dir> project --to <file>.yaml`"
        )
