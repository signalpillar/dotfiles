"""Shared create-contract validation for goals, slice kinds, and project routes."""

from __future__ import annotations

import difflib
import re
from pathlib import Path

from pi_job_harness.errors import die

BOILERPLATE_GOAL_RE = re.compile(r"^Initial .+ slice seeded by pi-job create$")
EXAMPLE_SLICE_GOAL = "Ship the bounded edit with verification."


def is_boilerplate_create_goal(text: str) -> bool:
    stripped = text.strip()
    if not stripped:
        return True
    if BOILERPLATE_GOAL_RE.match(stripped):
        return True
    return stripped == EXAMPLE_SLICE_GOAL


def normalize_real_goal(text: str, *, label: str = "goal") -> str:
    stripped = text.strip()
    if not stripped:
        raise ValueError(f"{label} is required and must be non-empty")
    if is_boilerplate_create_goal(stripped):
        raise ValueError(f"{label} must be a real outcome, not boilerplate seeded text")
    return stripped


def validate_real_goal(text: str, *, label: str = "goal") -> str:
    try:
        return normalize_real_goal(text, label=label)
    except ValueError as exc:
        die(str(exc))


def known_slice_kind_keys() -> frozenset[str]:
    from pi_job_harness.profile import load_profile_contract

    return frozenset(load_profile_contract().get("slice_kinds", {}).keys())


def normalize_slice_kind_key(kind: str, *, label: str = "kind") -> str:
    stripped = kind.strip()
    known = known_slice_kind_keys()
    if stripped not in known:
        raise ValueError(f"unknown {label} {stripped!r}; expected one of: {', '.join(sorted(known))}")
    return stripped


def _index_route_candidates(repo_root: Path) -> list[str]:
    candidates: list[str] = []
    projects = repo_root / "projects"
    if not projects.is_dir():
        return candidates
    for path in projects.rglob("*"):
        rel = path.relative_to(repo_root).as_posix()
        if path.is_file():
            candidates.append(rel)
        elif path.is_dir():
            candidates.append(f"{rel}/")
    return candidates


def nearest_existing_route_hint(route: str, repo_root: Path) -> str | None:
    normalized = route.strip().replace("\\", "/")
    candidates = _index_route_candidates(repo_root)
    if not candidates:
        return None
    matches = difflib.get_close_matches(normalized, candidates, n=3, cutoff=0.6)
    if not matches:
        return None
    if "." in Path(normalized).name:
        file_matches = [match for match in matches if not match.endswith("/")]
        if file_matches:
            return file_matches[0]
    return matches[0]


def _projects_segment(route: Path) -> str | None:
    parts = route.parts
    if len(parts) >= 2 and parts[0] == "projects":
        return parts[1]
    return None


def validate_project_route_and_key(
    route: str,
    key: str,
    *,
    repo_root: Path,
) -> None:
    route_stripped = route.strip()
    key_stripped = key.strip()
    if not route_stripped:
        return
    resolved_root = repo_root.resolve()
    route_path = (resolved_root / route_stripped).resolve()
    try:
        route_path.relative_to(resolved_root)
    except ValueError:
        die(f"project.route must be relative to the repository root (cwd): {route_stripped!r}")
    if not route_path.exists():
        hint = nearest_existing_route_hint(route_stripped, resolved_root)
        message = (
            f"project.route not found: {route_stripped!r} "
            f"(route check uses repository root = cwd: {resolved_root})"
        )
        if hint:
            message += f"; nearest existing: {hint!r}"
        die(message)
    segment = _projects_segment(Path(route_stripped))
    if segment is not None and key_stripped and key_stripped != segment:
        die(
            f"project.key {key_stripped!r} does not match projects/ segment "
            f"{segment!r} in route {route_stripped!r}"
        )
