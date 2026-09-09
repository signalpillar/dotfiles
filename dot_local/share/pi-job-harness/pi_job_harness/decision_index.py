"""Binding decision index: resolve spill bodies and hide superseded rows.

Boundary: only this type parses SUPERSEDES, resolves `path` / `Plan file:`,
builds the one-line YAML claim, and formats `## Decisions`. Callers pass
`task.decisions` plus an optional task directory; they do not parse spill
files or SUPERSEDES beside this module.

`load` is the I/O edge. `markdown_lines` and claim/slug helpers are pure.
"""

from __future__ import annotations

import re
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path

PLAN_FILE_PREFIX = "Plan file: "
CLAIM_MAX_CHARS = 140
SLUG_WORD_CAP = 6
SLUG_RE = re.compile(r"^[a-z0-9]+(?:-[a-z0-9]+)*$")
SUPERSEDES_RE = re.compile(
    r"^SUPERSEDES:\s+(\d{4}-\d{2}-\d{2})(?:\s+\(([^)]+)\))?",
    re.MULTILINE,
)
SUPERSEDES_CLAIM_RE = re.compile(
    r"^SUPERSEDES:\s+\d{4}-\d{2}-\d{2}(?:\s+\([^)]+\))?\s*(?:-\s*)?(.*)$"
)
META_LINE_RE = re.compile(r"^(#\s+Decision\b|Source:)", re.IGNORECASE)


def plan_file_relpath(note: str) -> str | None:
    """Return the layout-relative path from a legacy `Plan file:` note."""
    text = note.strip()
    if text.startswith(PLAN_FILE_PREFIX):
        rel = text[len(PLAN_FILE_PREFIX) :].strip()
        return rel or None
    return None


def claim_from_body(text: str) -> str:
    """Return a one-line claim from a decision body or SUPERSEDES note."""
    for raw in text.splitlines():
        line = raw.strip()
        if not line or META_LINE_RE.match(line):
            continue
        supersede = SUPERSEDES_CLAIM_RE.match(line)
        if supersede:
            rest = supersede.group(1).strip()
            if rest:
                line = rest
            else:
                continue
        sentence = line.split(". ", 1)[0].strip()
        if sentence.endswith("."):
            sentence = sentence[:-1].rstrip()
        sentence = " ".join(sentence.split())
        if not sentence:
            continue
        if len(sentence) > CLAIM_MAX_CHARS:
            return sentence[: CLAIM_MAX_CHARS - 1].rstrip() + "…"
        return sentence
    compact = " ".join(text.split())
    if len(compact) > CLAIM_MAX_CHARS:
        return compact[: CLAIM_MAX_CHARS - 1].rstrip() + "…"
    return compact


def suggest_slug(claim: str) -> str:
    """Kebab slug from a claim. Empty when the claim has no usable words."""
    words = re.findall(r"[a-z0-9]+", claim.lower())
    slug = "-".join(words[:SLUG_WORD_CAP])
    if SLUG_RE.fullmatch(slug):
        return slug
    return ""


def supersede_targets(text: str) -> tuple[tuple[str, str], ...]:
    """Return (date, source) pairs named by SUPERSEDES lines in `text`."""
    found: list[tuple[str, str]] = []
    for match in SUPERSEDES_RE.finditer(text):
        date = match.group(1)
        source = (match.group(2) or "").strip()
        found.append((date, source))
    return tuple(found)


@dataclass(frozen=True)
class DecisionRecord:
    """One resolved decision row (body already loaded)."""

    date: str
    source: str
    claim: str
    body: str
    path: str | None
    superseded: bool


class DecisionIndex:
    """Current-only view of `task.decisions` after SUPERSEDES filtering."""

    def __init__(self, records: Sequence[DecisionRecord]) -> None:
        self.records = tuple(records)

    @classmethod
    def load(
        cls,
        decisions: Sequence[Mapping[str, object]],
        task_dir: Path | None,
    ) -> DecisionIndex:
        """Resolve bodies (disk when `task_dir` is set) and mark superseded rows."""
        raw: list[DecisionRecord] = []
        for item in decisions:
            date = str(item.get("date") or "").strip()
            source = str(item.get("source") or "").strip()
            note = str(item.get("note") or "")
            stored_path = str(item.get("path") or "").strip() or None
            legacy_path = plan_file_relpath(note)
            path = stored_path or legacy_path
            body = note
            if path and task_dir is not None:
                candidate = task_dir / path
                try:
                    body = candidate.read_text(encoding="utf-8")
                except OSError:
                    body = note
            if path:
                claim = note.strip()
                if not claim or legacy_path:
                    claim = claim_from_body(body)
            else:
                claim = claim_from_body(note)
            raw.append(
                DecisionRecord(
                    date=date,
                    source=source,
                    claim=claim,
                    body=body,
                    path=path,
                    superseded=False,
                )
            )
        superseded_exact: set[tuple[str, str]] = set()
        superseded_dates: set[str] = set()
        for record in raw:
            for date, src in supersede_targets(f"{record.body}\n{record.claim}"):
                if src:
                    superseded_exact.add((date, src))
                else:
                    superseded_dates.add(date)
        marked: list[DecisionRecord] = []
        for record in raw:
            is_superseded = (record.date, record.source) in superseded_exact
            if record.date in superseded_dates and (record.date, record.source) not in superseded_exact:
                names_this_date = any(
                    date == record.date and not src
                    for date, src in supersede_targets(f"{record.body}\n{record.claim}")
                )
                if not names_this_date:
                    is_superseded = True
            marked.append(
                DecisionRecord(
                    date=record.date,
                    source=record.source,
                    claim=record.claim,
                    body=record.body,
                    path=record.path,
                    superseded=is_superseded,
                )
            )
        return cls(marked)

    def current(self) -> tuple[DecisionRecord, ...]:
        return tuple(record for record in self.records if not record.superseded)

    def superseded_count(self) -> int:
        return sum(1 for record in self.records if record.superseded)

    def markdown_lines(self) -> list[str]:
        """`## Decisions` with current rows only; spill bodies inline."""
        from pi_job_harness.app import append_blockquote, escape_md_inline

        lines = ["## Decisions", ""]
        current = self.current()
        if not current:
            lines.append("_none_")
            return lines
        hidden = self.superseded_count()
        if hidden:
            lines.append(
                f"_Current only. {hidden} superseded rows stay in the YAML index and spill files._"
            )
            lines.append("")
        for record in current:
            date = escape_md_inline(record.date)
            header = f"- **{date}**"
            if record.source:
                header += f" ({escape_md_inline(record.source)})"
            lines.append(header)
            if record.path:
                lines.append("")
                lines.append(f"  `{record.path}`")
            body = record.body.strip()
            if body:
                lines.append("")
                append_blockquote(lines, body, indent="  ")
        return lines

    @staticmethod
    def yaml_row(*, date: str, source: str, claim: str, path: str) -> dict[str, str]:
        """Thin index row written by `add-decision` after the spill file exists."""
        return {"date": date, "note": claim, "source": source, "path": path}
