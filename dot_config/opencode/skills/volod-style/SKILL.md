---
name: volod-style
description: Cross-project implementation and review style
license: personal
compatibility: opencode
metadata:
  audience: developer
---

## What I do

- Apply a consistent engineering style across repositories and projects.
- Keep change scope tight, prefer incremental delivery, and avoid speculative refactors.
- Prioritize explicit behavior, observability, and test coverage for every new code path.

## Architecture and design principles

- **Single responsibility boundaries**: define one service as the source of truth for each domain decision (e.g., policy/rule resolution), and make downstream code consume that output.
- **Explicit contracts**: prefer small, intentional response types with fields that are actually required by consumers.
- **Misconfiguration safety**: fail closed for risky flows (block action or return non-actionable state) when required configuration is missing.
- **Provider boundaries**: do not mix local configuration validation with external provider validation unless the use case explicitly requires it.
- **Future-proofing**: when new variants are expected, extract extension points only when they reduce complexity now; otherwise keep implementation simple.

## API and service behavior practices

- Return only what consumers need now; avoid leaking internal details.
- Re-check preconditions before executing side effects (do not trust stale state).
- Make safety decisions explicit in code comments when behavior is non-obvious.
- Prefer deterministic behavior over silent fallbacks when correctness is at risk.

## Documentation requirements

- **Document decisions in code**: for non-obvious business behavior, add concise comments near decision points explaining why the behavior exists.
- **Document edge cases**: when behavior differs for misconfiguration, fallback, unsupported modes, or delayed actions, add comments and tests that explicitly describe those cases.
- **Document contracts on types**: add field-level docstrings on response/request/internal contract types so intent is visible in code and generated docs.
- **Keep docs close to code**: prefer colocated type comments and focused README/test-doc notes over broad external prose.
- **Update docs with behavior changes**: when logic or API semantics change, update related comments/type docstrings in the same PR.

## Logging style

- Use stable, kebab-case event keys for observability.
- Add structured logs before meaningful throws/rejections.
- Include identifiers and decision context fields so failures are diagnosable without stack traces.
- Keep log messages decision-oriented (what was rejected and why), not implementation-oriented.

## Testing style

- Add tests for each new branch and edge case, not just happy paths.
- Use reusable fixtures/test doubles to reduce duplication and support future file moves.
- Ensure tests cover both business-success and safety-failure modes.
- Keep tests aligned with current contracts; update fixtures first when interfaces change.
- Add at least one test for each documented edge-case decision.

## Code style preferences

- Add comments for non-obvious decisions (the why, not the what).
- Keep methods short by extracting focused private helpers.
- Prefer explicit, domain-revealing names.
- Avoid dead code and stale TODO noise in production paths.

## When to use me

Use this skill for implementation/review tasks in any backend service where you need:

- clear service boundaries
- safe failure behavior
- high-signal observability
- maintainable test design
