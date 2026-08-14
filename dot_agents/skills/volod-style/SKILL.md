---
name: volod-style
description: Cross-project implementation and review style
license: personal
compatibility: all
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
- **Model over filter**: when a category of behavior needs special handling (e.g., expected errors, known event types), express it in the type system (class hierarchy, enum, tagged union) rather than adding string-based or convention-based runtime checks. Others will copy the pattern — make the right thing the easy thing.
- **Type strictness**: types must reflect the actual domain state, not the weakest type a library returns. Narrow external/library types at the boundary (resolver, converter, data loader); inner functions receive already-validated strict types with no unnecessary optionality. Never propagate `| undefined` or `?` on fields that are always present at runtime — it forces every consumer to re-check and clutters business logic with defensive guards.
- **Domain-first contracts**: model the valid, expected state of our domain rather than the loosest state an external SDK or provider accepts. If our flow requires a stricter contract (for example a required system prompt), express that in our code and fail closed at the boundary instead of mirroring optional provider fields throughout the implementation.
- **Collective domain policy**: when a domain rule needs several related facts (identifiers, eligible statuses, terminal write shape), put them in one named object or module next to that domain. Do not leave duplicated allow-lists or status arrays across services for reviewers to reconstruct.
- **Orchestrator vs capability service**: flow or orchestrator services coordinate transitions and inject capability services. They do not open the platform or client for another domain's read or write loop.
- **Domain package and fixtures**: new capability domains get a folder (`services/<domain>/`) with policy, service, and `__tests__/fixtures` owned by that package. Callers mock the service. Callers do not grow parallel fixture copies of domain rules.
- **Trust explicit search contracts**: when a client/API search is called with filters (group, status, reason, …), return that result. Do not re-assert the same filters on the response unless there is concrete evidence the client violates those filters. Speculative "broad/malformed response" re-filters are noise and hide the real contract under test (the search call).

### Architecture review checklist

When using this skill for review, also ask:

- Is there a single place that answers what counts as this domain entity and how we terminate it?
- Does the parent flow still talk to the client for that subdomain?
- Do the new package's fixtures encode the policy, or does the policy live only in the caller's tests?
- Is any post-search / post-read re-filter justified by evidence of a broken client contract, or is it speculative mistrust? If speculative, request removal.
- Are new/changed request, response, and internal contract types documenting each field?

## API and service behavior practices

- Return only what consumers need now; avoid leaking internal details.
- Re-check **business preconditions that can go stale** before side effects (e.g. gate still held, resource still in expected status). That is not a license to re-validate query filters already applied by the search/read you just issued.
- Make safety decisions explicit in code comments when behavior is non-obvious.
- Prefer deterministic behavior over silent fallbacks when correctness is at risk.
- Document accepted partial-failure trade-offs next to the write loop when multi-step updates are intentionally non-transactional.

## Documentation requirements

- **Document decisions in code**: for non-obvious business behavior, add concise comments near decision points explaining why the behavior exists.
- **Document edge cases**: when behavior differs for misconfiguration, fallback, unsupported modes, or delayed actions, add comments and tests that explicitly describe those cases.
- **Document contracts on types**: add field-level docstrings on response/request/internal contract types so intent is visible in code and generated docs.
- **Keep docs close to code**: prefer colocated type comments and focused README/test-doc notes over broad external prose.
- **Update docs with behavior changes**: when logic or API semantics change, update related comments/type docstrings in the same PR.

### Edge-case documentation review checklist

When using this skill for implementation or review, explicitly scan for edge-case decisions that are implemented but not documented in code comments.

- For every fallback path, document:
  - why fallback exists,
  - what data source is preferred vs fallback,
  - what correctness trade-off is accepted.
- For metadata/time fallbacks, explicitly call out temporal risk (for example, `meta.lastUpdated` can be later than the true business event time because storage updates are unrelated to the original event).
- For sequential multi-write loops without a transaction, document the accepted partial-update trade-off next to the loop.
- Ensure each documented edge-case has at least one test named after that decision.
- Treat missing edge-case comments as a review finding and request/update docs in the same change.

### Type-contract documentation review checklist

When using this skill for implementation or review, explicitly scan new and changed contract types.

- Every field on request, response, and internal contract types has a field-level docstring stating intent (not restating the TypeScript type).
- Treat missing field docs on new contract types as a review finding and add them in the same change.

## Logging style

- Use stable, kebab-case event keys for observability.
- Add structured logs before meaningful throws/rejections.
- Include identifiers and decision context fields so failures are diagnosable without stack traces.
- Keep log messages decision-oriented (what was rejected and why), not implementation-oriented.

## Testing style

- Add tests for each new branch and edge case, not just happy paths.
- Prefer `test.each` for cases that exercise the same behavior with different inputs or expected outputs instead of duplicating test bodies.
- Reuse existing fixtures and test doubles when they express the required scenario.
- When existing fixtures do not fit, create focused reusable fixtures rather than duplicating setup or object literals across tests.
- Ensure tests cover both business-success and safety-failure modes.
- Keep tests aligned with current contracts; update fixtures first when interfaces change.
- Add at least one test for each documented edge-case decision.
- When extracting a domain service, move scenario fixtures with it. Leave orchestrator tests asserting call contracts only.

### Test review checklist

When using this skill for review, explicitly scan changed tests for avoidable duplication.

- Request `test.each` when multiple tests differ only in input, expected output, or a small scenario parameter.
- Request reuse of an existing fixture when duplicated setup already has a shared representation.
- Request a new focused fixture when repeated setup has no suitable reusable fixture.
- Keep separate tests when parameterization would hide materially different behavior or make failures harder to understand.
- Request fixture ownership under the new domain package when domain scenarios still live only under the orchestrator test folder.

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
