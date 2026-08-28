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
- **Selfish units**: write code in a selfish manner. We do not care who calls us. The code is self-contained, works with valid state, and produces valid state. No assumptions about the caller. Comments, types, and logs describe this unit's contract, not a named caller, route, or auth mode.
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
- **Owner service loads owned facts**: if a fact lives on a resource the service already loads, the service reads it. Do not add a caller param that can disagree with storage. Flag a second id when the path resource already determines it (for example a FHIR `CarePlan.subject`).
- **Do not runtime-check typed-required fields**: if this unit's contract type says the field is always present, do not add an `if (!field)` fail-closed in that function. A helper whose only job is "assert non-null of a required field" is a review finding.
- **Load-edge narrowing**: when a library or FHIR type leaves a nested field optional, validate it at the site that loads the resource. Throw. Do not use `!`. Inner functions then receive the narrowed value. Do not skip this because the field is "always set in practice". This does not license a second fail-closed in a consumer of an already-loaded typed object.
- **Reuse collaborator output**: before adding a parse, helper, or second load for a fact, open the return type and implementation of every collaborator this unit already calls. Do not infer from the current destructure. If that type already carries the fact, or an object that owns it, use that field. Grep sibling consumers of the same collaborator and copy their access path. Do not re-parse the raw resource the collaborator already loaded. Do not add a fail-closed the loader already performs. Narrow once at the load site; consumers of an already-loaded object consume, they do not re-load. A new `getXFromRaw(resource)` next to a call that already returns `x` or `owner.x` is a review finding.
- **Shared identity seam**: keep a mint/resolve helper even when it is identity today. Two facades must not fork encodings. Do not inline until the encoding is actually opaque.
- **Fault vs empty outcome**: config load failures, missing definitions, and unreadable identifiers are hard fails (throw / HTTP 500). Do not catch them into a valid empty / not-applicable result. Operators must tell a broken identifier from a real empty match.
- **Pass-through vs policy owner**: a resolver that selects a row passes the row through. It does not allow-list values, invent omitted fields, or fail closed on a field another service owns. Put that policy in the consumer of the field.
- **Tagged union, no optional-on-some-status fields**: each variant carries only the fields that exist in that state, all required. Drop a shared base that makes a field optional because it is absent on other variants. A status nobody can act on is a throw, not a variant.
- **Absence is omit, not a sentinel**: do not encode "none" as `0`, an empty array, or a boolean plus payload pair. A match is the signal. Unmatched means none. `0` is a real value.
- **Parent-level facts once**: if every child row shares one value, put it on the parent result, not on each child. Do not hardcode a copy of a field that already exists on the parent.
- **Fold same-event flags**: two reason codes or two booleans that describe one business event become one name. Extra codes are extra branches in every facade.
- **Key vs id**: an internal catalog/config key is `*Key`. An external provider identifier is `*Id`. Do not name a key `Id`. FHIR identifier systems follow the same split.

### Architecture review checklist

When using this skill for review, also ask:

- Is there a single place that answers what counts as this domain entity and how we terminate it?
- Does the parent flow still talk to the client for that subdomain?
- Do the new package's fixtures encode the policy, or does the policy live only in the caller's tests?
- Is any post-search / post-read re-filter justified by evidence of a broken client contract, or is it speculative mistrust? If speculative, request removal.
- Are new/changed request, response, and internal contract types documenting each field?
- Did wire-format validation (ISO timestamps, query/body shapes, Zod) land in a domain service? Flag it. Those checks belong at the HTTP/API boundary. The service receives already-valid values.
- Do field comments help a reader who has never seen this code (format, ownership, when to omit)? Reject comments that only restate the field name or the TypeScript type.
- Does this unit assume a named caller, route, or auth mode in comments, types, or logs? Request removal. The unit takes valid state and returns valid state.
- Does this service re-check a field its input type already requires? Request removal.
- Did this unit re-parse a fact a collaborator already returned? Request using the collaborator field instead.
- Did the load site skip a nested field the library types as optional? Request a fail-closed check there, not a `!`. Do not request a second fail-closed in a consumer of that loaded object.
- Does a catch map a config throw into a valid empty result? Request that the throw surface.
- Does a child row repeat a parent field? Request it live once on the parent.
- Do two reason codes or flags name the same event? Request they fold.

### HTTP / Zod review checklist

When reviewing or writing an HTTP facade, scan every new or changed request field. This checklist exists because a non-empty string check shipped where a UUID check belonged.

- **Id format, not presence only.** UUID resource ids must fail closed on format at the HTTP boundary. `z.string().min(1)` is a review finding. Use the repo helper: Zod 3 `z.string().uuid()`, Zod 4 `z.uuid()`, or an existing `RequiredUuid`. Add a test that a non-UUID is 400 and does not call the domain service.
- **Do not take a redundant caller id.** If the path resource already determines the subject (for example a FHIR `CarePlan.subject`), do not also require that id on the query or body unless product asked for an ownership check. Derive it. Flag duplicate ids that can disagree.
- **One schema, one type.** Do not hand-write an interface that repeats a Zod object. Use `z.infer<typeof Schema>`. Flag the duplicate.
- **`Exclude` vs `Omit`.** Narrow a string-literal union with `Exclude<T, "X">`. `Omit` is for object keys. Flag the wrong helper.
- **Exhaustive `switch`.** If every typed member has a `case`, do not add a `default` throw or `assertNever` that cannot run. TypeScript already forces a new `case` when the union grows. Keep a fail-closed `case` only when that member is in the union and must not leak (for example a write-only reason on GET).
- **Prefer the unit the domain already uses.** Do not wrap a scalar the callers already share as `{ value, unit }` unless product named that shape.
- **Scopes match reads and writes.** Declare inbound scopes for every resource the handler reads or writes. `@Tags` is documentation only. GET stays non-mutating: read scopes only, including nested reads (discount history, preferences), not only the path resource. Flag a mutating route that still has only a read scope. Flag a GET that under-scopes its reads.
- **Boolean field docs cover both polarities.** If a flag is true in only one current state, say when it is true and when it is false.
- **Do not leak internals on consumer DTOs.** Hide catalog keys, provider ids, and matched-rule ids unless product named them as consumer fields. Consumers send opaque option ids.

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

- Every field on request, response, and internal contract types has a field-level docstring a first-time reader can use (format, ownership, when to omit). Do not restate the field name or the TypeScript type.
- Treat missing field docs on new contract types as a review finding and add them in the same change.
- Treat technical or verbose field comments that only restate the implementation as a review finding. Rewrite them so a newcomer understands the field.
- Treat comments that name a caller, route, or auth mode as a review finding. Rewrite them to the unit's own contract. The unit does not know who calls it.

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
- Extract assertion helpers when multiple tests repeat the same group of assertions (for example, zero writes or contract invariants).
- Name assertion helpers after domain intent (for example `expectNoBillingWrites`) rather than mock implementation details.
- Ensure tests cover both business-success and safety-failure modes.
- Keep tests aligned with current contracts; update fixtures first when interfaces change.
- Add at least one test for each documented edge-case decision.
- When extracting a domain service, move scenario fixtures with it. Leave orchestrator tests asserting call contracts only.

### Test review checklist

When using this skill for review, explicitly scan changed tests for avoidable duplication.

- Request `test.each` when multiple tests differ only in input, expected output, or a small scenario parameter.
- Request reuse of an existing fixture when duplicated setup already has a shared representation.
- Request a new focused fixture when repeated setup has no suitable reusable fixture.
- Request an assertion helper when tests duplicate identical multi-line assertion blocks across cases.
- Keep separate tests when parameterization would hide materially different behavior or make failures harder to understand.
- Request fixture ownership under the new domain package when domain scenarios still live only under the orchestrator test folder.
- Request a fixtures file when builders and test doubles accumulate inline in the spec file.
- If a later PR must honour an id this PR mints, add `it.failing` here that encodes that consumer contract. The next PR must make it pass and drop `.failing`. Do not leave the seam untested because the consumer is not in this PR.

## Code style preferences

- Add comments for non-obvious decisions (the why, not the what). Do not mention who calls this unit or how a route is authenticated.
- Keep methods short by extracting focused private helpers.
- Extract inlined strategy lambdas to module-level functions. Do not export them if only the module uses them.
- Pass a child logger into domain services. Do not pass the whole request/log context when the child logger already carries the ids.
- Prefer explicit, domain-revealing names.
- Avoid dead code and stale TODO noise in production paths.
- Do not call `.trim()` on JavaScript or TypeScript string fields.
  Apply this to every source: request bodies, query params, headers, env vars, config, databases, APIs, files, logs, and fixtures.
  Preserve the string as received.
  Treat new `.trim()` calls as a review finding and request removal.

## When to use me

Use this skill for implementation/review tasks in any backend service where you need:

- clear service boundaries
- safe failure behavior
- high-signal observability
- maintainable test design

Re-run after an architecture pivot or a collaborator-contract change. A prior verdict does not cover the new diff.
