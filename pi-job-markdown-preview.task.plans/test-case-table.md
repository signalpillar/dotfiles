# Markdown preview test cases

| Case | Version | Expectation |
| --- | --- | --- |
| Representative full task | v0 | Full dump; Decisions near top with dated bullets; `(current)` on cursor slice/step |
| Minimal / sparse task | v0 | Empty optional sections omitted; Decisions shows `_none_` |
| Uninitialized task | v0 | Preview succeeds without orchestration |
| Notes with Markdown metacharacters | v0 | Notes/context fenced; titles escaped; structure intact |
| Default slice order | v0 | Plan array order |
| Chronological sort flag | v0 | Oldest changed first via execution timestamps; missing timestamps after; plan-order tie-break |
| Validation / missing store | v0 | Non-zero exit; diagnostics on stderr; stdout empty or non-Markdown-safe |
| Read-only guarantee | v0 | Source bytes/hash unchanged after preview |
