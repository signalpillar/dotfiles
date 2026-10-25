---
name: outsider-lens
description: Look at a ticket, bug, incident, PR, or set of regressions through the eyes of an outsider. Produce a pattern-level postmortem and anonymised lessons learned that are safe to share outside the company. Use when the user asks for an outsider view, pattern-level postmortem, anonymised lessons, a sanitised case study, or wants to talk about problems in an interview, talk, or post.
license: personal
compatibility: all
metadata:
  audience: personal
---

# Outsider Lens

Turn specific problems into transferable patterns.
An outsider knows nothing about the product, the company, or the codebase.
The output must still teach them something true.

## Input

- One or more tickets, bugs, incidents, PRs, reverts, or review findings.
- Read the real evidence first: ticket text, PR diff, logs, journal notes.
- Do not abstract from a title alone.
 If the cause is not established, label the row `cause unknown` and abstract only the symptom.

## Three levels per problem

Write each problem at three levels.
Each level drops more detail than the one before.

1. **Specific (internal).**
 What happened, with names, ticket ids, PRs, and code symbols.
 This level stays inside the company.
2. **Pattern and bigger problem (internal, team-facing).**
 - `Pattern:` the failure mode in one sentence, with domain words allowed.
 - `Bigger problem:` the systemic gap that let this pattern through.
 Name the missing guard: type, test, contract, checklist, ownership, or release step.
3. **Anonymised lesson (shareable outside).**
 One or two sentences.
 No product, company, team, person, service, repo, vendor, ticket, or URL names.
 Use generic nouns: "a service", "a second product line", "a configuration bundle", "a shared helper", "a workflow step".

## Abstraction ladder

Climb one rung at a time until no identifying detail remains.

| Rung | Example |
|---|---|
| Code | `resolveX()` throws `not-found` for category B |
| Component | The membership resolver rejects category B users |
| Mechanism | A rule for a new single-category API went into a shared helper |
| Pattern | Extending a system built for one product line to a second one |
| Lesson | Shared code needs a contract that every product line satisfies, not a gate per call site |

## Bigger-problem prompts

Ask these for each problem.
Pick the answer with the most evidence.

- Which assumption from the original design does this break?
- Does the code fail open where it must fail closed?
- Is the rule enforced at one boundary, or repeated per field or per caller?
- Do two artefacts that release separately depend on each other's version?
- Is a non-fatal failure also invisible (logged, not alerted)?
- Does a step assume that the main path ran before it?
- Is configuration-as-data type-checked against the code that reads it?
- Was a revert or fix recorded with its cause?

## Cluster

After all rows, group problems that share a bigger problem.
State each cluster as one sentence with a count: "3 of 4 regressions share X."
A cluster is the headline for an outside audience.

## Leak check

Before you show the anonymised level, check every cell.
Remove or replace:

- Company, brand, product, programme, and partner names.
- Internal service, repo, package, and code symbol names.
- Ticket keys, PR numbers, commit hashes, URLs, and environment names.
- People's names and handles.
- Exact dates, regions, counts, and amounts that identify the event.
- Medical, financial, or customer data of any kind.

If a lesson only makes sense with a removed detail, rewrite it at a higher rung.
If it still does not make sense, mark it `internal only` and do not publish it.

## Output

- Default to a table with these columns:
 `Verdict | Item | Pattern and bigger problem | Shareable outside (anonymised)`.
- Put the cluster sentences above the table.
- For an HTML artifact, follow the light editorial theme rule in the global agent instructions.
- If the user asks for prose (talk, post, interview answer), write the anonymised level only.
 Use the structure: situation, pattern, why it got through, what changes.

## Writing rules

- Do not assign blame.
 Describe the system that allowed the error, not the person.
- Keep the verdict honest internally: `mine`, `exposed`, `not mine`, `unknown`.
 The anonymised level drops the verdict.
- Do not overstate what one incident proves.
- Put each full sentence on its own line in Markdown.

## Final check

- Every anonymised cell passes the leak check.
- Every pattern names a mechanism, not only a symptom.
- Every bigger problem names a missing guard.
- Rows without established cause are labelled.
