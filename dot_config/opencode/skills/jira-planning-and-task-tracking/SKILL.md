---
name: jira-planning-and-task-tracking
description: Track implementation plans in Jira stories and sub-tasks instead of local plan files
license: personal
compatibility: opencode
metadata:
  audience: developer
---

## What I do

- Turn a technical plan into a Jira story that acts as the source of truth for execution.
- Prefer Jira tracking over local filesystem planning docs when the plan should survive beyond the coding session.
- Create one parent story for the overall initiative and one sub-task per implementation PR or well-bounded work item.
- Keep the parent story updated as execution progresses, so the story remains the live tracker.
- Use markdown in Jira descriptions, not legacy Jira wiki markup.

## Core workflow

1. Confirm the Jira project and find the available issue types.
2. Create a parent **Story** for the initiative.
3. Put the full plan in the story description.
4. Structure the story around:
   - context
   - goals or concerns
   - ordered PR/workstream plan
   - open questions
5. Represent each planned PR inline in the story, not in a separate progress section.
6. Prefix each PR heading with a status emoji:
   - `⚪` not started
   - `🟡` in progress
   - `✅` done
7. When a PR is ready to be worked on, create a **Sub-task** under the story.
8. Include the sub-task key inline in the parent story next to that PR entry when useful.
9. Update the parent story description after each planning or execution milestone.

## Description format rules

- Always use **markdown** when creating or editing Jira descriptions.
- Do not use old Jira markup like `h2.`, `*item*`, or `{panel}` in descriptions you write.
- Keep the parent story readable first; it should work as a compact execution plan, not a dump of raw notes.
- Use inline code for ticket keys, commands, paths, and identifiers when helpful.
- Keep acceptance criteria with each PR/workstream so progress stays near the plan item it belongs to.

## Parent story template

Use this shape and adapt it to the initiative:

```md
## Context
- Why this work exists
- Main risks / drivers

## Plan

### 🟡 PR 1: <title> (`ABC-123`)
**Scope**
- ...

**Acceptance criteria**
- ...

### ⚪ PR 2: <title>
**Scope**
- ...

**Acceptance criteria**
- ...

## Open questions
- ...
```

## Sub-task rules

- Create one sub-task per PR-sized chunk of work.
- Keep each sub-task focused on the implementation slice, not the whole initiative.
- Link it to the parent story with the Jira parent relationship.
- Use markdown in the sub-task description too.
- Include:
  - scope
  - acceptance criteria
  - parent tracking reference

## Updating progress

- When work starts on a PR, update the corresponding parent-story heading from `⚪` to `🟡`.
- When work completes and lands, update it from `🟡` to `✅`.
- When a sub-task is created, add the sub-task key inline in that PR heading if it improves traceability.
- Avoid separate "current progress" sections when the inline status markers already tell the story.

## When to use me

Use this when:

- you just finished a planning session
- you want Jira, not the filesystem, to be the durable plan tracker
- the work will happen over multiple PRs
- you want the parent ticket to act as the live implementation tracker

## Good triggers

- "Create a Jira story for this plan"
- "Let's track this in Jira instead of a file"
- "Create sub-tasks for each PR"
- "Update the parent story with progress"

## Reminders

- Prefer one parent story over many disconnected tickets when the work is a single initiative.
- Keep plan state in the story description itself so someone can understand status at a glance.
- If a local planning file already exists, use it as drafting input, but move the durable plan into Jira.
- Be explicit about what changed in the story when you revise the plan after execution starts.
