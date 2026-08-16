---
name: decision-review-deck
description: Create a light, presentation-style markdown deck — ASCII diagrams, skimmable in under 2 minutes — that summarises the high-level decisions made in a piece of work for an async team review. Use when the user wants to present decisions to the team, prepare a review, make a decision walkthrough, or a lightweight "what I decided and why" summary.
license: personal
compatibility: all
metadata:
  audience: developer
---

# Decision review deck

Turn a chunk of work into a 2-minute, skim-friendly deck a colleague reads async and immediately
"gets" — the decisions and the *why*, not the implementation.

## Principles (what makes it land)

- **Tables for structure.** When comparing options, listing layers/fields, or showing lifecycle
  events, prefer a markdown table over long bullets. Reviewers skim tables faster than prose.
- **The diagram is the argument for flow.** Each decision earns its slide with a `Before → After`
  (or flow) that shows the change visually; the bullets only annotate. If you can't draw it and
  you can't table it, you don't understand it well enough yet.
- **Name the fork, not just the choice.** Every decision states the alternative you rejected and the
  trade-off ("app-version gating is all-or-nothing"). A decision with no visible alternative reads
  as a fact, not a decision.
- **Headline every decision.** `Decision · <punchy claim>` — the reader should get the gist from the
  titles alone.
- **Coin a mantra.** One short, memorable phrase per idea sticks better than a paragraph
  ("narrow, never widen", "B = A minus the token", "cheaper to change a decision than a diff").
- **Frame the problem as tension.** Open with two forces that pull apart (new apps want X; old apps
  want not-X). The decisions are how you resolved the tension.
- **One visual vocabulary.** Reuse the same glyphs everywhere — `──▶` flow, `[ ]` decision point,
  `└` sub-note, `▲` inline callout — and align columns so it scans as a diagram, not text.
- **Surface the non-obvious constraint.** Call out the hidden invariant that makes the design safe
  ("same FHIR identity ⇒ history unaffected"). That's often the line a reviewer most needs.
- **End with the map.** Close on a sequencing diagram (parallel vs dependent) plus a done/next
  status legend, so "where are we" is answered at a glance.

## Output

- One markdown file, dated, in the relevant project dir: `<project>/<YYYY-MM-DD>-<topic>-review.md`.
- First line marks it a temporary review artifact (delete after the review).

## Hard rules (these keep it light)

- **Slides, not prose.** Separate each slide with `---`. Aim for 6–9 slides total.
- **One idea per slide.** A title, a small ASCII diagram, and ≤3 bullets. If it needs more, split it.
- **Tables beat prose for structure.** Prefer markdown tables for layers, API fields, lifecycle
  matrices, and rejected-vs-chosen comparisons. Use ASCII for flow/causality; tables for shape.
- **ASCII first for causality.** Prefer a diagram over a paragraph for before→after and sequencing.
  Every decision slide gets a before→after, a flow, **or** a comparison table - not a wall of text.
- **High level only (slides).** Decisions + trade-offs + sequencing. NO file names, function names, code, or test counts on slides. Appendices may use httpyac and field tables.
- **2-minute budget.** If it can't be read in 2 minutes, cut slides. Front-load the punchline.
  Appendices are exempt but should still be table-first.

## Slide skeleton

1. Title + one-line context (what + why now).
2. The problem — one ASCII diagram of the pain.
3.–N. One slide per decision: `Decision · <one line>` + before→after ASCII + why (≤3 bullets, name the rejected alternative).
N+1. Delivery / sequencing — ASCII of the slices and their order + a tiny status legend.

## ASCII patterns to reuse

Before / after:

```
Before:  answer ──▶ [server logic decides] ──▶ reject
After:   answer(+token) ──▶ [token present?] ──▶ reject / continue
```

Flow / negotiation:

```
client ──capabilities:[X]──▶ server ──picks──▶ variant
```

Sequencing (parallel + dependencies):

```
S1 ─┐
    ├─▶ S3 ─▶ S4 ─▶ S5      (S1 ∥ S2)
S2 ─┘
```

## Process

1. Pull decisions from the task file / proposal `decisions` (the durable record), not from the diff.
2. For each: write the one-liner + the alternative that was rejected.
3. Draw the smallest ASCII that conveys it.
4. Read top to bottom; if over ~9 slides or 2 minutes, cut.
5. Tell the user the path and that it's a temp artifact.

## When to use me

User wants to present / share / walk through decisions, prep a quick review, or a light deck/summary
for the team. Not for full proposals — those are long-form (see proposal docs).
