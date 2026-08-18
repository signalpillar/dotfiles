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
- **Headline every decision in one clause, under ~10 words.** `Decision · <literal statement of what
  was decided>` — a reader gets the decision from the title alone, in one breath, no decoding. Prefer
  `Decision · Add a new hold reason instead of reusing the US one` over a slogan or metaphor like
  `New reason, never reuse the US one`. Equally bad: cramming two decisions into one title with a
  semicolon or a leading qualifier clause — `Decision · During the PPRE MVP, the gate stays after
  pharmacist approval; PPRE runs alongside the pharmacist, not instead of it` is two decisions and a
  scope note stacked into one sentence. It should have been `Decision · Gate placement unchanged for
  the PPRE MVP`, with the PPRE-alongside detail as a body bullet and the MVP scope in a status line,
  not the title. A reviewer who has to reread a title to find the verb will skim past it instead.
- **Optional one-line takeaway, stated plainly.** If a slide ends with a summary line, make it a direct
  restatement of the point ("Do the comparison once, in the integration layer"), not a slogan or
  wordplay ("compare once, react everywhere"). Skip it if it only repeats the title.
- **Simple technical English throughout.** Follow ASD-STE100, as the global agent instructions require.
  Short sentences, active voice, simple tenses, one word per meaning. Never coin a mantra, slogan, or
  memorable catchphrase for a decision - state the decision instead. Prefer the plain word over the
  domain word when both are accurate: "give the discount to one patient" beats "per-patient grant",
  "can be repeated safely" beats "idempotent", "settings" beats "configuration". Keep the precise term
  only where the reader needs it to search the code or talk to another team, and then use it once.
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
- **Tables for contracts and comparisons.** When a slide (or appendix) lists layers, API fields,
  lifecycle events, or rejected alternatives, prefer a markdown table over bullets or prose.
  Tables skim faster async and stay DRY - one row per fact. Use ASCII diagrams for *flow*;
  use tables for *structure* (who/what/when/shape).
- **ASCII first for causality.** Prefer a diagram over a paragraph for before→after and sequencing.
  Every decision slide gets a before→after or a flow **or** a comparison table - not a wall of text.
- **High level only (slides).** Decisions + trade-offs + sequencing. NO file names, function names, code, or test counts on slides. Appendices may include httpyac / field tables.
- **Plain language, no wordplay.** Titles and bullets state what was decided literally. No metaphors,
  slogans, puns, or "clever" phrasing. If a reader has to interpret a title to know the decision, rewrite it.
- **One decision, one clause, per title.** No semicolons, "and", or leading qualifiers ("During X, ...",
  "Given Y, ...") joining two decisions into one title. If you need a conjunction to state it, it's two
  slides. Move scope/timing to a status line in the body, not the headline.
- **2-minute budget.** If it can't be read in 2 minutes, cut slides. Front-load the main point.
  Appendices are exempt from the 2-minute budget but should still be table-first.

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
5. Read every title alone, with no body text visible. If it takes more than one breath to say, or you
   can't tell which part is *the* decision, rewrite it - usually by cutting a clause to a bullet.
6. Tell the user the path and that it's a temp artifact.

## When decisions keep arriving (multi-week / living review)

If the same deck gets reopened across many sessions instead of reviewed once and deleted - the team
keeps adding decisions as work continues - the per-decision rules above still apply in full (one
clause per title, no semicolons, name the rejected alternative). Drop only the slide-count and 2-minute
budget for the whole document; keep them for anything a reviewer is pointed at in one sitting. Mark
superseded decisions inline (`SUPERSEDED <date>: ...`) rather than deleting them, so the history of
*why* stays intact - but don't let "it's a living doc now" become an excuse to let titles or bullets
drift back into prose. Every new decision added later still gets the same one-breath-title treatment
as the first nine.

## When to use me

User wants to present / share / walk through decisions, prep a quick review, or a light deck/summary
for the team. Not for full proposals — those are long-form (see proposal docs).
