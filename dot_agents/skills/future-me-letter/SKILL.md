---
name: future-me-letter
description: Writes durable, learning-focused Markdown letters that explain incidents, decisions, fixes, mental models, and transferable lessons to the user's future self. Use when the user asks for a future-me letter, incident memory, durable personal explanation, or notes they can learn from when a problem returns.
license: personal
compatibility: all
metadata:
  audience: personal
---

# Future Me Letter

Turn the current conversation and available evidence into a durable explanation that helps the user's future self remember and transfer what they learned.

## Output

- Write one Markdown file under `~/Documents/future-me/`.
- Name it `<YYYY-MM-DD>-<short-topic>.md` using the local date and a lowercase kebab-case topic.
- Create the directory when it does not exist.
- Never overwrite a letter.
- If the path exists, add `-2`, `-3`, and so on before `.md`.
- Tell the user the final absolute path.

## Structure

1. Title the document `# Letter to Future Me: <Topic>`.
2. Open with `Dear future me,` and a concise statement of why this letter exists.
3. Describe the visible symptoms and their context.
4. State the verified root cause and cite the evidence that proved it.
5. Explain why the underlying system permits this failure mode.
6. Build a compact causal mental model that connects the components involved.
7. Explain the most plausible false lead and how the evidence ruled it out.
8. Record the repair and the commands that verified it.
9. Extract general lessons without overstating what one incident proves.
10. End with a short checklist and three retrieval questions.

## Learning Rules

- Teach the diagnostic reasoning, not only the final command.
- State the causal chain in the form `context -> decision -> mechanism -> symptom`.
- Contrast the broken and working paths when configuration changes by environment.
- Explain which observation had the highest information value and why.
- Derive transferable principles that apply beyond the specific tool.
- Add three answerable questions at the end so future me can test recall before rereading.

## Evidence Rules

- Clearly separate observed facts, conclusions supported by those facts, and unresolved possibilities.
- Do not invent an exact historical cause when the evidence only proves the final broken state.
- Include decisive error messages and commands, but omit noisy logs.
- Explain misleading symptoms, especially when the first warning was not the root cause.
- Compare tools or approaches by their operating model and trade-offs, not by declaring a universal winner.

## Writing Style

- Write for a technically capable reader who has forgotten the incident.
- Prefer direct prose and short sections over a transcript or timeline dump.
- Put each complete sentence on its own physical line.
- Use ASCII punctuation.
- Make the letter useful without requiring access to the original conversation.

## Final Check

- Confirm the destination is under `~/Documents/future-me/`.
- Confirm no existing file was overwritten.
- Confirm all commands and paths are accurate for the incident.
- Confirm uncertainty is explicitly labeled.
