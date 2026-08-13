# Research: simplifying AI agent prose with STE and Google style

Date: 2026-08-13

## Question / scope

How do people on the internet (especially Twitter/X, blogs, GitHub, and Cursor/Claude/Codex communities) update AI coding agents to reduce "AI language" / AI-sounding prose?

Particular focus:

- ASD-STE100 / Simplified Technical English (STE)
- Google developer documentation style guide

Also covered: adjacent "anti-slop" practices in `AGENTS.md` / `CLAUDE.md` / system prompts when people cite STE or Google's guide, or pursue the same goal without naming those standards.

## Honesty about evidence gaps

**Twitter/X primary evidence is thin in this pass.**
Direct `x.com` / `twitter.com` search returned little usable first-party HTML.
The main named X-adjacent signal found is Matt Pocock (`@mattpocockuk`) experimenting with a global `CLAUDE.md` STE line, preserved via a third-party aggregator rather than a stable official tweet URL ([bittide.aicompass.dev cache](https://bittide.aicompass.dev/article/e84d6d91-8d99-46cd-aad6-efcdb24868f0)).
Treat that as a secondary community source.

**Stronger community evidence is on Hacker News and GitHub**, not X.
The main STE-for-agents discussion is the HN thread on AminBlg/SimpleEnglish ([news.ycombinator.com/item?id=49114639](https://news.ycombinator.com/item?id=49114639)).

**Google style guide + agents** shows up clearly in docs-oriented GitHub repos (`CLAUDE.md` / `AGENTS.md`), not as a viral X meme.

Simon Willison writes about "slop" as unwanted AI content, not as STE/Google style wiring for agents ([simonwillison.net, 2024-05-08](https://simonwillison.net/2024/May/8/slop/)).
No primary Karpathy source was found that ties agent prose control specifically to STE or Google's style guide.

## What ASD-STE100 and Google's guide actually are

### ASD-STE100 (Simplified Technical English)

Primary source: [asd-ste100.org](https://www.asd-ste100.org/).

STE is a controlled natural language and international standard for technical documentation, owned by ASD (Aerospace, Security and Defence Industries Association of Europe) ([home page](https://www.asd-ste100.org/)).
It was developed so maintenance documentation is easier to understand for readers with only a basic command of English ([home page history](https://www.asd-ste100.org/); [about](https://www.asd-ste100.org/about.html)).

Structure (official):

- Part 1: writing rules (grammar and style)
- Part 2: controlled dictionary (approved words)

([about](https://www.asd-ste100.org/about.html); [FAQ](https://asd-ste100.org/STE_faq.html))

Core design goals from official pages:

- One word / one meaning where possible (example: prefer "start" over "begin", "commence", "initiate", "originate") ([about](https://www.asd-ste100.org/about.html))
- American English / Merriam-Webster for approved meanings and spelling ([about](https://www.asd-ste100.org/about.html); [FAQ](https://asd-ste100.org/STE_faq.html))
- Reduce ambiguity that can cause maintenance errors ([about](https://www.asd-ste100.org/about.html))

Current free issue: Issue 9, January 2025, available by request ([downloads](https://www.asd-ste100.org/STE_downloads.html); [ASD Europe announcement](https://www.asd-europe.org/news-media/news-events/news/simplified-technical-english-asd-ste100-issue-9/)).

STEMG also published a white paper on STE and AI.
Official summary: AI text can look STE-like without verified compliance; human oversight remains required; keep ASD-STE100 as the primary reference ([downloads page](https://www.asd-ste100.org/STE_downloads.html)).

Official FAQ scope limit: STE is for technical documentation (procedural and descriptive).
It is not intended for general-purpose writing, though short sentences, one topic per sentence, and active voice principles can transfer ([FAQ](https://asd-ste100.org/STE_faq.html)).

Unofficial agent skills that paraphrase STE rules are **not** ASD-endorsed and do not make output STE-certified ([AminBlg/SimpleEnglish README position](https://github.com/AminBlg/SimpleEnglish)).

### Google developer documentation style guide

Primary source: [developers.google.com/style](https://developers.google.com/style).

It is an editorial guide for clear, consistent technical documentation aimed at software developers and other technical practitioners ([about](https://developers.google.com/style)).
It is guidelines, not rigid law: "Break any of these rules sooner than say anything outright barbarous" ([about](https://developers.google.com/style)).

Highlights that matter for "less AI-sounding" prose ([highlights](https://developers.google.com/style/highlights); [voice and tone](https://developers.google.com/style/tone)):

- Conversational and friendly without being frivolous
- Second person ("you") rather than "we"
- Active voice
- Conditions before instructions
- Avoid buzzwords, filler ("simply", "It's that simple", "quickly" in procedures), and over-polite "please" in instructions
- Write for a global audience

Google's tone goal is different from STE's.
Google wants natural, approachable developer docs ([tone](https://developers.google.com/style/tone)).
STE wants controlled, unambiguous maintenance English ([about](https://www.asd-ste100.org/about.html)).

Overlap with STE-like anti-slop goals:

- Active voice
- Conditions before commands
- Short, direct phrasing
- Ban on empty intensifiers like "simply" / "easily" in procedures ([tone](https://developers.google.com/style/tone))

Tension with STE-style agent prompts:

- Google encourages conversational tone and can use transitions ([tone](https://developers.google.com/style/tone))
- Common STE agent paraphrases ban contractions, ban "should/would/may/might", and cap sentence length ([SimpleEnglish system prompt](https://raw.githubusercontent.com/AminBlg/SimpleEnglish/main/prompts/system-prompt.md))

## How people wire these into agent instructions

### Pattern A: one-line STE instruction in system prompt / CLAUDE.md / AGENTS.md

This is the dominant practical advice in the SimpleEnglish HN thread.

Example quoted on HN by `hsaliak`:

> Output tokens are precious, be succinct in your responses. Use ASD-STE100 simplified technical english

([HN comment](https://news.ycombinator.com/item?id=49116959); same line in [hsaliak/std_slop system_prompt.md](https://raw.githubusercontent.com/hsaliak/std_slop/main/system_prompt.md))

Another HN commenter reports using a plain ASD-STE100 prompt for "a little bit now": shorter sentences and fewer pitch-deck section titles, "significant quality increase" but "nothing earth shattering" ([danielbarla](https://news.ycombinator.com/item?id=49119780)).

Matt Pocock (secondary / aggregator-sourced):

> Always talk in ASD-STE100 Simplified Technical English. Always read CONTEXT.md files, and use their ubiquitous language.

([aggregator cache of @mattpocockuk](https://bittide.aicompass.dev/article/e84d6d91-8d99-46cd-aad6-efcdb24868f0))

### Pattern B: paste a condensed STE rule block into AGENTS.md / .cursorrules

AminBlg/SimpleEnglish ships a pasteable prompt for harnesses without skills support ([prompts/system-prompt.md](https://raw.githubusercontent.com/AminBlg/SimpleEnglish/main/prompts/system-prompt.md)).
Install path: `npx skills add AminBlg/SimpleEnglish` for Agent Skills-compatible tools (Claude Code, Cursor, Codex, etc.) ([repo](https://github.com/AminBlg/SimpleEnglish)).

Condensed rules people actually paste (paraphrase of STE, not the official dictionary):

- Procedural: imperative, max ~20 words/sentence, one instruction per sentence
- Descriptive: simple tenses, max ~25 words/sentence
- No present perfect; no "-ing" verb forms as verbs
- Approved modals: can, will, must; ban should/would/may/might/could
- Conditions before commands
- One word per meaning; delete filler (simply, seamlessly, robust, leverage, ...)
- Do not apply to marketing/brand writing

(~60-token "word-budget" version also provided in the same file.)

Repo claim (author-measured, not third-party audited): 72.9% fewer STE violations per 100 words across 6 Claude models × 8 tasks ([repo](https://github.com/AminBlg/SimpleEnglish); secondary write-up [explainx.ai](https://www.explainx.ai/blog/asd-ste100-simplified-technical-english-ai-skill-2026)).

### Pattern C: STE as a rewrite skill for agent-facing English

Secondary GitHub skill: [danyuchn/asd-ste100-skill](https://github.com/danyuchn/asd-ste100-skill).
Framing: rewrite tool descriptions, error messages, and inter-agent instructions so another agent (or non-native reader) cannot misparse them.
Explicitly not a reproduction of ASD's fixed word list.

### Pattern D: cite Google developer style guide in AGENTS.md / CLAUDE.md (docs repos)

This pattern is common in documentation repositories that use coding agents to edit docs.

**LangChain docs** ([CLAUDE.md](https://raw.githubusercontent.com/langchain-ai/docs/main/CLAUDE.md)):

```markdown
## Style guide

Follow [Google Developer Documentation Style Guide](https://developers.google.com/style).
```

They then list concrete Do/Don't rules that map Google guidance onto agent-enforceable constraints: be concise, second-person imperative present, active voice, no contractions, no first person, no weasel words ("simply", "easily", "just"), prefer commas/colons over em dashes, run `make lint_prose`.
Same style block is mirrored into Cursor and Copilot path-scoped instruction files (stated in the file header).

**InfluxData docs-v2** ([AGENTS.md](https://raw.githubusercontent.com/influxdata/docs-v2/master/AGENTS.md)):

```markdown
## Documentation style

- Follow the Google Developer Documentation Style Guide.
- Use semantic line feeds: one sentence per line.
- Prefer active voice, present tense, and second person.
```

These are not "make chat sound human" prompts.
They are "when the agent writes product docs, follow Google style."

### Pattern E: enforcement after generation (lint / hooks)

HN discussion points beyond prompts:

- Vale prose linting ([vale.sh](https://vale.sh/), mentioned in [HN](https://news.ycombinator.com/item?id=49116577))
- Vale package for AI writing tells: [tbhb/vale-ai-tells](https://github.com/tbhb/vale-ai-tells), used with post-tool-use hooks and pre-commit ([HN comment by tbhb](https://news.ycombinator.com/item?id=49123801))

This is adjacent practice: prompts reduce slop at generation time; linters catch drift.

## Twitter/X and community discourse

### X / Twitter

Thin primary corpus in this research pass.
Best named signal: Matt Pocock adding STE to global `CLAUDE.md` (aggregator only; see gap note above).

No high-volume first-party X thread was retrieved that systematically compares STE vs Google style for agents.

### Hacker News (primary community source for STE-in-agents)

Thread: [Agent Skill to Force Docs in ASD-STE100...](https://news.ycombinator.com/item?id=49114639) (363 points / 122 comments at fetch time).

Representative positions:

1. **One line is enough**
   - Prefix with "Rewrite this using ASD-STE100..." or put one STE line in the system prompt ([dan_sbl](https://news.ycombinator.com/item?id=49116416); [hsaliak](https://news.ycombinator.com/item?id=49116959)).

2. **Skill packaging is contested**
   - Pushback that a dedicated skill mainly burns context if the model already "knows" STE ([lab14](https://news.ycombinator.com/item?id=49124656)).
   - Counter: packaging + benchmarks + reusable rule set are the value ([repo FAQ / explainx summary](https://www.explainx.ai/blog/asd-ste100-simplified-technical-english-ai-skill-2026)).

3. **It reduces slop but does not fully humanize**
   - Shorter sentences and fewer vacuous headings; structure can stay agent-ish ([danielbarla](https://news.ycombinator.com/item?id=49119780)).
   - `baq`: using STE in prompts for a week "deslopifies" writing; fewer "load bearing belt and suspenders" constructions ([comment](https://news.ycombinator.com/item?id=49119386)).

4. **Drift is real**
   - `Syntaf`: CLAUDE.md / profile rules eventually "go off the rails" ([comment](https://news.ycombinator.com/item?id=49117699)).
   - This motivates hooks/linters, not longer prompts alone.

5. **Irony / credibility**
   - Several commenters note the SimpleEnglish README itself reads like marketing-AI prose while claiming to kill AI slop ([bayesnet](https://news.ycombinator.com/item?id=49116112); [mcintyre1994](https://news.ycombinator.com/item?id=49116329)).
   - Repo reply: marketing is explicitly out of STE scope.

Older HN interest in STE as a controlled language (not agent-specific): [2024 STE homepage discussion](https://news.ycombinator.com/item?id=40037811).

### Adjacent style-guide packaging on HN

Same thread: Economist style guide as an LLM writing skill ([TAJD/economist-style-guide-plugin](https://github.com/TAJD/economist-style-guide-plugin), linked from [HN](https://news.ycombinator.com/item?id=49116177)).
Shows the broader pattern: name a known human style guide, package it as agent instructions.

## Adjacent anti-AI-slop patterns (often without citing STE or Google)

These pursue the same user goal ("stop sounding like AI") with ban-lists and structural rules.

| Approach | What it does | Source |
| --- | --- | --- |
| Banned vocab / phrase lists | Block delve, tapestry, leverage, "It's worth noting", em-dash pileups, etc. | [jalaalrd/anti-ai-slop-writing](https://github.com/jalaalrd/anti-ai-slop-writing), [BioInfo/slopless](https://github.com/BioInfo/slopless) |
| Voice profile + anti-slop rules | Hard rules plus corpus-derived voice (e.g. Rossmann) | [realrossmanngroup/no_ai_slop_writing_rules](https://github.com/realrossmanngroup/no_ai_slop_writing_rules) |
| Detect + rewrite skills | `/unslop`, humanizer, slop-cop | [MohamedAbdallah-14/unslop](https://github.com/MohamedAbdallah-14/unslop), [hannsxpeter/humanizer](https://github.com/hannsxpeter/humanizer), [MahmoudHalat/slop-cop](https://github.com/MahmoudHalat/slop-cop) |
| Vale AI-tells | Post-generation lint of common LLM tells | [tbhb/vale-ai-tells](https://github.com/tbhb/vale-ai-tells) |
| "Slop" as cultural term | Unwanted / unreviewed AI content | [Simon Willison](https://simonwillison.net/2024/May/8/slop/) |

Important distinction for AGENTS.md authors:

- **STE**: controlled technical English for procedures/docs; side-effect is less marketing fluff.
- **Google style**: conversational developer-docs clarity; good default for product documentation agents.
- **Anti-slop ban-lists**: optimize for "sounds human / not detectable as AI," including for blogs and social posts.
  Those lists often encourage contractions and register variation, which can conflict with STE and with some Google-docs house rules (LangChain bans contractions while following Google style).

## Practical takeaways for updating AGENTS.md / Cursor rules

1. **Pick the goal explicitly.**
   - Unambiguous tech docs / runbooks / errors → STE-inspired rules (or cite STE + paste a short rule block).
   - Product documentation voice → cite [Google developer documentation style guide](https://developers.google.com/style) and extract 8-15 enforceable Do/Don't bullets (LangChain pattern).
   - Chat replies that "sound human" → anti-slop skill or ban-list; do not expect STE alone to create personality.

2. **Start with a short always-on line, then deepen only where needed.**
   - Example STE seed: `Technical text: ASD-STE100 style. Be succinct. Prefer short sentences, active voice, condition before command. Avoid filler and synonym rotation.`
   - Example Google seed: `Follow https://developers.google.com/style for docs. Second person, active voice, conditions before instructions, no filler ("simply", "easily").`
   - HN consensus leans toward short prompts over giant skills for everyday use.

3. **Scope the rule.**
   - Apply STE / Google style to docs, READMEs, errors, release notes, and agent-facing instructions.
   - Exclude marketing, brand copy, and casual chat unless you want a flat voice (SimpleEnglish explicitly excludes marketing).

4. **Mirror into the harness files your tools actually read.**
   - Shared: `AGENTS.md`
   - Claude Code: `CLAUDE.md` (or import/symlink)
   - Cursor: `.cursor/rules/*.mdc` path-scoped for docs (LangChain mirrors style into `.cursor/rules/docs-style.mdc`)

5. **Add enforcement if drift matters.**
   - Prose linter (Vale + AI-tells) or `lint_prose` in CI/hooks beats hoping the model remembers style forever.

6. **Do not claim certification.**
   - Official STEMG: AI can look compliant without being compliant ([downloads / white paper blurb](https://www.asd-ste100.org/STE_downloads.html)).
   - Community skills paraphrase rules; they are unofficial.

7. **If combining STE + Google, resolve conflicts up front.**
   - Decide on contractions, "should", sentence length caps, and conversational tone.
   - Write the house decision as explicit bullets so the agent does not average two standards into mush.

## Sources list

### Primary / official

- [ASD-STE100 home](https://www.asd-ste100.org/)
- [About ASD-STE100](https://www.asd-ste100.org/about.html)
- [STE FAQ](https://asd-ste100.org/STE_faq.html)
- [STE downloads (Issue 9 + AI white paper blurb)](https://www.asd-ste100.org/STE_downloads.html)
- [ASD Europe STE page](https://www.asd-europe.org/standards-specifications/simplified-technical-english/)
- [ASD-STE100 Issue 9 announcement](https://www.asd-europe.org/news-media/news-events/news/simplified-technical-english-asd-ste100-issue-9/)
- [Google developer documentation style guide](https://developers.google.com/style)
- [Google style highlights](https://developers.google.com/style/highlights)
- [Google voice and tone](https://developers.google.com/style/tone)
- [Google active voice](https://developers.google.com/style/voice)

### First-party agent wiring examples

- [AminBlg/SimpleEnglish](https://github.com/AminBlg/SimpleEnglish)
- [SimpleEnglish prompts/system-prompt.md](https://raw.githubusercontent.com/AminBlg/SimpleEnglish/main/prompts/system-prompt.md)
- [hsaliak/std_slop Keep It Simple (STE line)](https://raw.githubusercontent.com/hsaliak/std_slop/main/system_prompt.md)
- [langchain-ai/docs CLAUDE.md (Google style)](https://raw.githubusercontent.com/langchain-ai/docs/main/CLAUDE.md)
- [influxdata/docs-v2 AGENTS.md (Google style)](https://raw.githubusercontent.com/influxdata/docs-v2/master/AGENTS.md)
- [danyuchn/asd-ste100-skill](https://github.com/danyuchn/asd-ste100-skill)

### Community discourse

- [HN: Agent Skill ASD-STE100 / SimpleEnglish](https://news.ycombinator.com/item?id=49114639)
- [HN: Simplified Technical English (2024)](https://news.ycombinator.com/item?id=40037811)
- [Matt Pocock STE CLAUDE.md note (aggregator / secondary)](https://bittide.aicompass.dev/article/e84d6d91-8d99-46cd-aad6-efcdb24868f0)
- [Simon Willison on "slop"](https://simonwillison.net/2024/May/8/slop/)

### Adjacent anti-slop tooling (secondary for this question, useful context)

- [tbhb/vale-ai-tells](https://github.com/tbhb/vale-ai-tells)
- [jalaalrd/anti-ai-slop-writing](https://github.com/jalaalrd/anti-ai-slop-writing)
- [BioInfo/slopless](https://github.com/BioInfo/slopless)
- [realrossmanngroup/no_ai_slop_writing_rules](https://github.com/realrossmanngroup/no_ai_slop_writing_rules)
- [MohamedAbdallah-14/unslop](https://github.com/MohamedAbdallah-14/unslop)
- [explainx write-up of SimpleEnglish (secondary)](https://www.explainx.ai/blog/asd-ste100-simplified-technical-english-ai-skill-2026)
