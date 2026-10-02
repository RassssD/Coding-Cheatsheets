# AI coding workshop: notes, Obsidian plan and skill review

Draft, 2 October 2026. Sources: `notes/notes.md`, `notes/obsidian-integration-feasibility.md`,
the eight skills in `skills/claude/` of Arnaud Dyèvre's LSE workshop pack, and my five local
repositories under `H:\GitHub`.

Status of the evidence: the workflow description in section 3 is inferred from repository
contents, not from conversation history. No earlier Claude Code conversations or saved memories
were available when this was written, so anything marked *(inferred)* needs my confirmation.

---

## 1. Workshop notes

### What Arnaud does

- VS Code as the base, one window per project, opened on the project folder. Git Graph extension.
- One unified project history file. A start-of-session skill reads it (say the last 10 entries),
  checks Git and the current to-do list. It can be shortened to a command.
- A `document_dump/` folder: drop PDFs or anything else there, and a skill reads and documents it.
- The VS Code extension has been fine even for multi-agent work, though the CLI may be easier.
- Building a skill costs almost nothing, you can do this yourself very easily. The exceptions are very specific ones: a particular dataset, a bibliography. will sometimes have useful pre-existing skills.
- Gathering context is the highest-value activity for him: bookmarks, emails, conversations,  expectations, ideas.

### Models and effort

| Task | Model | Effort |
| --- | --- | --- |
| Background work: classification, bug fixes | Third-best | Lowish |
| Writing, especially anything with visuals | Second-best | Low |
| Planning, brainstorming, organisation, maths, summaries, big reviews | Best | Higher |

- Higher effort buys more innovation. A regression table needs none, so use low effort.
- A more capable model questions more. For a simple snap decision a weaker model can do better.
- Subagents: launch with `claude -p`. Say how many to spawn and which cheaper model to use. Tell
  them to keep checks and verification to a minimum beforehand and to log failures.
- Reviewing: asking for an HTML page to review samples of output costs very little.

### My own positions and open items

- Compartmentalising email access by project looks like too much hassle for now. Jerry's
  alternative: a dedicated account to CC, holding only project material.
- Skills: read them, then ask the agent to adapt them to how I have actually been working.
- To look into:
  - an HTML template instead of slides, hosted on my website so no local files are needed;
  - pointing the agent at my Obsidian folder, using the link to a project as the search key;
  - local models;
  - experimenting with effort levels and models, and asking the agent to set this up;
  - asking Arnaud about his setup: qmd, md and the extensions for them;
  - version control for slides, to move between formats easily.

---

## 2. Obsidian integration

### The idea

I keep one main research note per project. Point an agent at that note and have it collect what
is linked to it, in both directions, so I do not gather context by hand.

### Why it is feasible

A vault is a folder of Markdown files. Forward links are a regex over the seed note. Backlinks
are a search of the vault for links that resolve to it. Neither needs Obsidian running or a
plugin. Estimated effort: about an afternoon for a script of roughly 100 lines.

### Decisions already taken

- **Plain script outside Obsidian**, not the Local REST API plugin.
- **Depth 1 only.** The script returns the seed note's forward links and backlinks as paths to
  read. Second-hop paths are recorded under their first-hop parent but not read.
- **Two skills on top of the script:**
  1. `gather-context` reads each depth-1 note, classifies it, summarises why it matters to the
     project and writes a durable index.
  2. `use-context` takes a task, looks up the index and goes straight to the relevant files.
- **The index lives in the project folder, not the vault**, so it never becomes a traversal
  target itself.
- **Staleness:** the index records its build date and is refreshed every couple of days while
  the project is active, not on every call.
- **Depth 2 by judgement:** `gather-context` may read a second-hop note it judges relevant.
  Two limits keep the cost down: the relevance check is a light peek (title, first paragraph),
  and nothing past depth 2 is ever followed.
- **Version control:** keep Obsidian Sync for cross-device availability and single-file recovery.
  Add Git inside the vault for agent batch edits: commit before and after, so the batch is one
  diff. Let Sync settle before a batch run to avoid conflict copies.

### Still open

- **Frontmatter schema.** Flagged as the highest-value step and independent of the agent work:
  fixed keys such as `project`, `aliases`, `related`, `tags`, `created`. Not yet designed. If it
  is done first, the same scan gives the title-to-path index the script needs.
- **Write access.** Whether agent edits to the vault are reviewed by me or applied directly.
- **Vault facts the script depends on:** vault path, wikilinks or Markdown links, folders to
  exclude (templates, daily notes), whether relations already sit in frontmatter.
- **How a project finds its seed note.** My workshop note suggests the project link as the key.
  The simplest form is one line in each project's README naming the vault path and seed note.
- **Nothing is built.** The note stops at the design.

---

## 3. How I work, as far as the repositories show

- **Research:** first-year MRes/PhD in economics at LSE, public economics and political economy.
  Projects use administrative data (Estonia, Sweden, the Netherlands). *(inferred)* Such data
  usually sits in a secure environment that an agent cannot reach, so agent help is likely to be
  strongest on code written outside the enclave, writing, literature and slides.
- **Languages:** R is the main one (event-study helpers, `theme_rd`). Stata for `esttab` and
  `reghdfe` work. Python notebooks and MATLAB for coursework. One Quarto test file; the Quarto
  VS Code extension is installed.
- **Reusable code:** `Coding-Cheatsheets` already has a `CLAUDE.md`, per-language helpers and
  `theming/palette.yaml` as the single source for colours and the Inter font. The R theme is
  final; the Stata, Python and LaTeX themes are placeholders.
- **Website:** a Jekyll site on GitHub Pages. Slides are published there as a PDF.
- **Notes:** Obsidian with Sync, one main note per project.
- **This machine:** Windows, with the repositories on an LSE network drive (`H:`). Git exists
  only through GitHub Desktop and is not on the PATH. Git also refuses these repositories with a
  "dubious ownership" error because of the network share. LaTeX, R, Quarto and `pdftotext` were
  not found. The workshop skills were written for macOS.
- **Claude Code settings:** Opus 5.5 at high effort for everything, which is the opposite of the
  workshop advice to match model and effort to the task.

---

## 4. The eight workshop skills

| Skill | Verdict | Main reason |
| --- | --- | --- |
| `start-session` | Adopt, light adaptation | Matches the workflow I noted; read-only, so no risk |
| `close-session` | Adapt | Useful core; archive and commit steps do not fit my setup yet |
| `update-log` | Adopt, or fold into `close-session` | Cheap; overlaps with closing |
| `status` | Discard | Four Git commands that GitHub Desktop already shows |
| `prose` | Adapt | Highest value, but the style guide is Arnaud's voice |
| `ingest-paper` | Adapt heavily | Good structure; collides with Obsidian as the home of notes |
| `compile-paper` | Defer | No LaTeX on this machine; may become a Quarto render skill |
| `beamer-to-html` | Adapt, or replace | Matches my HTML-slides idea; Quarto may be the shorter route |

### start-session

Reads the README, the latest project history entry and Git state, then reports the last work,
the next step and any uncommitted changes. It changes nothing and is invoked only by name.

Fit: this is the session-opening routine from my notes. Changes worth making: read the last
several entries, not one; report the age of the Obsidian context index once that exists; drop
the dual-agent wording if I use Claude Code only. It already degrades to "recorded state only"
when Git is unavailable, which is the case on this machine today.

### close-session

Adds a history entry, refreshes context made stale by the session, archives finished
`document_dump/` inputs with SHA-256 verification, and makes a local commit of the session's own
changes. It never pushes.

Fit: the history entry and the "stage only what this session changed" rule are worth keeping.
Two parts need a decision. The checksum archive procedure is heavy for a solo project; keep it
only if I adopt `document_dump/` as an inbox. The commit step cannot run here until Git is on
the PATH and the network-drive ownership error is resolved, and I should decide whether the
agent commits or I keep committing through GitHub Desktop.

### update-log

Writes a mid-session checkpoint to the history file without committing.

Fit: harmless and cheap. For a single agent it is `close-session` without the commit, so one
skill with a "no commit" option would do the same job.

### status

Runs `git status`, `git log -5` and two `git diff --stat` commands and summarises them.

Fit: discard. GitHub Desktop or Git Graph shows this, and I can ask for it in a sentence.

### prose

Reviews human-written text without editing it and revises agent-written text directly. The
bundled `STYLE.md` has a seven-step procedure (state the claim, cut, name the actor, read aloud,
checklist, sentence-length variation, hedge pass), registers by document type, a list of banned
filler, and a section on preserving a non-native voice.

Fit: the most useful skill for a PhD student, and the authorship boundary is the part to keep
unchanged. The style guide needs rewriting for me: its examples are French calques and national
accounts, and its sentence-length calibration comes from nine papers of Arnaud's choosing. I
would replace the examples with ones from my own drafts, set the calibration from papers in
public economics that I want to sound like, and decide whether the non-native-voice section
applies to me. Section 5 (worked example for every abstract result, intuition next to the
formalism, limitation stated flatly) transfers as it stands.

### ingest-paper

Files a PDF, reads the whole paper including appendices, writes a structured note and updates a
catalogue. The note has frontmatter, a required methodology and identification section, a
required project-relevance section and a required "what does not transfer" section. Papers are
ranked in three tiers by relevance to the project, not by general importance.

Fit: the note structure suits applied micro well. The identification section (design, assumption
as the authors state it, supporting checks, my assessment) is what I would want from a reading
note anyway. Three things must change. First, where notes live: the skill writes them into the
project, whereas my notes live in Obsidian. Literature notes in the vault, with the skill's
frontmatter merged into my own schema, would let the Obsidian traversal pick them up. Second,
tiers are per project, so a paper relevant to two projects needs a tier per project or a note
per project. Third, it needs a PDF text extractor, which this machine lacks. If I already use a
reference manager, the catalogue should defer to it. This is the "very specific" kind of skill
that Arnaud said does carry a real setup cost.

### compile-paper

Finds the LaTeX entry point, picks the engine, builds with `latexmk` from the right directory
and reports errors, unresolved references and box warnings. It never edits the source.

Fit: nothing to run here, since there is no LaTeX installation on this machine. If I write in
Overleaf it is irrelevant. If I move to Quarto, the same idea becomes a short `render` skill:
find the `.qmd`, run `quarto render`, report errors and unresolved cross-references. Defer until
the qmd-versus-LaTeX question in my notes is settled.

### beamer-to-html

Converts a Beamer deck into one scrolling HTML page with a section menu, KaTeX maths and
per-slide references. It reads the TeX, uses the compiled PDF as the reference, converts frame
by frame without summarising, and then checks slide count, maths, figures and text against the
PDF. Conversion rules sit in `CONVERSION.md`, the look in `assets/template.html`.

Fit: this is the first item on my to-look-into list, and my website currently serves slides as a
PDF. Two reservations. It needs LaTeX and Poppler tools, which I do not have locally. And if the
goal is HTML slides under version control with easy movement between formats, writing slides in
Quarto gives HTML and PDF from one source with no conversion step. So: adapt this skill for
decks that already exist in Beamer, and consider Quarto for new ones. In either case the
template should take its colours and font from `Coding-Cheatsheets/theming/palette.yaml`.

---

## 5. Skills the pack does not include but my notes point to

| Candidate | What it would do | Source |
| --- | --- | --- |
| `gather-context` and `use-context` | Build and use the Obsidian context index | Section 2 |
| `review-page` | Build an HTML page for reviewing a sample of outputs | Notes; exercise 5 |
| `intake` | Read whatever is in `document_dump/` and document it | Notes |
| House-style rule | Apply `theme_rd` and my helpers to any plot | `Coding-Cheatsheets` |
| Fan-out recipe | Subagent settings for bulk classification | Notes on models and effort |

The house-style rule is one line in a user-level `CLAUDE.md` pointing at the cheat-sheet
repository, not a skill. The fan-out recipe is probably a short reference note for the same
reason.

---

## 6. Suggested order

1. Fix the environment first: Git on the PATH and the network-drive ownership error, or work
   from a local clone. Three of the eight skills depend on it.
2. Install `start-session` and a merged `close-session`/`update-log` at user level.
3. Rewrite `prose/STYLE.md` for my own voice.
4. Design the Obsidian frontmatter schema, then the traversal script, then the two skills.
5. Rework `ingest-paper` once the schema exists, so literature notes follow it.
6. Decide between Quarto and LaTeX, then build either a `render` skill or adapt
   `beamer-to-html`.
7. Change the default model and effort so high effort on the best model is the exception.

## 7. Questions only I can answer

1. Do I use Claude Code only, or Codex as well? This decides whether to keep the dual-agent
   conventions.
2. Where do I write papers and slides: Overleaf, local LaTeX or Quarto?
3. Do I use a reference manager, and where do literature notes live now?
4. Where is the vault, and does it use wikilinks?
5. Is this LSE machine my main one, or is there a laptop with R, LaTeX and Git set up?
6. Should the agent commit, or do I keep doing that in GitHub Desktop?
