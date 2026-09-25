# Doom Emacs config

Personal Doom Emacs private config (`$DOOMDIR`). See `CLAUDE.md` for
structure/architecture notes aimed at an AI assistant working in this repo —
this file just tracks the text-entry/completion productivity system built up
across several sessions, so it isn't re-derived (or duplicated) later.

## Productivity system

Four pieces work together: things you typed before, things you've saved
permanently, and a completion popup that surfaces both automatically.

### 1. History persistence (savehist)

Doom's default `savehist-mode` only wrote to disk on a clean exit, and capped
search history at 16 entries. Now (`config.el`, "History & Session
Persistence"):

- Autosaves every 3 minutes, not just on exit.
- `history-length` raised to 500; `search-ring-max`/`regexp-search-ring-max`
  raised to 100.
- `compile-command`, `compile-history`, and `last-kbd-macro` persist across
  restarts too.
- Deliberately **not** persisted, with reasons in the code comment:
  `mark-ring`/`global-mark-ring` (markers aren't reader-safe), `kmacro-ring`
  (Emacs 31 macro objects are oclosures, same problem), `dabbrev--last-table`
  (private buffer-tied cache, not a history), `log-edit-comment-ring`
  (Magit makes it buffer-local per repo; savehist would only ever see the
  empty global default).

### 2. Snippets vault (yasnippet)

Doom's `:editor snippets` module was already fully configured; it just needed
content. Two locations, split by sensitivity (`config.el`, "Snippets"):

- `snippets/` in this repo — tracked in git, for generic/shareable templates
  (e.g. `fundamental-mode/tdate` expands to today's date).
- `~/Documents/org/snippets/` — outside this repo, never reaches a remote,
  for anything personal (address, signature, etc.).

Usage: type a snippet's `key` + `TAB` in any buffer. `SPC & i` fuzzy-searches
all snippets by name (via `consult-yasnippet`, already auto-installed —
no config needed). `SPC & n` creates a new one; `C-Tab` in insert state runs
`auto-yasnippet` to turn the current line into a throwaway template.

### 3. Text expansion (`M-/`)

`M-/` now runs `hippie-expand` instead of plain `dabbrev-expand`
(`config.el`, "Historical-Text Expansion"), searching in order: **snippets
first** (exact key match, so a deliberate trigger always wins over an
incidental dabbrev match), then kill-ring, current buffer, other buffers,
then Emacs's usual line/list/file/abbrev/lisp-symbol fallbacks.

There's no "go back one source" — only forward-cycling, a relative
jump-forward (`M-2 M-/` jumps straight to source #2 on a fresh word), or full
undo (`C-u M-/`, resets and lets you restart the cycle from the top).

### 4. Consult history browsing

`M-r`, scoped to `minibuffer-local-map` only (a global binding would have
clobbered the real `move-to-window-line-top-bottom` default) — opens a
fuzzy-searchable list of the current prompt's history instead of stepping
through it one at a time with `M-p`/`M-n`.

### 5. Completion popup (Corfu, replacing Company)

Switched `init.el`'s `:completion` module from `company` to
`(corfu +icons +dabbrev)` — Doom's own module, not hand-rolled packages, so
it comes with `cape`'s per-mode dabbrev/file/elisp-block wiring,
`yasnippet-capf` (your snippets also appear as live popup candidates, not
just via `TAB`/`M-/`), `corfu-history` (candidates ranked by recency,
correctly wired into savehist this time), and `nerd-icons-corfu` for icons.
`config.el`'s "Completion" section only adds `C-c f` keys for explicitly
triggering one specific source on demand.

Rollback checkpoint before this migration: commit `f4874e8`.

## GTD / Org-mode

The workflow lives in `gtd.el` (loaded from `config.el`), separate from the rest so it can be
handed off/reviewed on its own.

- **Keywords**: one sequence — `TODO → NEXT → WAIT → PROJ → DONE`. `PROJ` is a task with
  todo-keyword subtasks; `org-stuck-projects` flags any `PROJ` with no `NEXT`/`WAIT` child.
- **Capture** (`SPC X`, a unified menu across `org-capture` and `org-roam-capture`): `i` inbox,
  `l` inbox + link, `p` new project, `b` bookmark (pulls the clipboard URL, no prompt), plus
  roam's `d`/`r`/`p` (note/reference/project-notes). A floating popup version exists for
  Hyprland's SUPER+X — see the `+org-capture-float` block in `gtd.el` for why it's built on a
  nested `emacsclient --create-frame` rather than Doom's own `+org-capture/open-frame` (the
  latter hangs the whole daemon on this Wayland build).
- **Refile**: `gtd.org` (2 levels deep) and `someday.org` (1 level) are the only targets; DONE
  headings are excluded from the completion list so it doesn't fill up with finished work.
- **Agenda** (`g` custom command): one view — today's agenda, NEXT tasks, WAIT tasks, stuck
  projects, and the inbox. Diary entries (holidays, from Emacs's default `calendar-holidays`
  list) show inline via `org-agenda-include-diary`.
- **Habits**: a task with a `:STYLE: habit` property and a repeating `SCHEDULED` date (e.g.
  `.+1d/3d`) gets a consistency graph in the agenda instead of a plain scheduled line. Put habits
  under their own heading with `:LOGGING: DONE(!)` in its property drawer (inherited by its
  subtasks) so cancelling one doesn't log a timestamp and skew the graph.

**Reviewed against [doc.norang.ca/org-mode.html](https://doc.norang.ca/org-mode.html)** (2026-09):
adopted the three items above (habit tracking, diary-in-agenda, refile DONE-exclusion) as
complementary to the existing structure. Declined the whole time-clocking system (punch-in/out,
effort/column view — not used here) and several smaller tweaks (auto-tagging WAIT, logging a
timestamp on leaving WAIT, persistent agenda filters) as unneeded complexity. Full comparison and
reasoning: `git log --grep=norang`.
