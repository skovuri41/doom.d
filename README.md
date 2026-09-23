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
