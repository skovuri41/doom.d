;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Identity

(setq user-full-name "Shyam Kovuri"
      user-mail-address "shyam32@fastmail.net")

;;; Fonts

(setq doom-font (font-spec :family "Monospace" :size 11.0)
      doom-big-font (font-spec :family "Monospace" :size 15.0))

;;; UI

(setq fancy-splash-image (concat doom-private-dir "splash.png"))
(setq save-interprogram-paste-before-kill t)
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)

;; Revert buffers (including Dired) when the underlying file changes on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq display-time-world-list '(("America/New_York" "New York")
                                ("Europe/London" "London")
                                ("Asia/Calcutta" "New Delhi")
                                ("Asia/Calcutta" "Hyderabad")
                                ("Asia/Singapore" "Singapore")
                                ("Asia/Tokyo" "Tokyo")
                                ("Australia/Melbourne" "Melbourne")
                                ("America/Los_Angeles" "San Franscisco")))

;;; File-type Associations

(setq org-directory "~/Documents/org/")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;; Journal entries

;;; History & Session Persistence

;; Doom already enables `savehist-mode' (kill-ring, register-alist,
;; search-ring, regexp-search-ring, plus every minibuffer history - M-x,
;; find-file, etc. - via `savehist-save-minibuffer-history'). By default it
;; only writes `savehist-file' on a clean exit (`savehist-autosave-interval'
;; is nil), so a crash, `kill -9', or a Doom/package error mid-session loses
;; everything back to the last graceful quit.
(after! savehist
  (setq savehist-autosave-interval 180) ; autosave every 3 minutes too
  (setq history-length 500) ; default 100; deeper M-x/consult history

  ;; `search-ring'/`regexp-search-ring' are already saved by Doom, but
  ;; `search-ring-max'/`regexp-search-ring-max' default to 16, so only the
  ;; last 16 isearches ever exist to persist in the first place - raise the
  ;; cap so the persistence Doom already set up is actually worth something.
  (setq search-ring-max 100)
  (setq regexp-search-ring-max 100)

  ;; `compile-command' and `last-kbd-macro' are plain strings/vectors (safe
  ;; to print+read) and aren't minibuffer histories, so they need to be
  ;; listed explicitly to survive a restart. `compile-history' technically
  ;; IS a minibuffer history (so `savehist-save-minibuffer-history' picks it
  ;; up automatically) but only for sessions that actually ran `M-x compile'
  ;; - listing it explicitly means it still round-trips on sessions that
  ;; didn't, instead of quietly falling out of the save file.
  (add-to-list 'savehist-additional-variables 'compile-command)
  (add-to-list 'savehist-additional-variables 'compile-history)
  (add-to-list 'savehist-additional-variables 'last-kbd-macro)

  ;; Deliberately NOT added:
  ;; - `mark-ring'/`global-mark-ring': hold markers, which print but can't
  ;;   be read back into anything useful.
  ;; - `kmacro-ring': holds oclosures (Emacs 31's macro objects), which
  ;;   don't round-trip through the Lisp reader either - `last-kbd-macro'
  ;;   above is the safe equivalent (last macro only, but actually works).
  ;; - `dabbrev--last-table': an internal expansion-cycling cache tied to a
  ;;   live buffer object (`dabbrev--last-buffer'), not a real history.
  ;; - `log-edit-comment-ring': Magit's git-commit buffers make this
  ;;   buffer-local per repo (see git-commit.el), so savehist would only
  ;;   ever see the empty global default - Magit's actual per-repo message
  ;;   rings live in `magit-repository-local-cache', which isn't disk
  ;;   persisted by Magit either and isn't safe to bulk-persist (it also
  ;;   caches other, non-printable Magit state).
  )

;; `(undo +tree)' persists undo history to disk keyed by a sha1 hash of the
;; buffer's content at save time. Any time a file changes on disk without
;; going through that Emacs buffer (Git, another editor, a tool editing the
;; file directly, etc.) and the buffer picks up the new content, the hash no
;; longer matches and `undo-tree-load-history' can't reconcile the saved
;; history with the buffer -- it's harmless (undo just starts fresh for that
;; buffer) but prints "Buffer has been modified since undo-tree history was
;; saved to ...; could not load undo-tree history" every time. Doom already
;; silences the equivalent save-side chatter (`undo-tree-save-history' via
;; `doom-shut-up-a', in doom+/modules/emacs/undo/config.el) but not this
;; load-side one -- apply the same treatment: it still lands in *Messages*,
;; just not the echo area.
(after! undo-tree
  (advice-add 'undo-tree-load-history :around #'doom-shut-up-a))

;;; Which-Key

(setq which-key-idle-delay 0.5)
(setq which-key-allow-multiple-replacements t)
(setq which-key-use-C-h-commands t)
(setq which-key-side-window-max-height 0.3)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

;;; Local Lisp

(load! "lisp/clojure.el")
(load! "lisp/defuns.el")
(load! "lisp/keybindings.el")
(load! "lisp/emacs-float.el")

;;; Editing Enhancements

(use-package! easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

(use-package! key-chord
  :init
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.3)
  (key-chord-define evil-normal-state-map "kj" 'evil-force-normal-state)
  (key-chord-define evil-visual-state-map "kj" 'evil-change-to-previous-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
  (key-chord-define evil-emacs-state-map "kj" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "kj" 'evil-normal-state)
  (key-chord-define evil-motion-state-map "kj" 'evil-change-to-previous-state)
  (key-chord-define evil-replace-state-map "kj" 'evil-change-to-previous-state)
  (key-chord-define-global "df" 'execute-extended-command)
  (key-chord-define minibuffer-local-map "kj" (kbd "C-g")))

(use-package! evil-matchit
  :init
  (progn
    (global-evil-matchit-mode t)
    (evilmi-load-plugin-rules '(clojure-mode) '(simple))))

(use-package! beacon
  :init
  (progn
    (setq
     beacon-blink-when-buffer-changes t
     beacon-blink-when-window-changes t
     beacon-blink-when-window-scrolls nil
     beacon-blink-when-focused t
     beacon-color "deep sky blue"
     beacon-blink-duration 0.6)
    (beacon-mode 1)))

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-java-style-cycle
             string-inflection-python-style-cycle
             string-inflection-elixir-style-cycle
             string-inflection-ruby-style-cycle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase
             string-inflection-kebab-case)
  :init
  (map! :prefix ("g SPC" . "Convert case")
        :desc "cycle" :nv "n" #'string-inflection-all-cycle
        :desc "toggle" :nv "t" #'string-inflection-toggle
        :desc "PascalCase" :nv "p" #'string-inflection-camelcase
        :desc "camelCase" :nv "c" #'string-inflection-lower-camelcase
        :desc "kebab-case" :nv "k" #'string-inflection-kebab-case
        :desc "snake_case" :nv "s" #'string-inflection-underscore
        :desc "Capital_Snake_Case" :nv "S" #'string-inflection-capital-underscore
        :desc "UP_CASE" :nv "u" #'string-inflection-upcase))

;;; Org & Calendar

(after! org
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
  (add-hook 'org-mode-hook #'hl-line-mode)
  (setq org-file-apps
        '((auto-mode . emacs) ; Open files matching Emacs modes (like .org) inside Emacs
          (directory . emacs) ; Open directories in Dired
          (system . "xdg-open %s")     ; Fallback for system defaults
          (t . "xdg-open %s")))        ; Catch-all rule for all other extensions
  (setq org-ctrl-k-protect-subtree t))

;; `calendar-exit' (bound to `q') restores the window configuration that was
;; saved when the calendar was opened -- e.g. by `org-agenda-goto-calendar'.
;; If a buffer in that saved config (typically *Org Agenda*, after it got
;; refreshed/regenerated while the calendar was up) no longer exists,
;; `set-window-configuration' can't put it back and leaves a stray split
;; window showing some unrelated buffer instead of collapsing back to one
;; window. When that happens (calendar's own window is gone but we're still
;; split), just drop back to a single window.
(after! calendar
  (advice-add 'calendar-exit :after #'my/calendar-exit-fixup))

(defun my/calendar-exit-fixup (&rest _)
  (when (and (> (count-windows) 1)
             (not (get-buffer-window calendar-buffer)))
    (delete-other-windows)))

;; The other direction of that round trip: from the calendar, jumping to the
;; agenda for the date at point (`org-calendar-goto-agenda') and quitting it
;; again (`q'/`Q'). Doom's org module sets `org-agenda-window-setup' to
;; `current-window' (see doom+/modules/lang/org/config.el), meaning the agenda
;; is meant to just take over the window it was called from. But the calendar
;; popup is a *dedicated* Doom popup window, and a dedicated window refuses to
;; show a different buffer in place -- so Org silently pops the agenda into a
;; brand new window next to it instead. `org-agenda--quit' only ever calls
;; `delete-window' when `org-agenda-window-setup' is NOT `current-window', so
;; that extra window never gets cleaned up: quitting just replaces its buffer
;; with whatever Emacs falls back to, leaving a stray split showing an
;; unrelated buffer instead of landing back in the calendar.
;;
;; (I tried preventing the split from ever happening -- generically, for any
;; calendar hotkey, via pre/post-command-hook watching calendar-mode -- but
;; Emacs's window-splitting fallback logic turned out too unpredictable to
;; rely on: which window ends up being "the new one" isn't consistent, and it
;; broke a live test. Cleaning up after the fact, scoped to the one command
;; this is actually verified against, is far more reliable.)
;;
;; `org-agenda--quit' is the shared internal function behind `org-agenda-quit',
;; `org-agenda-Quit' and `org-agenda-exit', so advising it covers all three.
;; This is a no-op whenever the calendar isn't actually involved.
(after! org-agenda
  (advice-add 'org-agenda--quit :around #'my/org-agenda-quit-and-cleanup))

(defun my/org-agenda-quit-and-cleanup (orig-fn &rest args)
  (let ((agenda-win (selected-window)))
    (apply orig-fn args)
    (when-let (cal-win (get-buffer-window calendar-buffer))
      (when (and (window-live-p agenda-win)
                 (not (eq agenda-win cal-win))
                 (not (one-window-p agenda-win)))
        (delete-window agenda-win))
      (when (window-live-p cal-win)
        (select-window cal-win)))))

(use-package! org-superstar
  :init
  (setq org-startup-indented t
        org-ellipsis " ▼ " ;; folding symbol
        org-superstar-headline-bullets-list '("▶" "◉" "○" "»")))

(use-package! outshine
  :commands (outshine-mode))

(use-package! org-roam
  :config
  (progn
    (setq org-roam-directory "~/Documents/org/braindump")
    (setq org-roam-file-extensions '("org" "txt"))
    (setq org-roam-index-file "index.org")
    (setq org-roam-graph-extra-config '(("overlap" . "prism")
                                        ("color" . "skyblue"))
          org-roam-graph-exclude-matcher "private"))
  (setq org-roam-dailies-directory "daily/"))

(setq diary-file "~/Documents/org/diary")
(diary)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

;; Surface diary entries (holidays, from the default `calendar-holidays'
;; list, plus anything else in `diary-file') directly in the org-agenda
;; view, instead of only in the separate diary buffer.
(setq org-agenda-include-diary t)

(after! evil-org
  (map! (:map evil-org-mode-map
         :i "C-j" nil
         :i "C-k" nil
         :i "C-;" nil
         :i "C-l" nil
         :i "<return>" nil
         :i "RET" nil)))

;;; GTD

;; See gtd.el - kept in its own file per the GTD handover spec. Loaded here,
;; after org-directory/org-roam-directory are both set above.
(load! "gtd")

;;; Snippets

;; Doom's `:editor snippets' module (already enabled) wires up yasnippet with
;; its own private dir at $DOOMDIR/snippets/ - fine for generic, shareable
;; templates since this whole directory is tracked in git. Personal/sensitive
;; snippets (address, signature, etc.) go under `org-directory' instead,
;; which isn't a git repo and never reaches a remote.
;;
;; `yas-global-mode' (enabled by Doom's module) scans `yas-snippet-dirs' as
;; soon as it turns on, which happens while the dashboard/*scratch* buffer -
;; in `fundamental-mode' - already exists, so fundamental-mode's JIT-load
;; queue fires and gets consumed immediately, before this file even runs.
;; No amount of `:init' vs `after!' timing here beats that, since the
;; buffer predates this file. `yas-load-directory' with USE-JIT is the
;; actual fix: unlike a plain directory-list append, it explicitly re-checks
;; for buffers already in the relevant mode and force-loads for them too.
(after! yasnippet
  (let ((dir (concat org-directory "snippets/")))
    (add-to-list 'yas-snippet-dirs dir t)
    (yas-load-directory dir t)))

;;; Historical-Text Expansion (M-/)

(use-package! hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        ;; yas-hippie-try-expand goes first: it only fires on an exact
        ;; snippet-key match and safely no-ops otherwise, so a deliberate
        ;; trigger like "sig" always wins over an incidental dabbrev match
        ;; from some other open buffer (e.g. a word starting with "sig...").
        '(yas-hippie-try-expand             ; your snippets (tdate, sig, ...)
          try-expand-dabbrev-from-kill      ; recently killed/copied text
          try-expand-dabbrev                ; current buffer
          try-expand-dabbrev-all-buffers    ; other open buffers
          try-expand-line
          try-expand-list
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;;; Consult

;; Scoped to the minibuffer only - a global "M-r" would clobber the default
;; `move-to-window-line-top-bottom' in every other buffer.
(map! :map minibuffer-local-map
      "M-r" #'consult-history)

;; Insert-state only: Evil already remaps "M-y" to `evil-paste-pop' in normal
;; state (see evil-maps.el), and `P' already gives fuzzy kill-ring search
;; there via `consult-yank-from-kill-ring'. In insert state "M-y" currently
;; falls through to plain `yank-pop', which only works right after an
;; Emacs-style C-y (not evil's p/P) - remapping it here is a real gap-fill,
;; not a duplicate of either of those.
(map! :i [remap yank-pop] #'consult-yank-pop)

;;; Dired

(after! dired
  (add-hook! 'dired-mode-hook 'hl-line-mode)
  (setq ls-lisp-dirs-first t)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top)))

(use-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map
        :n  "/" 'dired-narrow-fuzzy))

(use-package! dired-open
  :after dired
  :config
  (setq open-extensions
        '(("webm" . "mpv")
          ("avi" . "mpv")
          ("mp3" . "mpv")
          ("mp4" . "mpv")
          ("m4a" . "mpv")
          ("mkv" . "mpv")
          ("ogv" . "mpv")
          ("png" . "feh")
          ("pdf" . "zathura")))
  (setq dired-open-extensions open-extensions))

;;; Eval Tooling

(use-package! eval-sexp-fu
  :hook ((lisp-mode emacs-lisp-mode eshell-mode) . +eval-sexp-fu--init)
  :custom-face
  (eval-sexp-fu-flash ((t (:inherit isearch))))
  (eval-sexp-fu-flash-error ((t (:inherit error :inverse-video t))))
  :config
  (defun +eval-sexp-fu--init ()
    (require 'eval-sexp-fu)))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;;; LSP

(after! lsp
  (setq lsp-ui-imenu-auto-refresh t))

;;; Eshell

(require 'em-alias)
(require 'eshell)
(eshell/alias "0" "(eshell/cd (suggest-project-root))")

;;; Completion

;; Corfu itself, cape's per-mode dabbrev/file/elisp-block wiring, snippet
;; candidates (yasnippet-capf), icons (nerd-icons-corfu), and corfu-history's
;; savehist integration are all handled by Doom's `:completion corfu' module
;; (enabled in init.el). This just adds keys to explicitly trigger one
;; specific completion source on demand, on top of what pops up automatically.
(use-package! cape
  :config
  (map! (:prefix "C-c f"
         :i "p" #'completion-at-point
         :i "d" #'cape-dabbrev
         :i "h" #'cape-history
         :i "f" #'cape-file
         :i "k" #'cape-keyword
         :i "i" #'cape-ispell
         :i "s" #'cape-symbol
         :i "t" #'cape-tex)))

(setq envrc-direnv-executable "/usr/bin/direnv")

;;; Icons

(use-package! nerd-icons
  :custom
  (doom-modeline-major-mode-icon t))

;;; Wayland / Hyprland Integration

(when (and (executable-find "wl-copy")
           (executable-find "wl-paste"))
  (defun my-wl-copy (text)
    "Copy TEXT to the Wayland clipboard using wl-copy."
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "wl-copy" nil 0 nil)))

  (defun my-wl-paste ()
    "Paste from the Wayland clipboard using wl-paste."
    (with-temp-buffer
      (call-process "wl-paste" nil t nil "--no-newline")
      (buffer-string)))

  (setq interprogram-cut-function 'my-wl-copy)
  (setq interprogram-paste-function 'my-wl-paste))

;;; GPTel

;; Secret IDs from Bitwarden Secrets Manager (`bws') - these are UUIDs, not
;; the secrets themselves, so they're safe to keep in this tracked file.
(defconst +bws-claude-api-secret-id "f7f07964-6d9f-4fdf-8455-b4ce012aafa8")

(defun my/bws-get (secret-id)
  "Return a Bitwarden Secrets Manager secret's value by its SECRET-ID (a UUID).
Requires BWS_ACCESS_TOKEN to be set in the environment (already the case on
this machine) and `jq' on PATH."
  (string-trim
   (shell-command-to-string
    (format "bws secret get %s -o json | jq -r .value"
            (shell-quote-argument secret-id)))))

(use-package! gptel
  :config
  (defun my/gptel--anthropic-key ()
    (my/bws-get +bws-claude-api-secret-id))

  (gptel-make-anthropic "Claude"
    :stream t
    :key #'my/gptel--anthropic-key
    :models '(claude-sonnet-4-5-20250929
              claude-haiku-4-5-20251001
              claude-opus-4-20250514))

  ;; TODO: ChatGPT backend, once its Secrets Manager entry exists.

  (setq gptel-model   'claude-sonnet-4-5-20250929
        gptel-backend (gptel-get-backend "Claude")))
