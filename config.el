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
        '((auto-mode . emacs)
          (directory . emacs)
          ("\\.org\\'" . emacs)
          ("\\.txt\\'" . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . system)
          ("\\.pdf::\\([0-9]+\\)?\\'" . "zathura %s -P %1")
          ("\\.pdf\\'" . "zathura %s")))
  (setq org-ctrl-k-protect-subtree t))

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
          org-roam-graph-exclude-matcher "private")

    (setq org-roam-capture-templates
          '(("d" "default" plain
             "%?"
             :if-new (file+head "${slug}.org"
                                "#+title: ${title}\n")
             :immediate-finish t
             :unnarrowed t))))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>.org"
           :head "#+title: %<%Y-%m-%d>\n\n"))))

(setq diary-file "~/Documents/org/diary")
(diary)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

(after! evil-org
  (map! (:map evil-org-mode-map
         :i "C-j" nil
         :i "C-k" nil
         :i "C-;" nil
         :i "C-l" nil
         :i "<return>" nil
         :i "RET" nil)))

;;; Snippets

;; Doom's `:editor snippets' module (already enabled) wires up yasnippet with
;; its own private dir at $DOOMDIR/snippets/ - fine for generic, shareable
;; templates since this whole directory is tracked in git. Personal/sensitive
;; snippets (address, signature, etc.) go under `org-directory' instead,
;; which isn't a git repo and never reaches a remote.
(after! yasnippet
  (add-to-list 'yas-snippet-dirs (concat org-directory "snippets/") t))

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

;;; Consult History

;; Scoped to the minibuffer only - a global "M-r" would clobber the default
;; `move-to-window-line-top-bottom' in every other buffer.
(map! :map minibuffer-local-map
      "M-r" #'consult-history)

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
         :i "f" #'cape-file
         :i "k" #'cape-keyword
         :i "i" #'cape-ispell
         :i "s" #'cape-symbol
         :i "t" #'cape-tex)))

;;; Python

(use-package! virtualenvwrapper)
(after! virtualenvwrapper
  (setq venv-location "~/.virtualenvs/"))

(use-package! python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(after! python
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3"))

;; Projectile project type - python + poetry + pytest
(after! projectile
  (projectile-register-project-type 'python-poetry '("poetry.lock")
                                    :project-file "poetry.lock"
                                    :compile "poetry build"
                                    :test "poetry run pytest"
                                    :test-dir "tests"
                                    :test-prefix "test_"
                                    :test-suffix "_test"))

(setq envrc-direnv-executable "/usr/bin/direnv")

;;; Icons

(use-package! nerd-icons
  :custom
  (doom-modeline-major-mode-icon t))

;;; Obsidian

(use-package! obsidian
  :config
  (obsidian-specify-path "~/Documents/notes")
  (global-obsidian-mode t)
  :custom
  (obsidian-inbox-directory "inbox") ;; used by `obsidian-capture'
  :bind (:map obsidian-mode-map
              ("C-c C-o" . obsidian-follow-link-at-point)
              ("C-c C-b" . obsidian-backlink-jump)
              ("C-c C-l" . obsidian-insert-wikilink)
              ("C-c M-o" . obsidian-hydra/body)))

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

;;; Secrets & Auth

(setq auth-sources '("~/.authinfo"))

;;; GPTel

(use-package! gptel
  :config
  (defun my/bw-get (item-name)
    "Return secret from Bitwarden vault. ITEM-NAME is exact name or ID."
    (string-trim
     (shell-command-to-string
      (format "bw get password %s --session $BW_SESSION"
              (shell-quote-argument item-name)))))

  ;; Anthropic (Claude)
  (defun my/gptel--anthropic-key ()
    (my/bw-get "anthropic api key"))

  (gptel-make-anthropic "Claude"
    :stream t
    :key #'my/gptel--anthropic-key
    :models '(claude-sonnet-4-5-20250929
              claude-haiku-4-5-20251001
              claude-opus-4-20250514))

  ;; OpenAI (ChatGPT)
  (defun my/gptel--openai-key ()
    (my/bw-get "OpenAI"))

  (gptel-make-openai "ChatGPT"
    :stream t
    :key #'my/gptel--openai-key
    :models '("gpt-4o" "gpt-4o-mini" "o1" "o1-mini" "gpt-4-turbo"))

  ;; Default backend
  (setq gptel-model   'claude-sonnet-4-5-20250929
        gptel-backend (gptel-get-backend "Claude")))
