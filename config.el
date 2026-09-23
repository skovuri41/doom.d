;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Shyam Kovuri"
      user-mail-address "shyam32@fastmail.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'+ `doom-big-font' -- used for `doom-big-font-mode'; use this for presentations or streaming.
;;
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 19)
;;       doom-big-font (font-spec :family "JetBrains Mono" :size 23)
;;       doom-variable-pitch-font (font-spec :family "Overpass" :size 19)
;;       doom-unicode-font (font-spec :family "JuliaMono"))

(setq doom-font (font-spec :family "Monospace" :size 11.0)
      doom-big-font (font-spec :family "Monospace" :size 15.0))

;; (defvar required-fonts '("JetBrains.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Alegreya"))

;; (defvar available-fonts
;;   (delete-dups (or (font-family-list)
;;                    (split-string (shell-command-to-string "fc-list : family")
;;                                  "[,\n]"))))

;; (defvar missing-fonts
;;   (delq nil (mapcar
;;              (lambda (font)
;;                (unless (delq nil (mapcar (lambda (f)
;;                                            (string-match-p (format "^%s$" font) f))
;;                                          available-fonts))
;;                  font))
;;              required-fonts)))

(setq fancy-splash-image (concat doom-private-dir "splash.png"))
(setq save-interprogram-paste-before-kill t)
(setq doom-theme 'doom-one)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
(setq org-directory "~/Documents/org/")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))

(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq display-time-world-list '(("America/New_York" "New York")
                                ("Europe/London" "London")
                                ("Asia/Calcutta" "New Delhi")
                                ("Asia/Calcutta" "Hyderabad")
                                ("Asia/Singapore" "Singapore")
                                ("Asia/Tokyo" "Tokyo")
                                ("Australia/Melbourne" "Melbourne")
                                ("America/Los_Angeles" "San Franscisco")))

(setq which-key-idle-delay 0.5)
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

(load! "lisp/clojure.el")
(load! "lisp/defuns.el")
(load! "lisp/keybindings.el")

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

(after! org
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;; Journal entries
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

(after! dired
  ;; (add-hook! 'dired-mode-hook 'dired-hide-details-mode)
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

(use-package! eval-sexp-fu
  :hook ((lisp-mode emacs-lisp-mode eshell-mode) . +eval-sexp-fu--init)
  :custom-face
  (eval-sexp-fu-flash ((t (:inherit isearch))))
  (eval-sexp-fu-flash-error ((t (:inherit error :inverse-video t))))
  :config
  (defun +eval-sexp-fu--init ()
    (require 'eval-sexp-fu)))

(after! cider
  (set-popup-rules!
    '(("^\\*cider-inspect\\*" :side right :width 0.39 :height 0.5 :select t :slot 10 :vslot 0)
      ("^\\*cider-repl.*\\*" :side right :width 0.39 :height 0.5 :select f :slot 0 :vslot 0 :quit nil)
      ("^\\*cider-error.*\\*" :side right :width 0.39 :height 0.5 :select t :slot 1 :vslot 0))))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(after! lsp
  (setq lsp-ui-imenu-auto-refresh t))

(setq which-key-use-C-h-commands t)
(setq which-key-side-window-max-height 0.3)

(require 'em-alias)
(require 'eshell)
(eshell/alias "0" "(eshell/cd (suggest-project-root))")

(use-package! cape
  :config
  (map! (:prefix "C-c f"
         :i "p" #'completion-at-point
         :i "d" #'cape-dabbrev
         :i "f" #'cape-file
         :i "k" #'cape-keyword
         :i "i" #'cape-ispell
         :i "s" #'cape-symbol
         :i "t" #'cape-tex))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(after! evil-org
  (map! (:map evil-org-mode-map
         :i "C-j" nil
         :i "C-k" nil
         :i "C-;" nil
         :i "C-l" nil
         :i "<return>" nil
         :i "RET" nil)))

(use-package! kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq diary-file "~/Documents/org/diary")
(diary)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

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
(use-package! company
  :config
  (add-hook 'inferior-python-mode-hook (lambda () (company-mode -1)) 'append)
  ;; The append argument ensures that it's added to the hook after other functions
  )

(setq envrc-direnv-executable "/usr/bin/direnv")
;; config.el
;;(use-package! clj2el)

(use-package! nerd-icons
  :custom
  (doom-modeline-major-mode-icon t))

;; ;; ;; ;; ;;
;; Obsidian ;;
;; ;; ;; ;; ;;

(use-package! obsidian
  :config
  (obsidian-specify-path "~/Documents/notes")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "inbox")
  :bind (:map obsidian-mode-map
              ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Jump to backlinks
              ("C-c C-b" . obsidian-backlink-jump)
              ;; If you prefer you can use `obsidian-insert-link'
              ("C-c C-l" . obsidian-insert-wikilink)
              ;; Open the Obsidian hydra
              ("C-c M-o" . obsidian-hydra/body)))

;; -- String inflection: underscore -> UPCASE -> CamelCase conversion of names
;; https://github.com/akicho8/string-inflection

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

;; (string= (getenv "XDG_SESSION_TYPE") "wayland")
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


(setq redisplay-dont-pause t)
(setq xdg-session-type-string "wayland")
(setq xdg-session-type 1)
(setq auth-sources '("~/.authinfo"))  ; Prioritizes .gp

;; ────── Bitwarden session – fresh on every Emacs start AND callable on demand ──────
(defun my/ensure-bw-session ()
  "Make sure Bitwarden is logged in + unlocked and BW_SESSION is in Emacs env.
Idempotent – safe to call anytime (startup, manually, after sleep, etc.)."
  (interactive)                         ; ← makes M-x my/ensure-bw-session work
  (let* ((script "~/.local/bin/bw-session.sh")
         (session-file (format "%s/bw-session"
                               (or (getenv "XDG_RUNTIME_DIR") "/tmp"))))
    ;; 1. Run the external script (does login/unlock only when needed)
    (when (file-executable-p script)
      (shell-command script))

    ;; 2. Read the fresh session token and inject it into Emacs
    (when (file-exists-p session-file)
      (setenv "BW_SESSION"
              (string-trim
               (shell-command-to-string
                "cat ${XDG_RUNTIME_DIR:-/tmp}/bw-session 2>/dev/null || echo")))
      (message "Bitwarden session loaded (%s)"
               (truncate-string-to-width (getenv "BW_SESSION") 20)))))

;; Run it automatically once when Emacs finishes starting
(add-hook 'emacs-startup-hook #'my/ensure-bw-session)

(use-package! gptel
  :config
  ;; ────── Helper: fetch any secret from Bitwarden CLI ──────
  (defun my/bw-get (item-name)
    "Return secret from Bitwarden vault. ITEM-NAME is exact name or ID."
    (string-trim
     (shell-command-to-string
      (format "bw get password %s --session $BW_SESSION"
              (shell-quote-argument item-name)))))
  (shell-command-to-string "bw status")

  ;; ────── Anthropic (Claude) ──────
  (defun my/gptel--anthropic-key ()
    (my/bw-get "anthropic api key"))

  (gptel-make-anthropic "Claude"
    :stream t
    :key #'my/gptel--anthropic-key
    :models '(claude-sonnet-4-5-20250929
              claude-haiku-4-5-20251001
              claude-opus-4-20250514))

  ;; ────── OpenAI (ChatGPT) ──────
  (defun my/gptel--openai-key ()
    (my/bw-get "OpenAI"))             

  (gptel-make-openai "ChatGPT"
    :stream t
    :key #'my/gptel--openai-key
    :models '("gpt-4o" "gpt-4o-mini" "o1" "o1-mini" "gpt-4-turbo"))

  ;; ────── Default backend & nice keybindings ──────
  (setq gptel-model   'claude-sonnet-4-5-20250929
        gptel-backend (gptel-get-backend "Claude")) ; default

  ;; SPC a … menu
  ;; (map! :leader
  ;;       :desc "GPTel menu"          "a i" #'gptel-menu
  ;;       :desc "GPTel quick ask"     "a a" #'gptel-quick
  ;;       :desc "New Claude chat"     "a c" (lambda () (interactive) (gptel "Claude"))
  ;;       :desc "New ChatGPT chat"    "a g" (lambda () (interactive) (gptel "ChatGPT")))
  )

;; ;; ;; ;; ;; ;; ;; ;;
;; "write anywhere" popup ;;
;; ;; ;; ;; ;; ;; ;; ;;

;; SUPER+ALT+E (omadots bindings.lua): floating scratch Emacs popup that
;; sends its text back to whichever window was focused before it opened.
;;
;; Tried emacs-everywhere (tecosaur/emacs-everywhere) and tinee
;; (tusharhero/tinee) here first and abandoned both, having concluded
;; auto-paste was fundamentally unachievable on this Hyprland build: neither
;; `ydotool' (uinput/evdev) nor `wtype' (Wayland virtual-keyboard protocol)
;; appeared to deliver synthetic keystrokes to the focused window, even
;; though `WAYLAND_DEBUG=1 wtype ...` showed clean, compositor-ack'd
;; requests. That conclusion was WRONG - a broken test, not a broken tool.
;; Every verification that session used a `cat > file' terminal as the
;; target, checked with no trailing newline - terminals buffer typed input
;; in canonical mode until a newline arrives, so the keystrokes were
;; genuinely delivered and just sitting unflushed in the pty, never reaching
;; `cat'. Confirmed by hand against a real (non-terminal) GUI Emacs buffer
;; target instead: `wtype' delivers text completely, every time, across
;; repeated clean runs - once one specific race is worked around (below).
;;
;; The one real bug: `wtype' has a startup race establishing its Wayland
;; virtual-keyboard connection - text sent immediately can have its first
;; several dozen milliseconds of characters silently dropped (confirmed by
;; hand: a payload came through as e.g. "yload-two-calls" instead of the
;; full string). A harmless warm-up (`-M shift -m shift' - press and
;; release Shift, no visible character) before the real payload absorbs
;; that race - but ONLY when it's part of the SAME `wtype' process/
;; connection as the real payload. Two SEPARATE `wtype' processes (a
;; throwaway warm-up call, then a second call for the real text) does NOT
;; work - each is its own independent Wayland connection, so the second
;; call hits the exact same race the first one was supposed to absorb;
;; confirmed by hand, this dropped characters intermittently even after
;; generously increasing every delay involved. Passing the modifiers AND
;; the real text to ONE SINGLE `wtype' invocation fixed it outright - 10/10
;; clean runs, single-line and multi-paragraph, right after `delete-frame'
;; + a focus-restore dispatch (the real, realistic flow, not just wtype in
;; isolation). An earlier attempt warmed up with a literal space instead of
;; a modifier press, which worked but leaked a stray leading space into the
;; target on every line (an `electric-indent-mode'/`indent-relative'
;; artifact when the test target was itself an Emacs buffer) - a modifier
;; press avoids inserting anything visible at all, in the warm-up or
;; otherwise.
;;
;; Focus-restore uses the SAME `hl.dsp.focus' Lua-dispatch mechanism fixed
;; for emacs-everywhere previously (see this file's git history) - an
;; explicit dispatch to a captured window address, confirmed reliable across
;; this whole investigation. This is NOT the same thing as tinee's approach
;; (relying on Hyprland's native refocus-on-close with no explicit dispatch
;; at all), which separately proved unreliable under rigorous testing - the
;; two are unrelated mechanisms and only one of them was ever shown to be
;; flaky.
(defun +emacs-float--call (&rest args)
  "Run a program with ARGS, returning its stdout as a string."
  (with-temp-buffer
    (apply #'call-process (car args) nil t nil (cdr args))
    (buffer-string)))

(defun +emacs-float--active-window-address ()
  "Return the address of the currently active Hyprland window."
  (require 'json)
  (alist-get 'address
             (json-read-from-string
              (+emacs-float--call "hyprctl" "-j" "activewindow"))))

(defvar-local +emacs-float-origin nil
  "Hyprland window address to send this popup buffer's text back to.")

(defun +emacs-float-wtype-send (text)
  "Type TEXT into the currently focused window via `wtype'.
A harmless Shift press/release runs first, in the SAME `wtype' process as
TEXT - see the comment above this section for why that matters."
  (call-process "wtype" nil nil nil "-M" "shift" "-m" "shift" text))

(defun +emacs-float-done ()
  "Send this buffer's text to the window +emacs-float was invoked from, then close."
  (interactive)
  (let ((text (buffer-string))
        (origin +emacs-float-origin))
    (delete-frame)
    (when origin
      (call-process "hyprctl" nil nil nil "dispatch"
                    (format "hl.dsp.focus({ window = %S })"
                            (concat "address:" origin)))
      (sleep-for 0.15))
    (+emacs-float-wtype-send text)))

(defun +emacs-float-cancel ()
  "Close the popup without sending anything."
  (interactive)
  (delete-frame))

(defun +emacs-float-init (origin)
  "Set up the just-created popup frame: fresh org-mode buffer in Evil
insert state, remembering ORIGIN to send text back to on C-c C-c."
  (let ((buf (generate-new-buffer "float")))
    (switch-to-buffer buf)
    (org-mode)
    (setq-local +emacs-float-origin origin)
    (local-set-key (kbd "C-c C-c") #'+emacs-float-done)
    (local-set-key (kbd "C-c C-k") #'+emacs-float-cancel)
    (evil-insert-state)))

;;;###autoload
(defun +emacs-float ()
  "Open a floating scratch Emacs popup; C-c C-c sends its text back to
whichever window was focused when this was invoked (C-c C-k cancels)."
  (interactive)
  (let ((origin (+emacs-float--active-window-address)))
    (call-process "emacsclient" nil 0 nil
                  "--create-frame" "--frame-parameters"
                  "((name . \"emacs-float\"))"
                  "--eval"
                  (format "(+emacs-float-init %S)" origin))))

