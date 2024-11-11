;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Shyam Kovuri"
      user-mail-address "shyam32@fastmail.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'+ `doom-big-font' -- used for `doom-big-font-mode'; use this for presentations or streaming.
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 20)
      doom-big-font (font-spec :family "JetBrains Mono" :size 22)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 20)
      doom-unicode-font (font-spec :family "JuliaMono")
      ;; doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light)
      )
(defvar required-fonts '("JetBrains.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Alegreya"))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(setq fancy-splash-image (concat doom-private-dir "splash.png"))

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

(setq ditaa-jar-path (expand-file-name "extra/ditaa0_9.jar" doom-private-dir))
(setq org-ditaa-jar-path (expand-file-name "extra/ditaa0_9.jar" doom-private-dir))
(setq plantuml-jar-path (expand-file-name "extra/plantuml.jar" doom-private-dir))
(setq org-plantuml-jar-path (expand-file-name "extra/plantuml.jar" doom-private-dir))

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

;; (use-package! corfu
;;   :bind (:map corfu-map
;;               ("<escape>" . corfu-quit)
;;               ("C-l" . corfu-insert)
;;               ("C-j" . corfu-next)
;;               ("C-k" . corfu-previous))
;;   :config
;;   (setq corfu-cycle t
;;         corfu-auto t
;;         corfu-auto-prefix 2
;;         corfu-auto-delay 0.01
;;         corfu-separator ?\s
;;         corfu-quit-at-boundary nil
;;         corfu-quit-no-match t
;;         corfu-preview-current nil
;;         corfu-preselect-first t
;;         corfu-on-exact-match nil
;;         corfu-echo-documentation t
;;         corfu-scroll-margin 10)
;;   (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
;;   (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
;;   (evil-make-overriding-map corfu-map)
;;   (map! :i "C-e" #'completion-at-point)
;;   :init
;;   (corfu-mode +1)
;;   (corfu-doc-mode +1)
;;   )

(after! evil-org
  (map! (:map evil-org-mode-map
         :i "C-j" nil
         :i "C-k" nil
         :i "C-;" nil
         :i "C-l" nil
         :i "<return>" nil
         :i "RET" nil)))

;; (use-package! corfu-doc
;;   :bind (:map corfu-map
;;               ("C-;" . corfu-doc-toggle)
;;               ("C-n" . corfu-doc-scroll-down)
;;               ("C-p" . corfu-doc-scroll-up))
;;   :config
;;   (setq corfu-doc-delay 0.2
;;         corfu-doc-max-width 80
;;         corfu-doc-max-height 40)
;;   :init
;;   (corfu-doc-mode +1))

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

(use-package! obsidian
  :demand t
  :config
  (obsidian-specify-path "~/Documents/notes")
  (global-obsidian-mode t))

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
