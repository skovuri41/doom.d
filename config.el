;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Shyam Kovuri"
      user-mail-address "shyam32@fastmail.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-big-font (font-spec :family "JetBrains Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
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

(setq doom-theme 'doom-one)
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

(load! "lisp/keybindings.el")
(load! "lisp/defuns.el")

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
          ("\\.pdf\\'" . "zathura %s"))))

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
        ("pdf" . "zathura")))
  (setq dired-open-extensions open-extensions))
