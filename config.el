;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
(setq doom-theme 'doom-one)
(setq org-directory "~/Documents/org/")
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

(load! "lisp/keybindings.el")
(load! "lisp/defuns.el")

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
