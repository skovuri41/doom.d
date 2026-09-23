;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Run 'doom sync' after editing this file, then restart Emacs (or `M-x doom/reload').

;;; Completion

(package! cape)
(package! corfu)
(package! kind-icon)
(package! consult)

;;; Editing & Navigation

(package! all-the-icons-ibuffer)
(package! dired-narrow)
(package! dired-open)
(package! git-link)
(package! imenu-list)
(package! key-chord)
(package! evil-matchit)
(package! beacon)
(package! easy-kill)
(package! aggressive-indent)
(package! outshine)
(package! tldr)

;;; Org

(package! org-autolist)
(package! org-superstar)
(package! ox-tailwind
  :recipe (:host github :repo "vascoferreira25/ox-tailwind"))

;;; Clojure

(package! cider-eval-sexp-fu)
(package! eval-sexp-fu)
(package! jet)

;;; Python

(package! virtualenvwrapper)
(package! python-black)

;;; Desktop / Notes

(package! obsidian)
(package! desktop-environment)

;;; GPTel (Claude)

(package! gptel
  :recipe (:host github :repo "karthink/gptel"))
;; Quick companion package (requires gptel)
(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick"))
