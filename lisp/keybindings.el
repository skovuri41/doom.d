;;; lisp/keybindings.el -*- lexical-binding: t; -*-
(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)
(define-key isearch-mode-map (kbd "M-j") 'isearch-yank-word-or-char)

(map! :nir "C-d"  #'lispy-delete)
(map! :leader (:prefix "w"
               :desc "delete other windows" "o" #'delete-other-windows))
(map! :leader (:prefix "b"
               :desc "Delete Other Windows" "o" #'delete-other-windows))
write-file
(bind-key "<f11>" #'xah-previous-user-buffer)
(bind-key "<f12>" #'xah-next-user-buffer)
(bind-key "<C-f11>" #'centaur-tabs-backward)
(bind-key "<C-f12>" #'centaur-tabs-forward)

;;;;;; aliases

;; make frequently used commands short
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)
(defalias 'dup 'duplicate-thing)
;; (defalias 'max 'maximize-frame)

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'wd 'wdired-change-to-wdired-mode)

(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)
(defalias 'sbc 'set-background-color)
(defalias 'rof 'recentf-open-files)
(defalias 'lcd 'list-colors-display)
(defalias 'cc 'calc)
(defalias 'cal 'calendar)
(defalias 'wtime 'display-time-world)
(defalias 'of 'other-frame)

;; Macro
(defalias 'ms 'start-kbd-macro)
(defalias 'me 'end-kbd-macro)
(defalias 'ml 'call-last-kbd-macro)

;; ; elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)
(defalias 'lf 'load-file)

;; ; major modes
(defalias 'hm 'html-mode)
(defalias 'tm 'text-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)

(defalias 'setnu 'display-line-numbers-mode)

(defalias 'dv 'describe-variable)
(defalias 'df 'describe-function)
(defalias 'dk 'describe-key)
(defalias 'db 'describe-buffer)
(defalias 'dm 'describe-keymap)

(defalias 'fnd 'find-name-dired)
(defalias 'ne 'next-error)
(defalias 'pe 'previous-error)
(defalias 'pd 'projectile-dired)
(defalias 'pff 'projectile-find-file)
(defalias 'pfd 'projectile-find-dir)
(defalias 'psp 'projectile-switch-project)

;; ;pretty print
(defalias 'ppx'pretty-print-xml-region)
(defalias 'ppj 'beautify-json)

(set-register ?i '(file . "~/.doom.d/init.el"))
