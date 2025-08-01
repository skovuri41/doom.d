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



(map!
 (:mode (clojure-mode clojurec-mode clojurescript-mode)
  :in "C-<return> e" #'doom/cider-send-last-sexp-to-repl
  :in "C-<return> d" #'doom/cider-send-function-to-repl
  :in "C-<return> n" #'doom/cider-send-ns-form-to-repl
  :in "C-<return> r" #'doom/cider-send-region-to-repl
  :in "C-<return> c" #'user/eval-list-dwim
  :i "M-." #'cider-find-var))

(map! (:localleader
       (:map (cider-repl-mode-map)
             (:prefix ("z" . "repl2")
                      "n" #'cider-repl-set-ns
                      "q" #'cider-quit
                      "r" #'cider-ns-refresh
                      "R" #'cider-restart
                      "t" #'toggle-nrepl-buffer
                      "l" #'doom/cider-clear-repl-buffer
                      "o" #'doom/cider-clear-repl-output
                      "b" #'cider-switch-to-last-clojure-buffer
                      "B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
                      "c" #'cider-find-and-clear-repl-output))))

(map! (:localleader
       (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map cider-repl-mode-map)
             (:prefix ("x" . "xtras")
                      "s" #'cider-selector))))

(map! (:localleader
       (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
             (:prefix ("e" . "eval")
                      "." #'doom/cider-send-last-sexp-to-repl
                      "d" #'cider-eval-defun-or-region
                      "n" #'doom/cider-send-ns-form-to-repl)
             (:prefix ("r" . "repl")
                      "t" #'toggle-nrepl-buffer
                      "l" #'doom/cider-clear-repl-buffer
                      "o" #'doom/cider-clear-repl-output))))

(map!
 (:map cider-inspector-mode-map
       (:n "<tab>" #'cider-inspector-next-inspectable-object
        :n "S-<tab>" #'cider-inspector-previous-inspectable-object)))

(map!
 (:map lsp-ui-imenu-mode-map
       (:n "l" #'lsp-ui-imenu--view
        :n "<return>" #'lsp-ui-imenu--visit)))

;; (evil-define-key 'normal 'lsp-ui-imenu-mode-map
;;   (kbd "<return>") 'lsp-ui-imenu--visit
;;   (kbd "l") 'lsp-ui-imenu--view)

(after! lispy
  (setq lispy-safe-copy t
        lispy-safe-delete t
        lispy-safe-paste t)
  (lispy-define-key lispy-mode-map "t" #'lispy-different)
  (lispy-define-key lispy-mode-map "y" #'lispy-new-copy)
  (lispy-define-key lispy-mode-map "n" #'lispy-occur)
  (lispy-define-key lispy-mode-map "X" #'lispy-splice)
  (lispy-define-key lispy-mode-map "d" 'lispy-delete)
  (lispy-define-key lispy-mode-map "p" 'lispy-paste)
  (lispy-define-key lispy-mode-map "P" 'lispy-eval-other-window)
  (map! :nir "C-d" #'lispy-delete-or-splice-or-slurp)
  (map! :nv "(" #'lispy-parens)
  (map! :nv "{" #'lispy-braces)
  (map! :nv "}" #'lispy-brackets)
  (map! :nv "\"" #'lispy-quotes))

(map! :nv "C-a"  #'evil-first-non-blank)
(map! :nv "C-e"  #'evil-end-of-line)
(map! :nv "C-i" #'better-jumper-jump-forward)
(map! :nv "C-o" #'better-jumper-jump-backward)
(map! :nv "C-'" #'better-jumper-set-jump)
(define-key input-decode-map [?\C-i] [C-i])
(global-set-key (kbd "<C-i>") #'better-jumper-jump-forward)
(map! :nv "t" #'evilmi-jump-items)
(map! :nv "P" #'consult-yank-from-kill-ring)
(map! :leader (:prefix "h"
               :desc "gptel-quick" "h" #'gptel-quick))
(map! :leader (:prefix "s"
               :desc "consult-ripgrep" "g" #'consult-ripgrep))
(map! :leader (:prefix "w"
               :desc "delete-other-windows" "o" #'delete-other-windows))
(map! :leader (:prefix "b"
               :desc "write-file" "w" #'write-file))
(map! :leader (:prefix "b"
               :desc "switch-to-buffer" "l" #'consult-buffer))
(map! :leader (:prefix "f"
               :desc "dired-jump" "j" #'dired-jump))
(map! :leader (:prefix "f"
               :desc "treemacs toggle" "t" #'+treemacs/toggle))
(map! (:when (modulep! :ui treemacs)
        "<f6>" #'+treemacs/toggle
        "<C-f6>" #'treemacs-find-file))
(map! "<f9>" '(lambda () (interactive)
                (if (org-roam-buffer-p (current-buffer))
                    (org-roam-buffer-toggle)
                  (imenu-lsp-ui-smart-toggle))))
(map! :nv "gl"  #'org-down-element)
(map! :nv "g]"  #'lispyville-forward-list)
(map! :nv "g["  #'lispyville-backward-list-begin)
(map! :map org-mode-map
      "M-n" #'outline-next-visible-heading
      "M-p" #'outline-previous-visible-heading)
(bind-key "<f7>" #'xah-previous-user-buffer)
;; (bind-key "<f11>" #'better-jumper-jump-backward)
(bind-key "<M-f11>" #'evil-prev-buffer)
(bind-key "<f8>" #'xah-next-user-buffer)
;; (bind-key "<f12>" #'better-jumper-jump-forward)
(bind-key "<M-f12>" #'evil-next-buffer)
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
(defalias 'ppx 'pretty-print-xml-region)
(defalias 'ppj 'json-pretty-print)
;; org mode
(defalias 'oih 'org-insert-heading)
(defalias 'clip 'org-cliplink)
(defalias 'link 'ar/org-insert-link-dwim)

(set-register ?i '(file . "~/.doom.d/init.el"))
(set-register ?c '(file . "~/.doom.d/config.el"))
