(use-package clojure-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.edn$'" . clojure-mode))
  :config
  (progn
    (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
    (add-hook 'before-save-hook 'cider-format-buffer t t)
    (setq clojure-align-forms-automatically t)
    (setq clojure-toplevel-inside-comment-form t)

    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2)
      (let-routes 1))

    (define-clojure-indent
      (form-to 1))

    (define-clojure-indent
      (match 1)
      (are 2)
      (checking 2)
      (async 1))

    (define-clojure-indent
      (select 1)
      (insert 1)
      (update 1)
      (delete 1))

    (define-clojure-indent
      (run* 1)
      (fresh 1))

    (define-clojure-indent
      (extend-freeze 2)
      (extend-thaw 1))

    (define-clojure-indent
      (go-loop 1))

    (define-clojure-indent
      (this-as 1)
      (specify 1)
      (specify! 1))

    (define-clojure-indent
      (s/fdef 1))

    (define-clojure-indent
      (rf/reg-event-db 1)
      (rf/reg-event-fx 1)
      (rf/reg-sub 1)
      (rf/reg-fx 1)
      (rf/reg-cofx 1))

    (setq clojure--prettify-symbols-alist
          '(("fn" . ?λ)
            (">=" . ?≥)
            ("<=" . ?≤)
            ("not=" . ?≠)
            ("identical?" . ?≡)))

    (defun clojure-fancify-symbols (mode)
      "Pretty symbols for Clojure's anonymous functions and sets,
       like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
      (font-lock-add-keywords mode
                              `(("(\\(fn\\)[\n\[[:space:]]"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "λ"))))
                                ("(\\(partial\\)[\[[:space:]]"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "Ƥ"))))
                                ("(\\(comp\\)[\n\[[:space:]]"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "∘"))))
                                ("\\(#\\)("
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "ƒ"))))
                                ("\\(#\\){"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1) "∈")))))))

    (dolist (m '(clojure-mode clojurescript-mode clojurec-mode clojurex-mode cider-mode cider-repl-mode))
      (clojure-fancify-symbols m))


    (add-hook 'clojurescript-mode-hook
              '(lambda ()
                 (add-to-list 'imenu-generic-expression
                              '("re-frame" "(reg-\\(event-fx\\|event-db\\|sub\\)[ \\|\n]*\\(:[^ \\|\n]*\\)" 2) t)))

    (after! all-the-icons
      (add-to-list 'all-the-icons-icon-alist
                   '("\\.edn$"
                     all-the-icons-alltheicon
                     "clojure"
                     :height 1.0
                     :face all-the-icons-lyellow
                     :v-adjust 0.0))
      (add-to-list 'all-the-icons-icon-alist
                   '("\\.cljc?$"
                     all-the-icons-alltheicon
                     "clojure"
                     :height 1.0
                     :face all-the-icons-lgreen
                     :v-adjust 0.0))
      (add-to-list 'all-the-icons-icon-alist
                   '("\\.cljs$"
                     all-the-icons-fileicon
                     "cljs"
                     :height 1.0
                     :face all-the-icons-lgreen
                     :v-adjust 0.0))))
  (defun clerk-show ()
    (interactive)
    (save-buffer)
    (let
        ((filename
          (buffer-file-name)))
      (when filename
        (cider-interactive-eval
         (concat "(nextjournal.clerk/show! \"" filename "\")")))))

  ;; (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)
  )

(use-package! cider
  :init
  :config
  (setq cider-print-fn 'fipp)
  (setq cider-test-show-report-on-success t)
  (setq cider-repl-display-help-banner nil)
  (add-hook
   'cider-popup-buffer-mode-hook
   '(lambda ()
      (local-set-key "\C-g" 'cider-popup-buffer-quit)))
  (defun doom//cider-eval-in-repl-no-focus (form)
    "Insert FORM in the REPL buffer and eval it."
    (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
      (setq form (replace-match "" t t form)))
    (with-current-buffer (cider-current-repl-buffer)
      (let ((pt-max (point-max)))
        (goto-char pt-max)
        (insert form)
        (indent-region pt-max (point))
        (cider-repl-return))))

  (use-package! cider-eval-sexp-fu
    :after (clojure-mode cider))

  (defun doom/cider-send-last-sexp-to-repl ()
    "Send last sexp to REPL and evaluate it without changing
the focus."
    (interactive)
    (doom/cider-clear-repl-buffer)
    ;; (lispyville-end-of-defun)
    ;; (evil-next-close-paren)
    (evil-append 1)
    (just-one-space)
    (evil-force-normal-state)
    (doom//cider-eval-in-repl-no-focus (cider-last-sexp))
    (delete-horizontal-space)
    (save-buffer))

  (defun doom/cider-send-last-sexp-to-repl-focus ()
    "Send last sexp to REPL and evaluate it and switch to the REPL in
`insert state'."
    (interactive)
    (cider-insert-last-sexp-in-repl t)
    (evil-insert-state))

  (defun doom/cider-send-region-to-repl (start end)
    "Send region to REPL and evaluate it without changing
the focus."
    (interactive "r")
    (doom/cider-clear-repl-buffer)
    (doom//cider-eval-in-repl-no-focus
     (buffer-substring-no-properties start end)))

  (defun doom/cider-send-region-to-repl-focus (start end)
    "Send region to REPL and evaluate it and switch to the REPL in
`insert state'."
    (interactive "r")
    (cider-insert-in-repl
     (buffer-substring-no-properties start end) t)
    (evil-insert-state))

  (defun doom/cider-send-function-to-repl ()
    "Send current function to REPL and evaluate it without changing
the focus."
    (interactive)
    (doom/cider-clear-repl-buffer)
    (doom//cider-eval-in-repl-no-focus (cider-defun-at-point)))

  (defun doom/cider-send-function-to-repl-focus ()
    "Send current function to REPL and evaluate it and switch to the REPL in
insert state'."
    (interactive)
    (cider-insert-defun-in-repl t)
    (evil-insert-state))

  (defun doom/cider-send-ns-form-to-repl ()
    "Send buffer's ns form to REPL and evaluate it without changing
the focus."
    (interactive)
    (doom/cider-clear-repl-buffer)
    (doom//cider-eval-in-repl-no-focus (cider-ns-form)))

  (defun doom/cider-send-ns-form-to-repl-focus ()
    "Send ns form to REPL and evaluate it and switch to the REPL in
`insert state'."
    (interactive)
    (cider-insert-ns-form-in-repl t)
    (evil-insert-state))

  (defun doom/cider-send-buffer-in-repl-and-focus ()
    "Send the current buffer in the REPL and switch to the REPL in
`insert state'."
    (interactive)
    (cider-load-buffer)
    (cider-switch-to-repl-buffer)
    (evil-insert-state))

  (defun cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user"))

  (defun toggle-nrepl-buffer ()
    "Toggle the nREPL REPL on and off"
    (interactive)
    (if (string-match "cider-repl" (buffer-name (current-buffer)))
        (delete-window)
      (cider-switch-to-repl-buffer)))

  (defun cider-save-and-refresh ()
    (interactive)
    (save-buffer)
    (call-interactively 'cider-refresh))

  (defun cider-send-and-evaluate-sexp ()
    "Sends the s-expression located before the point or the active
       region to the REPL and evaluates it. Then the Clojure buffer is
       activated as if nothing happened."
    (interactive)
    (if (not (region-active-p))
        (cider-insert-last-sexp-in-repl)
      (cider-insert-in-repl
       (buffer-substring (region-beginning) (region-end)) nil))
    (cider-switch-to-repl-buffer)
    (cider-repl-closing-return)
    (cider-switch-to-last-clojure-buffer))

  (defun cider-eval-defun-or-region ()
    "Eval defun at point or region when it is active"
    (interactive)
    (if (use-region-p)
        (cider-eval-region)
      (cider-eval-defun-at-point)))

  (defun doom/cider-clear-repl-buffer ()
    (interactive)
    (if (memq major-mode '(cider-repl-mode))
        (cider-repl-clear-buffer)
      (cider-find-and-clear-repl-output '(4))))

  (defun doom/cider-clear-repl-output ()
    (interactive)
    (if (memq major-mode '(cider-repl-mode))
        (cider-repl-clear-output)
      (progn
        (cider-switch-to-repl-buffer)
        (cider-repl-clear-output)
        (cider-switch-to-last-clojure-buffer)))))
