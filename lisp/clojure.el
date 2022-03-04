(use-package! cider
  :config
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

  (defun doom/cider-send-last-sexp-to-repl ()
    "Send last sexp to REPL and evaluate it without changing
the focus."
    (interactive)
    (doom//cider-eval-in-repl-no-focus (cider-last-sexp)))

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
