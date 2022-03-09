;;; lisp/defuns.el -*- lexical-binding: t; -*-


(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t)))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))


(defun user/eval-list-dwim ()
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (call-interactively 'eval-sexp-fu-eval-sexp-inner-list))
        ((eq major-mode 'clojure-mode)
         (call-interactively 'eval-sexp-fu-cider-eval-sexp-inner-list))))

(defun user/pprint-eval-list-dwim ()
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (call-interactively 'eval-sexp-fu-eval-sexp-inner-list))
        ((eq major-mode 'clojure-mode)
         (call-interactively 'eval-sexp-fu-cider-pprint-eval-sexp-inner-list))))

(defun user/eval-sexp-dwim ()
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (call-interactively 'eval-sexp-fu-eval-sexp-inner-sexp))
        ((eq major-mode 'clojure-mode)
         (call-interactively 'eval-sexp-fu-cider-eval-sexp-inner-sexp))))

(defun user/pprint-eval-sexp-dwim ()
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (call-interactively 'eval-sexp-fu-eval-sexp-inner-sexp))
        ((eq major-mode 'clojure-mode)
         (call-interactively 'eval-sexp-fu-cider-pprint-eval-sexp-inner-sexp))))
