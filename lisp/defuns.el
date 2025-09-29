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

(defun imenu-lsp-ui-smart-toggle ()
  (interactive)
  (if (get-buffer-window "*lsp-ui-imenu*" t)
      (progn (select-window (get-buffer-window "*lsp-ui-imenu*"))
             (lsp-ui-imenu--kill))
    (lsp-ui-imenu)))

(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

(defun eval-n-defuns (n)
  "Evaluate N top-level forms, starting with the current one."
  (interactive "P")
  (+eval/region (car (bounds-of-thing-at-point 'defun))
                (save-excursion
                  (dotimes (_ (or n 2))
                    (end-of-defun))
                  (point))))

(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
    (if (not (bolp)) (forward-line 1))
    (delete-whitespace-rectangle (point) end nil)))

(defun suggest-project-root ()
  "Get project root."
  (or
   (when (featurep 'projectile) (condition-case nil
                                    (projectile-project-root)
                                  (error nil)))
   (when (featurep 'project)
     (when-let ((project (project-current)))
       (if (fboundp 'project-root)
           (project-root project)
         (car (with-no-warnings
                (project-roots project))))))
   default-directory))

(defun my-copy-buffer-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "File path copied: %s" buffer-file-name)))

;; Add backlinks to the export
(defun collect-backlinks-string (backend)
  (when (org-roam-node-at-point)
    (let* ((source-node (org-roam-node-at-point))
           (source-file (org-roam-node-file source-node))
           ;; Sort the nodes by the point to avoid errors when inserting the
           ;; references
           (nodes-in-file (--sort (< (org-roam-node-point it)
                                     (org-roam-node-point other))
                                  (-filter (lambda (node)
                                             (s-equals?
                                              (org-roam-node-file node)
                                              source-file))
                                           (org-roam-node-list))))
           ;; Nodes don't store the last position so, get the next node position
           ;; and subtract one character
           (nodes-start-position (-map (lambda (node) (org-roam-node-point node))
                                       nodes-in-file))
           (nodes-end-position (-concat (-map (lambda (next-node-position)
                                                (- next-node-position 1))
                                              (-drop 1 nodes-start-position))
                                        (list (point-max))))
           ;; Keep track of the current-node index
           (current-node 0)
           ;; Keep track of the amount of text added
           (character-count 0))
      (when (and nodes-in-file (s-equals? backend "latex"))
        (insert "\n\n\\clearpage\n\n"))
      (dolist (node nodes-in-file)
        (when (org-roam-backlinks-get node)
          ;; Go to the end of the node and don't forget about previously inserted
          ;; text
          (goto-char (+ (nth current-node nodes-end-position) character-count))
          ;; Add the references as a subtree of the node
          (setq heading (format "\n\n%s Backlinks\n"
                                (s-repeat (+ (org-roam-node-level node) 1) "*")))
          ;; Count the characters and count the new lines (4)
          (setq character-count (+ 3 character-count (string-width heading)))
          (insert heading)
          ;; Insert properties drawer
          (setq properties-drawer ":PROPERTIES:\n:HTML_CONTAINER_CLASS: references\n:END:\n")
          ;; Count the characters and count the new lines (3)
          (setq character-count (+ 3 character-count (string-width properties-drawer)))
          (insert properties-drawer)
          (dolist (backlink (org-roam-backlinks-get node))
            (let* ((source-node (org-roam-backlink-source-node backlink))
                   (point (org-roam-backlink-point backlink))
                   (text (s-replace "\n" " " (org-roam-preview-get-contents
                                              (org-roam-node-file source-node)
                                              point)))
                   (references (format "- [[./%s][%s]]: %s\n\n"
                                       (file-relative-name (org-roam-node-file source-node))
                                       (org-roam-node-title source-node)
                                       text)))
              ;; Also count the new lines (2)
              (setq character-count (+ 2 character-count (string-width references)))
              (insert references))))
        (setq current-node (+ current-node 1))))))

(add-hook 'org-export-before-processing-hook 'collect-backlinks-string)

(defun publish-file-and-build-toc ()
  "Force publish the current org-mode file."
  (interactive)
  (org-publish-current-file)
  (org-tailwind-build-toc))

(defun force-publish-file-and-build-toc ()
  "Force publish the current org-mode file."
  (interactive)
  (org-publish-current-file t)
  (org-tailwind-build-toc))

(defun publish-all-and-build-toc ()
  "Force publish all org-mode files."
  (interactive)
  (org-publish-all)
  (org-tailwind-build-toc))

(defun force-publish-all-and-build-toc ()
  "Force publish all org-mode files."
  (interactive)
  (org-publish-all t)
  (org-tailwind-build-toc))
