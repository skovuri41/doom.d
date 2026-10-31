;;; gtd.el -*- lexical-binding: t; -*-

(defun my/gtd-file (file) (expand-file-name file org-directory))

(after! org
  (setq org-default-notes-file (my/gtd-file "inbox.org")
        org-agenda-files (list (my/gtd-file "inbox.org") (my/gtd-file "gtd.org"))

        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "PROJ(p)" "|" "DONE(d)"))
        org-todo-keyword-faces '(("NEXT" . +org-todo-active)
                                 ("WAIT" . +org-todo-onhold)
                                 ("PROJ" . +org-todo-project))
        org-use-fast-todo-selection 'expert
        org-log-done 'time
        org-log-into-drawer t
        org-enforce-todo-dependencies t

        org-tag-alist '(("@computer" . ?c) ("@phone" . ?p)
                        ("@errand" . ?e) ("@agenda" . ?a))

        org-id-link-to-org-use-id 'create-if-interactive

        org-refile-targets `((,(my/gtd-file "gtd.org") :maxlevel . 2)
                             (,(my/gtd-file "someday.org") :maxlevel . 1))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        ;; Keep the refile-target completion list from accumulating finished
        ;; work: a DONE heading is never somewhere you still want to file a
        ;; new task under.
        org-refile-target-verify-function
        (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords)))

        org-stuck-projects '("TODO=\"PROJ\"" ("NEXT" "WAIT") nil "")

        org-capture-templates
        '(("i" "Inbox" entry (file "inbox.org") "* TODO %?\n%U\n")
          ("l" "Inbox + link" entry (file "inbox.org") "* TODO %?\n%U\n%a\n")
          ("p" "Project" entry (file+headline "gtd.org" "Projects")
           "* PROJ %^{Outcome}\n%U\n** NEXT %?\n")
          ("b" "Bookmark" entry (file "bookmarks.org") "* %?\n%U\n"))

        org-agenda-custom-commands
        '(("g" "GTD"
           ((agenda "" ((org-agenda-span 'day) (org-agenda-start-day nil)))
            (todo "NEXT" ((org-agenda-overriding-header "Next")
                          (org-agenda-todo-ignore-scheduled 'all)))
            (todo "WAIT" ((org-agenda-overriding-header "Waiting")))
            (stuck ""    ((org-agenda-overriding-header "Stuck projects")))
            (todo "TODO" ((org-agenda-overriding-header "Inbox")
                          (org-agenda-files (list org-default-notes-file))))))))

  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; "b" (Bookmark) needs no target-picking prompt, unlike the other
  ;; templates - it just wants whatever URL is on the clipboard, with its
  ;; title auto-fetched, inserted as a link. `ar/org-insert-link-dwim'
  ;; (lisp/defuns.el) already does exactly that, but it works by inserting
  ;; at point as a side effect, not by returning a string - so it can't go
  ;; inside the template text itself via %(...), which org-capture
  ;; evaluates in a throwaway temp buffer, not the real capture buffer (see
  ;; org-capture's own docstring for %(sexp)). Running the existing,
  ;; unmodified function from a capture-mode-hook instead, scoped to just
  ;; this template's key, puts it in the real buffer at the right position
  ;; (%? placed point there first) with no change to defuns.el.
  (add-hook 'org-capture-mode-hook
    (defun +org-capture-bookmark-link-h ()
      (when (equal (org-capture-get :key) "b")
        (ar/org-insert-link-dwim)))))

;; Habit tracking: a habit is a normal task with a `:STYLE: habit' property
;; and a repeating SCHEDULED date (e.g. `.+1d/3d'); the agenda then shows a
;; consistency graph instead of a plain scheduled entry. `org-habit' has to
;; be loaded explicitly - Doom's org module already handles sizing the graph
;; nicely (see +org-habit-resize-graph-h), it just needs the feature present.
;; Tip for actually using this: put habits under their own heading with
;; `:LOGGING: DONE(!)' in its property drawer (inherited by its habit
;; subtasks) so cancelling a habit doesn't log a timestamp and skew the
;; consistency graph - that's a per-file/heading convention in gtd.org
;; itself, not something to set here.
(after! org
  (require 'org-habit))

;; `K' in the agenda hides habits for the rest of the session, which is easy
;; to forget about since Emacs stays running for days - force them back on
;; every morning so a hidden habit doesn't silently stay hidden.
(run-at-time "06:00" 86400 (lambda () (setq org-habit-show-habits t)))

;; Roam templates match the existing convention (flat "${slug}.org" files,
;; no subfolders/timestamps, `:if-new' rather than `:target') instead of the
;; handover's own suggested layout - this replaces the prior single "d"
;; (immediate-finish) template entirely, per user confirmation.
(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "note" plain "%?"
           :if-new (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("r" "reference" plain "* Source\n%^{Source}\n\n* Notes\n%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+filetags: :reference:\n")
           :unnarrowed t)
          ("p" "project notes" plain
           "* Outcome\nTracked in %a\n\n* Thinking\n%?\n\n* Decisions\n"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+filetags: :project:\n")
           :unnarrowed t))))

;; "K"/"k"/"b" are already taken under org's localleader (Doom's own babel
;; result-block commands, tables prefix) - "m" (move) for kanban instead.
(use-package! org-kanban
  :after org
  :config
  (map! :map org-mode-map
        :localleader
        :desc "Kanban: move card" "m" #'org-kanban/shift))

;; One capture menu across org-capture (i/l/p) and org-roam-capture (d/r/p).
;; Roam templates all use ${title}/${slug}, so a roam pick still needs a
;; title prompt (org-roam-node-read) as a second step - there's no way
;; around that, it's inherent to how roam templates work, not a limitation
;; here. This replaces Doom's default SPC X (`org-capture` alone).
(after! (org org-roam)
  (defun my/capture ()
    "Unified capture menu: org-capture (i/l/p) plus org-roam-capture (d/r/p)."
    (interactive)
    (let* ((choices
            (append
             (mapcar (lambda (tpl) (cons (format "%s  %s" (nth 0 tpl) (nth 1 tpl)) (cons 'org (nth 0 tpl))))
                     org-capture-templates)
             (mapcar (lambda (tpl) (cons (format "%s  %s (roam)" (nth 0 tpl) (nth 1 tpl)) (cons 'roam (nth 0 tpl))))
                     org-roam-capture-templates)))
           (pick (cdr (assoc (completing-read "Capture: " (mapcar #'car choices) nil t) choices))))
      (pcase (car pick)
        ('org (org-capture nil (cdr pick)))
        ('roam (org-roam-capture- :node (org-roam-node-read) :keys (cdr pick))))))

  (map! :leader :desc "Capture (org + roam)" "X" #'my/capture))

;; Floating capture popup, for Hyprland's SUPER+SHIFT+E (see ~/.config/hypr/).
;;
;; Doom's built-in `+org-capture/open-frame' HANGS THE WHOLE DAEMON on this
;; pgtk/Wayland build - it calls `(make-frame ...)' directly from inside an
;; --eval-only client, which hangs at the GTK/Wayland level creating the
;; surface (confirmed via `hyprctl clients -j' never showing a mapped
;; window, while emacsclient processes piled up unable to get through).
;;
;; Fix: go through a *separate* nested `emacsclient --create-frame'
;; subprocess instead (the same proven path `+emacs-float' uses, and that
;; plain SUPER+E uses) - this still gets Doom's own finish/abort/refile
;; cleanup for free, since that only keys off the frame's `name'/`transient'
;; parameters (`+org-capture-frame-p'), not how the frame was created.
;;
;; New here vs. `+emacs-float': captures the window focused before the
;; popup opened and restores focus to it after, via `hl.dsp.focus'.
(after! org
  (defvar +org-capture-hypr-origin nil
    "Hyprland window address to restore focus to once the capture popup closes.")

  (defun +org-capture-hypr--call (&rest args)
    "Run a program with ARGS, returning its stdout as a string."
    (with-temp-buffer
      (apply #'call-process (car args) nil t nil (cdr args))
      (buffer-string)))

  (defun +org-capture-hypr--active-window-address ()
    "Return the address of the currently active Hyprland window.

If this ever throws `(error \"*ERROR*: JSON readtable error: 67\")',
it's not a Lisp bug here: the Emacs daemon's `HYPRLAND_INSTANCE_SIGNATURE'
has gone stale relative to the live Hyprland instance (e.g. Hyprland
reloaded/restarted without the daemon restarting), so `hyprctl' can't
find its socket and prints a connect-error string instead of JSON.
Fix live without restarting the daemon (loses no state):
  emacsclient --eval \"(setenv \\\"HYPRLAND_INSTANCE_SIGNATURE\\\" \\\"$HYPRLAND_INSTANCE_SIGNATURE\\\")\"
run from a shell in the *current* Hyprland session. Same env is read by
`+emacs-float' (lisp/emacs-float.el), so that binding breaks the same way
at the same time."
    (require 'json)
    (alist-get 'address
               (json-read-from-string
                (+org-capture-hypr--call "hyprctl" "-j" "activewindow"))))

  (defun +org-capture-hypr--restore-focus-h (&rest _)
    "Dispatch focus back to the window active before the capture popup opened."
    (when +org-capture-hypr-origin
      (call-process "hyprctl" nil nil nil "dispatch"
                    (format "hl.dsp.focus({ window = %S })"
                            (concat "address:" +org-capture-hypr-origin)))
      (setq +org-capture-hypr-origin nil)))

  (add-hook 'org-capture-after-finalize-hook #'+org-capture-hypr--restore-focus-h)

  ;; Purely cosmetic: once `my/capture' opens a real entry (an indirect
  ;; buffer of inbox.org/gtd.org, in org-mode), Doom's tabs module groups it
  ;; with every other org file already open elsewhere, cluttering the
  ;; popup's tab bar with buffers that have nothing to do with this capture.
  ;; Scoped to `+org-capture-frame-p' (Doom's own capture-popup-frame check)
  ;; so normal in-session captures elsewhere are unaffected - only the
  ;; floating popup gets the tab-free look.
  (add-hook 'org-capture-mode-hook
    (defun +org-capture-hypr--hide-tabs-h ()
      (when (and (+org-capture-frame-p) (bound-and-true-p centaur-tabs-mode))
        (centaur-tabs-local-mode 1))))

  ;; Doom's `+org-capture-cleanup-frame-h' only fires via `org-capture-
  ;; after-finalize-hook'/`org-capture-refile' - it doesn't cover the popup
  ;; being closed directly (Hyprland close bind, clicking X) mid-capture,
  ;; which would otherwise abandon org-capture's indirect buffer and state.
  ;;
  ;; Root cause of the original hang here was NOT `org-capture-kill' - it
  ;; was `+workspaces-delete-associated-workspace-h' (an earlier `delete-
  ;; frame-functions' hook, from `:ui workspaces') mistaking this frame for
  ;; the main workspace's frame and killing the whole workspace, prompting
  ;; `save-some-buffers' on every buffer in the session. Fixed by tagging
  ;; this frame with a `workspace' value that can never match a real one
  ;; (see `+org-capture-float' below). The save-some-buffers/y-or-n-p
  ;; override here stays as defense-in-depth.
  ;;
  ;; A second gap: closing the popup while still at `my/capture''s template
  ;; picker (before `org-capture-mode' exists) orphans that `completing-
  ;; read' instead - the popup frame didn't own its own minibuffer, so
  ;; deleting it didn't end the read. Fixed by giving the frame `(minibuffer
  ;; . t)' at creation, plus a dedicated `catch'/`throw' tag
  ;; (`+org-capture-float-abort') to unstick the read, deferred via
  ;; `run-at-time 0' since this hook runs nested inside `delete-frame'
  ;; itself - throwing from it directly interrupts `delete-frame''s own
  ;; teardown and leaves the frame/window alive but abandoned.
  ;;
  ;; Do NOT use `abort-recursive-edit'/`top-level' for this - tried first,
  ;; and it unwinds to whatever recursive-edit context is globally nearest,
  ;; which turned out to be server.el's own dispatch loop, corrupting it
  ;; ("Process server <N> not running: deleted", frame count climbing). A
  ;; named `catch'/`throw' pair can only ever unwind to its own exact tag,
  ;; so it's safe no matter what else is on the stack.
  (add-hook! 'delete-frame-functions
    (defun +org-capture-hypr--finalize-on-manual-close-h (frame)
      (with-selected-frame frame
        (when (+org-capture-frame-p)
          (if (bound-and-true-p org-capture-mode)
              (progn
                (let ((buf (window-buffer (frame-selected-window frame))))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf (set-buffer-modified-p nil))))
                (cl-letf (((symbol-function 'save-some-buffers) #'ignore)
                          ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                          ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                  (ignore-errors (org-capture-kill))))
            ;; Still at the picker - unstick `my/capture''s read via the
            ;; dedicated tag, deferred to the next event-loop turn so
            ;; `delete-frame' finishes its own teardown first (see comment
            ;; above the hook).
            (run-at-time 0 nil
                         (lambda ()
                           (ignore-errors
                             (throw '+org-capture-float-abort nil)))))
          (+org-capture-hypr--restore-focus-h)))))

  (defun +org-capture-float-init ()
    "Run inside the freshly created popup frame: go straight into the
unified capture menu.

Uses a fresh buffer instead of Doom's own `(doom-fallback-buffer)' (what
`+org-capture/open-frame' uses to avoid a window split) because `:ui
dashboard' redefines that to literally return the dashboard buffer.

C-g here cancels the template/title picker before any real capture session
starts, so `org-capture-after-finalize-hook' never fires to clean up -
caught explicitly instead. Aborting an actual capture buffer once one is
open is unaffected (that's the existing C-c C-k).

Re-tags this frame's `workspace' parameter here, not at creation, because
persp-mode's own frame-setup hook overwrites whatever `--frame-parameters'
set, running after frame creation but before this `--eval'. Also kills the
stray perspective that same hook creates in the process (via
`+workspace-kill', not `+workspace/kill' which would also switch frames) -
see `+org-capture-float' below for why the frame needs its own `workspace'
tag at all.

Clears `+org-capture-hypr--pending' - the frame is now real and visible in
`(frame-list)'."
    (setq +org-capture-hypr--pending nil)
    (let ((stray (frame-parameter (selected-frame) 'workspace)))
      (set-frame-parameter (selected-frame) 'workspace "*capture-popup*")
      (when (and stray (not (equal stray "main")) (+workspace-exists-p stray))
        (ignore-errors (+workspace-kill stray t))))
    (switch-to-buffer (get-buffer-create "*org-capture-float*"))
    ;; Purely cosmetic, same reasoning as +emacs-float-init's identical call:
    ;; hides the tab bar for this blank picker buffer itself. The real
    ;; capture buffer `my/capture' opens next (an indirect buffer of
    ;; inbox.org/gtd.org, in org-mode) is handled separately by
    ;; `+org-capture-hypr--hide-tabs-h' below, since Doom's tabs module
    ;; groups by major mode and this one starts in fundamental-mode.
    (when (bound-and-true-p centaur-tabs-mode)
      (centaur-tabs-local-mode 1))
    (letf! ((#'pop-to-buffer #'switch-to-buffer))
      (condition-case nil
          ;; `+org-capture-float-abort': the dedicated catch tag
          ;; `+org-capture-hypr--finalize-on-manual-close-h' throws to when
          ;; the popup closes at the bare picker (see comment on that hook).
          (catch '+org-capture-float-abort (my/capture))
        (quit
         (+org-capture-hypr--restore-focus-h)
         (delete-frame)))))

  (defun +org-capture-float--existing-frame ()
    "Return the live capture-popup frame, if one is already open."
    (cl-find-if (lambda (f)
                  (and (equal (frame-parameter f 'name)
                              (alist-get 'name +org-capture-frame-parameters))
                       (frame-parameter f 'transient)))
                (frame-list)))

  (defvar +org-capture-hypr--pending nil
    "Timestamp (`float-time') set when a new capture popup is requested,
cleared once `+org-capture-float-init' runs inside it.

The frame is created by an async, fire-and-forget nested `emacsclient
--create-frame', which takes a moment to register with the compositor -
`+org-capture-float--existing-frame''s `(frame-list)' check can't see it
during that window, so a fast double-press would spawn a duplicate instead
of focusing the first. The 5s cutoff in `+org-capture-hypr--pending-p' is
just a safety net in case `+org-capture-float-init' never runs.

Mirrors `+emacs-float--pending' (lisp/emacs-float.el) - same race, same
fix.")

  (defun +org-capture-hypr--pending-p ()
    "Non-nil if a capture popup was very recently requested and may not
have finished registering with the compositor yet."
    (and +org-capture-hypr--pending
         (< (- (float-time) +org-capture-hypr--pending) 5)))

  ;;;###autoload
  (defun +org-capture-float ()
    "Open a floating Emacs frame straight into the unified capture menu,
via a nested `emacsclient --create-frame' (the proven `+emacs-float'
pattern), not Doom's `+org-capture/open-frame' (which hangs this build -
see the comment above). Restores focus to whichever Hyprland window was
active when invoked, once capture finishes or is aborted.

If a capture popup is already open, focuses that instead of spawning a
second one. If one was *just* requested but hasn't finished opening yet,
does nothing rather than racing a second frame into existence - see
`+org-capture-hypr--pending' for why the existing-frame check alone isn't
enough for a fast repeat press."
    (interactive)
    (cond
     ((+org-capture-float--existing-frame)
      (select-frame-set-input-focus (+org-capture-float--existing-frame)))
     ((+org-capture-hypr--pending-p)
      (message "org-capture-float: still opening..."))
     (t
      (setq +org-capture-hypr--pending (float-time))
      (setq +org-capture-hypr-origin (+org-capture-hypr--active-window-address))
      ;; `workspace': tags the frame so it's never mistaken for the main
      ;; workspace's frame on close (see the `delete-frame-functions' hook
      ;; above). `minibuffer': must be set here, at creation - there's no
      ;; `set-frame-parameter' equivalent after the fact - so the frame owns
      ;; its own read instead of sharing the daemon's default minibuffer
      ;; frame (which is what orphaned the picker's `completing-read').
      (call-process "emacsclient" nil 0 nil
                    "--create-frame" "--frame-parameters"
                    (format "%S" (list (cons 'name (alist-get 'name +org-capture-frame-parameters))
                                       (cons 'transient (alist-get 'transient +org-capture-frame-parameters))
                                       (cons 'workspace "*capture-popup*")
                                       (cons 'minibuffer t)))
                    "--eval"
                    "(+org-capture-float-init)")))))
