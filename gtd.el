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

;; Floating capture popup, for Hyprland's SUPER+X (see ~/.config/hypr/).
;;
;; Doom's org module ships a built-in "external capture frame" feature for
;; exactly this (`+org-capture/open-frame', modules/lang/org/autoload/
;; org-capture.el) - a first attempt at this built directly on top of it,
;; via its `+org-capture-fn' override hook. That HUNG THE ENTIRE DAEMON on
;; this pgtk/Wayland build, every time: `+org-capture/open-frame' creates
;; its frame by calling `(make-frame +org-capture-frame-parameters)'
;; directly - from inside an already-connected `--eval'-only client, with
;; hardcoded `window-system'/`display' parameters - and that hung
;; indefinitely creating the actual Wayland surface. Because the hang was
;; at the GTK/toolkit level, not the Lisp level, it froze the whole Emacs
;; process, not just that one frame - confirmed by `hyprctl clients -j'
;; never showing a mapped window at all, while `emacsclient' piled up one
;; stuck process per SUPER+X press, none able to get through.
;;
;; `+emacs-float' (lisp/emacs-float.el) never has this problem, because it
;; creates its popup a different way: a *separate*, nested `emacsclient
;; --create-frame' subprocess, going through emacsclient's own well-tested
;; frame-creation RPC - the exact same path as the plain SUPER+E binding.
;; This reuses that proven path instead, while still keeping Doom's own
;; (pure Lisp, not the problem) cleanup machinery working: the nested
;; frame is given the same `name'/`transient' parameters
;; `+org-capture-frame-parameters' uses, which is all `+org-capture-frame-p'
;; (and therefore `+org-capture-cleanup-frame-h') checks to recognize a
;; capture frame - so Doom's existing finish/abort/refile cleanup still
;; applies, even though `+org-capture/open-frame' itself is never called.
;;
;; What's new here, carried over from the `+emacs-float' investigation, is
;; Hyprland-specific: capturing the window focused before the popup opened
;; and restoring focus to it once done, via the same `hl.dsp.focus'
;; dispatch mechanism. The `+emacs-float' race condition itself (wtype
;; dropping the first several ms of input unless warmed up in the same
;; process invocation) doesn't apply here - there's no wtype call in this
;; path, since the destination is an org file, not the origin window - but
;; the discipline that fix came from still applies: do the whole "capture
;; origin, act, restore origin" sequence as one coherent unit.
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

  ;; Doom's own `+org-capture-cleanup-frame-h' only fires via
  ;; `org-capture-after-finalize-hook' and `org-capture-refile' - it doesn't
  ;; cover closing the popup window directly (Hyprland's own close bind,
  ;; clicking the X) while a capture is mid-flight, which would otherwise
  ;; abandon org-capture's indirect buffer and staging state instead of
  ;; cleanly finalizing it. This finalizes it first in that case.
  ;;
  ;; `ignore-errors' alone is NOT enough here - confirmed live 2026-09-28,
  ;; traced with an instrumented backtrace (not guessed): closing the popup
  ;; externally hit a real, blocking "Save file .../inbox.org? (y, n, !,
  ;; ...)" prompt with nobody there to answer it, hanging the daemon
  ;; indefinitely - which then silently broke `kill-emacs'/`erestart' the
  ;; next time either ran, since it's the same command loop.
  ;;
  ;; The ACTUAL source surprised us: not `org-capture-kill' at all. It was
  ;; `+workspaces-delete-associated-workspace-h', an EARLIER function on
  ;; this same `delete-frame-functions' hook (Doom's own, from `:ui
  ;; workspaces'/persp-mode) - it saw this frame tagged with the *main*
  ;; workspace (every new frame inherits the current one by default, and
  ;; the nested `emacsclient --create-frame' call below never overrode it),
  ;; concluded the main workspace's frame had just closed, and called
  ;; `+workspace/kill' on it - which runs `save-some-buffers' across EVERY
  ;; buffer in the *whole session*, not just this popup's. The real fix is
  ;; where the frame is created, below: giving it an explicit `workspace'
  ;; frame parameter that can never match a real workspace name, so that
  ;; hook's guard condition is never true for this frame in the first
  ;; place. `org-capture-kill' was never actually the culprit - but this
  ;; hardening (the save-some-buffers/y-or-n-p/yes-or-no-p override and
  ;; marking the buffer unmodified first) stays as defense-in-depth in case
  ;; it ever does raise its own prompt via some other path.
  ;; A THIRD gap, found live 2026-09-28 (reported as "so many frames open"
  ;; after repeated SUPER+SHIFT+E + SUPER+Q): everything above only fires
  ;; once `org-capture-mode' is genuinely active - i.e. once a template has
  ;; actually been picked. Close the window while STILL at `my/capture''s
  ;; own template-picker `completing-read' (the default state right after
  ;; opening, before typing anything) and neither branch below's guard is
  ;; true, so nothing aborts that read - it's simply orphaned. Since the
  ;; popup frame doesn't have its own minibuffer (confirmed live: the
  ;; picker's `*Minibuf-1*' lives on the base "F1" frame, shared across all
  ;; client frames), deleting the popup frame does NOT end the minibuffer
  ;; read on its own the way it would if the frame owned it - the daemon's
  ;; single command loop just sits blocked on it forever. Every later
  ;; request (including `emacsclient-safe''s own readiness probe) then
  ;; times out too; `emacsclient-safe' sees that as "down", tries
  ;; `systemctl start' (a no-op - the unit is already active, just wedged),
  ;; and eventually falls through to its own `-a' invocation, which sees an
  ;; unresponsive-but-technically-alive socket and can self-fork yet
  ;; ANOTHER daemon on top - compounding into multiple simultaneous popups
  ;; across multiple actual processes, not just one hung window.
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
            ;; Still at the picker - nothing to finalize, just unstick the
            ;; orphaned read and restore focus ourselves (there's no real
            ;; capture to fire `org-capture-after-finalize-hook' this time).
            (when (active-minibuffer-window)
              (ignore-errors
                (with-current-buffer (window-buffer (active-minibuffer-window))
                  (abort-recursive-edit))))
            (+org-capture-hypr--restore-focus-h))))))

  (defun +org-capture-float-init ()
    "Run inside the freshly created popup frame: go straight into the
unified capture menu. org-capture's own buffer display normally splits the
window to show its target alongside whatever was already there, which is
why this otherwise showed a split. Doom's own `+org-capture/open-frame'
fixes that by switching to `(doom-fallback-buffer)' first and temporarily
making `pop-to-buffer' behave like `switch-to-buffer' (single window, no
split) - the second half is reused below, but NOT the first: the
`:ui dashboard' module (enabled here) redefines `doom-fallback-buffer' to
literally return the dashboard buffer (`doom-fallback-buffer-name' gets
set to the dashboard's name), so that call showed the dashboard where a
blank buffer was intended. A plain fresh buffer sidesteps that dashboard
override entirely, whether or not it's enabled.

C-g here (Emacs's normal abort key) already works to cancel the template
picker or org-roam's title prompt - `my/capture' hasn't started any real
capture session yet at that point, so there's nothing for
`org-capture-after-finalize-hook' to fire on, and the popup would
otherwise sit open and empty. Catch that quit signal and clean up
manually instead. (Aborting an actual capture buffer, once one is open,
is unaffected - that's the existing C-c C-k, which already goes through
the finalize hook correctly.)

Also (re-)tags this frame with a dummy `workspace' parameter, overwriting
whatever persp-mode already set - confirmed live 2026-09-28 that the
`workspace' cons passed via `+org-capture-float''s own `--frame-parameters'
does NOT survive: persp-mode's own frame-setup hook stamps every new frame
with the CURRENT workspace immediately after creation, running after
`--frame-parameters' is applied but before this `--eval' runs, so this is
the only point late enough to actually stick. See
`+org-capture-hypr--finalize-on-manual-close-h' for why this matters."
    (set-frame-parameter (selected-frame) 'workspace "*capture-popup*")
    (switch-to-buffer (get-buffer-create "*org-capture-float*"))
    (letf! ((#'pop-to-buffer #'switch-to-buffer))
      (condition-case nil
          (my/capture)
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

  ;;;###autoload
  (defun +org-capture-float ()
    "Open a floating Emacs frame straight into the unified capture menu,
via a nested `emacsclient --create-frame' (the proven `+emacs-float'
pattern), not Doom's `+org-capture/open-frame' (which hangs this build -
see the comment above). Restores focus to whichever Hyprland window was
active when invoked, once capture finishes or is aborted.

If a capture popup is already open, focuses that instead of spawning a
second one - pressing SUPER+X back-to-back otherwise stacks independent
popups while overwriting the single `+org-capture-hypr-origin' each one
reads on finish, so whichever finishes first restores focus to the
wrong window instead of where you actually started."
    (interactive)
    (if-let* ((existing (+org-capture-float--existing-frame)))
        (select-frame-set-input-focus existing)
      (setq +org-capture-hypr-origin (+org-capture-hypr--active-window-address))
      ;; `workspace' here is NOT cosmetic - see `+org-capture-hypr--finalize-
      ;; on-manual-close-h' below for why a frame with no explicit workspace
      ;; of its own is dangerous: it silently inherits the CURRENT one
      ;; (persp-mode tags every new frame with it by default), and closing
      ;; it externally then reads as "the main workspace's frame just
      ;; closed" to Doom's own `+workspaces-delete-associated-workspace-h'
      ;; (also on `delete-frame-functions') - which kills the ENTIRE main
      ;; workspace in response, `save-some-buffers'-prompting on every
      ;; buffer in it. A value that can never match a real workspace name
      ;; keeps this frame's closing from ever being mistaken for that.
      (call-process "emacsclient" nil 0 nil
                    "--create-frame" "--frame-parameters"
                    (format "%S" (list (cons 'name (alist-get 'name +org-capture-frame-parameters))
                                       (cons 'transient (alist-get 'transient +org-capture-frame-parameters))
                                       (cons 'workspace "*capture-popup*")))
                    "--eval"
                    "(+org-capture-float-init)"))))
