;;; lisp/emacs-float.el -*- lexical-binding: t; -*-

;;; Floating "Write Anywhere" Popup

;; SUPER+ALT+E (omadots bindings.lua): floating scratch Emacs popup that
;; sends its text back to whichever window was focused before it opened.
;;
;; Tried emacs-everywhere (tecosaur/emacs-everywhere) and tinee
;; (tusharhero/tinee) here first and abandoned both, having concluded
;; auto-paste was fundamentally unachievable on this Hyprland build: neither
;; `ydotool' (uinput/evdev) nor `wtype' (Wayland virtual-keyboard protocol)
;; appeared to deliver synthetic keystrokes to the focused window, even
;; though `WAYLAND_DEBUG=1 wtype ...` showed clean, compositor-ack'd
;; requests. That conclusion was WRONG - a broken test, not a broken tool.
;; Every verification that session used a `cat > file' terminal as the
;; target, checked with no trailing newline - terminals buffer typed input
;; in canonical mode until a newline arrives, so the keystrokes were
;; genuinely delivered and just sitting unflushed in the pty, never reaching
;; `cat'. Confirmed by hand against a real (non-terminal) GUI Emacs buffer
;; target instead: `wtype' delivers text completely, every time, across
;; repeated clean runs - once one specific race is worked around (below).
;;
;; The one real bug: `wtype' has a startup race establishing its Wayland
;; virtual-keyboard connection - text sent immediately can have its first
;; several dozen milliseconds of characters silently dropped (confirmed by
;; hand: a payload came through as e.g. "yload-two-calls" instead of the
;; full string). A harmless warm-up (`-M shift -m shift' - press and
;; release Shift, no visible character) before the real payload absorbs
;; that race - but ONLY when it's part of the SAME `wtype' process/
;; connection as the real payload. Two SEPARATE `wtype' processes (a
;; throwaway warm-up call, then a second call for the real text) does NOT
;; work - each is its own independent Wayland connection, so the second
;; call hits the exact same race the first one was supposed to absorb;
;; confirmed by hand, this dropped characters intermittently even after
;; generously increasing every delay involved. Passing the modifiers AND
;; the real text to ONE SINGLE `wtype' invocation fixed it outright - 10/10
;; clean runs, single-line and multi-paragraph, right after `delete-frame'
;; + a focus-restore dispatch (the real, realistic flow, not just wtype in
;; isolation). An earlier attempt warmed up with a literal space instead of
;; a modifier press, which worked but leaked a stray leading space into the
;; target on every line (an `electric-indent-mode'/`indent-relative'
;; artifact when the test target was itself an Emacs buffer) - a modifier
;; press avoids inserting anything visible at all, in the warm-up or
;; otherwise.
;;
;; Focus-restore uses the SAME `hl.dsp.focus' Lua-dispatch mechanism fixed
;; for emacs-everywhere previously (see this file's git history) - an
;; explicit dispatch to a captured window address, confirmed reliable across
;; this whole investigation. This is NOT the same thing as tinee's approach
;; (relying on Hyprland's native refocus-on-close with no explicit dispatch
;; at all), which separately proved unreliable under rigorous testing - the
;; two are unrelated mechanisms and only one of them was ever shown to be
;; flaky.
(defun +emacs-float--call (&rest args)
  "Run a program with ARGS, returning its stdout as a string."
  (with-temp-buffer
    (apply #'call-process (car args) nil t nil (cdr args))
    (buffer-string)))

(defun +emacs-float--active-window-address ()
  "Return the address of the currently active Hyprland window."
  (require 'json)
  (alist-get 'address
             (json-read-from-string
              (+emacs-float--call "hyprctl" "-j" "activewindow"))))

(defvar-local +emacs-float-origin nil
  "Hyprland window address to send this popup buffer's text back to.")

(defun +emacs-float-wtype-send (text)
  "Type TEXT into the currently focused window via `wtype'.
A harmless Shift press/release runs first, in the SAME `wtype' process as
TEXT - see the comment above this section for why that matters."
  (call-process "wtype" nil nil nil "-M" "shift" "-m" "shift" text))

(defun +emacs-float-done ()
  "Send this buffer's text to the window +emacs-float was invoked from, then close."
  (interactive)
  (let ((text (buffer-string))
        (origin +emacs-float-origin))
    (delete-frame)
    (when origin
      (call-process "hyprctl" nil nil nil "dispatch"
                    (format "hl.dsp.focus({ window = %S })"
                            (concat "address:" origin)))
      (sleep-for 0.15))
    (+emacs-float-wtype-send text)))

(defun +emacs-float-cancel ()
  "Close the popup without sending anything."
  (interactive)
  (delete-frame))

(defun +emacs-float-init (origin)
  "Set up the just-created popup frame: fresh org-mode buffer in Evil
insert state, remembering ORIGIN to send text back to on C-c C-c.

Also (re-)tags this frame with a dummy `workspace' parameter, overwriting
whatever persp-mode already set - confirmed live 2026-09-28 (on gtd.el's
identically-patterned capture popup) that the `workspace' cons passed via
`+emacs-float''s own `--frame-parameters' does NOT survive: persp-mode's
own frame-setup hook stamps every new frame with the CURRENT workspace
immediately after creation, running after `--frame-parameters' is applied
but before this `--eval' runs, so this is the only point late enough to
actually stick. See gtd.el's `+org-capture-hypr--finalize-on-manual-
close-h' comment for why this matters - same root cause, same fix."
  (set-frame-parameter (selected-frame) 'workspace "*emacs-float-popup*")
  (let ((buf (generate-new-buffer "float")))
    (switch-to-buffer buf)
    (org-mode)
    (setq-local +emacs-float-origin origin)
    (local-set-key (kbd "C-c C-c") #'+emacs-float-done)
    (local-set-key (kbd "C-c C-k") #'+emacs-float-cancel)
    (evil-insert-state)))

;;;###autoload
(defun +emacs-float ()
  "Open a floating scratch Emacs popup; C-c C-c sends its text back to
whichever window was focused when this was invoked (C-c C-k cancels)."
  (interactive)
  (let ((origin (+emacs-float--active-window-address)))
    ;; `workspace' here is load-bearing, not cosmetic - see gtd.el's
    ;; `+org-capture-hypr--finalize-on-manual-close-h' comment for the full
    ;; story (traced live 2026-09-28): a frame with no explicit workspace of
    ;; its own inherits the CURRENT one (persp-mode tags every new frame by
    ;; default), and closing it externally then reads as "the main
    ;; workspace's frame just closed" to Doom's own
    ;; `+workspaces-delete-associated-workspace-h' (on `delete-frame-
    ;; functions'), which responds by killing the entire main workspace -
    ;; `save-some-buffers'-prompting across every buffer in the session,
    ;; not just this popup's. A value that can never match a real
    ;; workspace name keeps this frame from ever being mistaken for it.
    (call-process "emacsclient" nil 0 nil
                  "--create-frame" "--frame-parameters"
                  "((name . \"emacs-float\") (workspace . \"*emacs-float-popup*\"))"
                  "--eval"
                  (format "(+emacs-float-init %S)" origin))))
