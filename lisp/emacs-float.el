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

(defvar-local +emacs-float--finishing nil
  "Set non-nil right before `+emacs-float-done'/`+emacs-float-cancel' call
`delete-frame' themselves, so the `delete-frame-functions' safety net
below can tell this expected self-close apart from an external one
(SUPER+Q, clicking the window's close button) and skip redundant/
conflicting cleanup on the former. See that hook's docstring for why an
external close needs handling here at all.")

(defun +emacs-float--restore-focus (origin)
  "Dispatch focus back to hyprland window address ORIGIN, if non-nil."
  (when origin
    (call-process "hyprctl" nil nil nil "dispatch"
                  (format "hl.dsp.focus({ window = %S })"
                          (concat "address:" origin)))
    (sleep-for 0.15)))

(defun +emacs-float-done ()
  "Send this buffer's text to the window +emacs-float was invoked from, then close."
  (interactive)
  (let ((text (buffer-string))
        (origin +emacs-float-origin)
        (buf (current-buffer)))
    (setq +emacs-float--finishing t)
    (delete-frame)
    (kill-buffer buf)
    (+emacs-float--restore-focus origin)
    (+emacs-float-wtype-send text)))

(defun +emacs-float-cancel ()
  "Close the popup without sending anything."
  (interactive)
  (let ((buf (current-buffer)))
    (setq +emacs-float--finishing t)
    (delete-frame)
    (kill-buffer buf)))

(add-hook! 'delete-frame-functions
  (defun +emacs-float--cleanup-on-manual-close-h (frame)
    "`+emacs-float-done'/`+emacs-float-cancel' are ordinary commands bound
only to C-c C-c/C-c C-k - an external close (SUPER+Q, clicking the
window's close button) never runs them at all, since the window manager
calls `delete-frame' directly instead. Without this hook that means an
external close silently skips the buffer-kill fix above (leaking a
`float' buffer again, just through a different door) and skips the
explicit focus-restore dispatch too, falling back to Hyprland's own
native refocus-on-close - already documented at the top of this file as
separately proved unreliable for that exact purpose.

Treat an external close like C-c C-k (cancel), not C-c C-c (send): don't
type a possibly-unfinished scratch note into the origin window just
because the popup was dismissed via the window manager rather than from
inside it.

Checks `+emacs-float--finishing' to skip this entirely when the frame is
closing via `+emacs-float-done'/-cancel's own `delete-frame' call, which
also runs this hook - both of those commands already handle their own
cleanup, so redundant/conflicting handling here would double-dispatch a
focus-restore and try to kill an already-dead buffer."
    (when (equal (frame-parameter frame 'name) "emacs-float")
      (let* ((win (frame-selected-window frame))
             (buf (and (window-live-p win) (window-buffer win))))
        (when (and buf (not (buffer-local-value '+emacs-float--finishing buf)))
          (+emacs-float--restore-focus (buffer-local-value '+emacs-float-origin buf))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

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
close-h' comment for why this matters - same root cause, same fix.

Also cleans up the STALE PERSPECTIVE that frame-setup hook creates - see
gtd.el's `+org-capture-float-init' comment for the full mechanism (found
live 2026-09-29, same root cause here: `+workspaces-associate-frame-fn'
creates a real, freshly-numbered perspective for every new frame since
this daemon always has other frames already, and simply overwriting the
frame parameter afterward left it behind, orphaned and accumulating).

Also clears `+emacs-float--pending' - this frame is now real and will
show up in `(frame-list)', so the guard has done its job."
  (setq +emacs-float--pending nil)
  (let ((stray (frame-parameter (selected-frame) 'workspace)))
    (set-frame-parameter (selected-frame) 'workspace "*emacs-float-popup*")
    (when (and stray (not (equal stray "main")) (+workspace-exists-p stray))
      (ignore-errors (+workspace-kill stray t))))
  (let ((buf (generate-new-buffer "float")))
    (switch-to-buffer buf)
    (org-mode)
    ;; Purely cosmetic: Doom's `tabs' module (centaur-tabs) groups buffers by
    ;; major mode, so this org-mode scratch buffer's tab bar would otherwise
    ;; show every other org file already open elsewhere in the session, not
    ;; just itself. `centaur-tabs-local-mode' hides the tab bar for just this
    ;; buffer, independent of anything else open.
    (when (bound-and-true-p centaur-tabs-mode)
      (centaur-tabs-local-mode 1))
    (setq-local +emacs-float-origin origin)
    (local-set-key (kbd "C-c C-c") #'+emacs-float-done)
    (local-set-key (kbd "C-c C-k") #'+emacs-float-cancel)
    (evil-insert-state)))

(defun +emacs-float--existing-frame ()
  "Return the live float-popup frame, if one is already open."
  (cl-find-if (lambda (f)
                (and (equal (frame-parameter f 'name) "emacs-float")
                     (frame-parameter f 'transient)))
              (frame-list)))

(defvar +emacs-float--pending nil
  "Timestamp (`float-time') set the instant a new popup frame is requested,
cleared once `+emacs-float-init' actually runs inside it - nil otherwise.

The 2026-09-29 existing-frame check above isn't sufficient by itself:
`+emacs-float--open' spawns the real frame via an ASYNCHRONOUS, fire-
and-forget nested `emacsclient --create-frame' subprocess (so the outer
`--eval' returns immediately), and that subprocess takes a non-zero,
variable amount of time to actually register the frame with the
compositor. Confirmed live (2026-09-29): firing two `+emacs-float' calls
back-to-back with NO gap - a fast real double-press, not the sequential-
with-a-pause presses the existing-frame check alone was validated
against - reproduces two separate, identically-positioned frames that
Hyprland tabs together, because the second call's `(frame-list)' check
still runs before the first call's frame has appeared in it. Setting
this flag synchronously, before the async subprocess is even started,
closes that window; the 5-second staleness cutoff in
`+emacs-float--pending-p' is just a safety net in case `+emacs-float-init'
never runs (e.g. the nested emacsclient call fails) so a permanently
stuck flag can't wedge the popup shut for the rest of the session.")

(defun +emacs-float--pending-p ()
  "Non-nil if a popup frame was very recently requested and may not have
finished registering with the compositor yet."
  (and +emacs-float--pending
       (< (- (float-time) +emacs-float--pending) 5)))

;;;###autoload
(defun +emacs-float ()
  "Open a floating scratch Emacs popup; C-c C-c sends its text back to
whichever window was focused when this was invoked (C-c C-k cancels).

If one is already open, focuses that instead of spawning a second one.
If one was *just* requested but hasn't finished opening yet, does nothing
rather than racing a second frame into existence - see
`+emacs-float--pending' for why the existing-frame check alone isn't
enough for a fast repeat press. Same `+org-capture-float' pattern gtd.el
already uses for both of these."
  (interactive)
  (cond
   ((+emacs-float--existing-frame)
    (select-frame-set-input-focus (+emacs-float--existing-frame)))
   ((+emacs-float--pending-p)
    (message "emacs-float: still opening..."))
   (t
    (setq +emacs-float--pending (float-time))
    (+emacs-float--open))))

(defun +emacs-float--open ()
  "Actually create the popup frame - see `+emacs-float' for the reuse check
wrapping this."
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
    ;;
    ;; `minibuffer' here matches gtd.el's identically-patterned capture
    ;; popup, fixing the same underlying gap even though this one hasn't
    ;; shown the symptom yet: without its own minibuffer a frame implicitly
    ;; shares the daemon's default one ("F1"), so closing it mid-read
    ;; doesn't end that read the way closing a normal minibuffer-owning
    ;; frame does - confirmed live on the capture popup as an orphaned,
    ;; permanently-blocking read. Must be set at creation time; unlike
    ;; `workspace' there's no `set-frame-parameter' equivalent after the
    ;; fact for this one.
    ;;
    ;; `transient' is what `+emacs-float--existing-frame' (see `+emacs-
    ;; float' above) actually matches on, same as gtd.el's capture popup.
    (call-process "emacsclient" nil 0 nil
                  "--create-frame" "--frame-parameters"
                  "((name . \"emacs-float\") (workspace . \"*emacs-float-popup*\") (minibuffer . t) (transient . t))"
                  "--eval"
                  (format "(+emacs-float-init %S)" origin))))
