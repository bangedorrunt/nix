;;; core/chidori-earlyinit.el -*- lexical-binding: t; -*-
;;
;;; Startup optimizations

;; Here are Chidori's hackiest (and least offensive) startup optimizations. They
;; exploit implementation details and unintended side-effects, and will change
;; often between major Emacs releases. However, I disable them if this is a
;; daemon session (where startup time matters less).
;;
;; Most of these have been tested on Linux and on fairly fast machines (with
;; SSDs), so your mileage may vary depending on your hardware.
(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (setq file-name-handler-alist
          ;; HACK: If the bundled elisp for this Emacs install isn't
          ;;   byte-compiled (but is compressed), then leave the gzip file
          ;;   handler there so Emacs won't forget how to read read them.
          ;;
          ;;   calc-loaddefs.el is our heuristic for this because it is built-in
          ;;   to all supported versions of Emacs, and calc.el explicitly loads
          ;;   it uncompiled. This ensures that the only other, possible
          ;;   fallback would be calc-loaddefs.el.gz.
          (if (eval-when-compile
                (locate-file-internal "calc-loaddefs.el" load-path))
              nil
            (list (rassq 'jka-compr-handler old-value))))
    ;; Make sure the new value survives any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)
    ;; COMPAT: ...but restore `file-name-handler-alist' later, because it is
    ;;   needed for handling encrypted or compressed files, among other things.
    (defun chidori--reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have been changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist old-value))))
    (add-hook 'emacs-startup-hook #'chidori--reset-file-handler-alist-h :depth 101))

  (unless noninteractive
    ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
    ;;   larger than the system font) appears to impact startup time
    ;;   dramatically. The larger the delta in font size, the greater the delay.
    ;;   Even trivial deltas can yield a ~1000ms loss, though it varies wildly
    ;;   depending on font size.
    (setq frame-inhibit-implied-resize t)

    ;; PERF,UX: Reduce *Message* noise at startup. An empty scratch buffer (or
    ;;   the dashboard) is more than enough, and faster to display.
    (setq
     inhibit-startup-screen t
     inhibit-startup-echo-area-message user-login-name)
    ;; PERF,UX: Remove "For information about GNU Emacs..." message at startup.
    ;;   It's redundant with our dashboard and incurs a premature redraw.
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
    ;;   with `inhibit-startup-screen', but it would still initialize anyway.
    ;;   This involves some file IO and/or bitmap work (depending on the frame
    ;;   type) that we can no-op for a free 50-100ms boost in startup time.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; PERF: Shave seconds off startup time by starting the scratch buffer in
    ;;   `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
    ;;   pull in a ton of packages.
    (setq
     initial-major-mode 'fundamental-mode
     initial-scratch-message
     (concat
      ";; Chidori Emacs v" emacs-version
      "." (number-to-string emacs-build-number)
      "\n;;\n"
      ";; Go placidly amid the noise and haste,\n"
      ";; and remember what peace there may be in silence.\n"
      ";;\n"
      "\n\n")
     )

    (unless init-file-debug
      ;; PERF,UX: Site files tend to use `load-file', which emits "Loading X..."
      ;;   messages in the echo area. Writing to the echo-area triggers a
      ;;   redisplay, which can be expensive during startup. This may also cause
      ;;   an flash of white when creating the first frame.
      (define-advice load-file (:override (file) silence)
        (load file nil 'nomessage))
      ;; COMPAT: But undo our `load-file' advice later, as to limit the scope of
      ;;   any edge cases it could induce.
      (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
        (advice-remove #'load-file #'load-file@silence))

      ;; PERF: `load-suffixes' and `load-file-rep-suffixes' are consulted on each
      ;;   `require' and `load'. Chidori won't load any modules this early, so omit
      ;;   .so for a small startup boost. This is later restored in chidori-start.
      (put 'load-suffixes 'initial-value (default-toplevel-value 'load-suffixes))
      (put 'load-file-rep-suffixes 'initial-value (default-toplevel-value 'load-file-rep-suffixes))
      (set-default-toplevel-value 'load-suffixes '(".elc" ".el"))
      (set-default-toplevel-value 'load-file-rep-suffixes '(""))
      ;; COMPAT: Undo any problematic startup optimizations; from this point, I make
      ;;   no assumptions about what might be loaded in userland.
      (defun chidori--reset-load-suffixes-h ()
        (setq load-suffixes (get 'load-suffixes 'initial-value)
              load-file-rep-suffixes (get 'load-file-rep-suffixes 'initial-value)))
      ;; TODO find the best spot for hook to fix `emacs-vterm' issue
      (add-hook 'doom-before-init-hook #'chidori--reset-load-suffixes-h)

      ;; PERF: The mode-line procs a couple dozen times during startup. This is
      ;;   normally quite fast, but disabling the default mode-line and reducing the
      ;;   update delay timer seems to stave off ~30-50ms.
      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf (setq mode-line-format nil)))
      ;; PERF,UX: Premature redisplays can substantially affect startup times and
      ;;   produce ugly flashes of unstyled Emacs.
      (setq-default
       inhibit-redisplay t
       inhibit-message t)
      ;; COMPAT: Then reset it with advice, because `startup--load-user-init-file'
      ;;   will never be interrupted by errors. And if these settings are left
      ;;   set, Emacs could appear frozen or garbled.
      (defun chidori--reset-inhibited-vars-h ()
        (setq-default
         inhibit-redisplay nil
         ;; Inhibiting `message' only prevents redraws and
         inhibit-message nil)
        (redraw-frame))
      (add-hook 'after-init-hook #'chidori--reset-inhibited-vars-h)
      (define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
        (when init-file-had-error
          (chidori--reset-inhibited-vars-h))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value))))

      ;; PERF: Chidori: disables the UI elements by default, so that there's less for
      ;;   the frame to initialize. However, the toolbar is still populated
      ;;   regardless, so I lazy load it until tool-bar-mode is actually used.
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file (:before (&rest _) defer-tool-bar-setup)
        (advice-remove #'tool-bar-setup #'ignore)
        (add-transient-hook! 'tool-bar-mode (tool-bar-setup))))))

(provide 'chidori-earlyinit)
