;;; core/lisp/popup.el -*- lexical-binding: t; -*-
;; TODO drop this in favour of `popper.el' or builtin `display-buffer-alist'

(noct-handle-popup (rx "*Warnings*"))
(noct-handle-popup "^\\*info\\*$")
(noct-handle-popup "^\\*Completions")
(noct-handle-popup "^\\*Local variables\\*$")
(noct-handle-popup "^\\*Customize")
(noct-handle-popup "^\\*Process List\\*")
(noct-handle-popup "^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*")

(cl-pushnew
 (list (rx "*Async Shell Command*" (0+ any)) #'display-buffer-no-window)
 display-buffer-alist)

;; TODO not working
(noct-handle-popup "^\\*Pp Macroexpand Output\\*")

(after! compile
  (noct-handle-popup compilation-mode))

(after! help
  (noct-handle-popup help-mode))

(after! apropos
  (noct-handle-popup apropos-mode))

(after! helpful
  (noct-handle-popup helpful-mode))

(after! docker
  (noct-handle-popup "*docker")
  (noct-handle-popup "* docker"))

(after! xref
  ;; window for selecting between definition and variable
  (noct-handle-popup "*xref*"))

(after! vterm
  (noct-handle-popup-same-window (rx "*vterm")))

(after! evil
  ;; Make evil-mode cooperate with popups
  (defadvice! +popup--evil-command-window-a (hist cmd-key execute-fn)
    "Monkey patch the evil command window to use `pop-to-buffer' instead of
`switch-to-buffer', allowing the popup manager to handle it."
    :override #'evil-command-window
    (when (eq major-mode 'evil-command-window-mode)
      (user-error "Cannot recursively open command line window"))
    (dolist (win (window-list))
      (when (equal (buffer-name (window-buffer win))
                   "*Command Line*")
        (kill-buffer (window-buffer win))
        (delete-window win)))
    (setq evil-command-window-current-buffer (current-buffer))
    (ignore-errors (kill-buffer "*Command Line*"))
    (with-current-buffer (pop-to-buffer "*Command Line*")
      (setq-local evil-command-window-execute-fn execute-fn)
      (setq-local evil-command-window-cmd-key cmd-key)
      (evil-command-window-mode)
      (evil-command-window-insert-commands hist)))

  (defadvice! +popup--evil-command-window-execute-a ()
    "Execute the command under the cursor in the appropriate buffer, rather than
the command buffer."
    :override #'evil-command-window-execute
    (interactive)
    (let ((result (buffer-substring (line-beginning-position)
                                    (line-end-position)))
          (execute-fn evil-command-window-execute-fn)
          (execute-window (get-buffer-window evil-command-window-current-buffer))
          (popup (selected-window)))
      (if execute-window
          (select-window execute-window)
        (user-error "Originating buffer is no longer active"))
      ;; (kill-buffer "*Command Line*")
      (delete-window popup)
      (funcall execute-fn result)
      (setq evil-command-window-current-buffer nil)))
  )

(provide 'chidori-popup)
