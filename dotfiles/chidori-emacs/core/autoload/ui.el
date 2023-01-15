;;; core/autoload/ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +modus-themes-custom-faces ()
  (setq
   hl-todo-keyword-faces
   '(("TODO" . "#ff0000")
     ("WARNING" . "#ff0000")
     ("HACK" . "#ffff00")
     ("FIXME" . "#ffff00")
     ("DEPRECATED" . "#ffff00")
     ("REVIEW" . "#ffff00")
     ("XXX"  . "#00ffff")
     ("NOTE" . "#ff00ff")))
  (modus-themes-with-colors
    (custom-set-faces
     `(diff-hl-margin-insert ((,c :foreground ,green-intense)))
     `(diff-hl-margin-change ((,c :foreground ,red-faint)))
     `(diff-hl-margin-delete ((,c :foreground ,red-intense)))
     `(diff-hl-margin-unknown ((,c :foreground ,yellow-intense)))
     `(diff-hl-margin-ignore ((,c :foreground ,magenta-warmer)))
     `(diff-hl-insert ((,c :foreground ,green-intense)))
     `(diff-hl-change ((,c :foreground ,red-faint)))
     `(diff-hl-delete ((,c :foreground ,red-intense)))
     `(diff-hl-unknown ((,c :foreground ,yellow-intense)))
     `(diff-hl-ignore ((,c :foreground ,magenta-warmer)))
     `(git-gutter:modified ((,c :background ,red-faint)))
     `(git-gutter-fr:modified ((,c :background ,red-faint)))))
  )

;;;###autoload
(defun +ui-set-up-display-for-terminal (&optional frame)
  (or frame (setq frame (selected-frame)))
  (unless (display-graphic-p frame)
    (progn
      (set-face-background 'default "unspecified-bg" frame)
      (menu-bar-mode 0))))

;;;###autoload
(defun +ui-disable-not-useful-fringes (frame)
  "Turn off fringes where they're not helpful, like in a minibuffer
and Help/Info frames"
  (set-window-fringes (minibuffer-window frame) 0 0 nil t)
  (when (cl-some #'derived-mode-p '(help-mode info-mode))
    (set-window-fringes (frame-root-window frame) 0 0 nil)))

;;;###autoload
(defun +ui-configure-fringes-and-margins (frame)
  "Hook to immediately apply window/frame customizations."
  (when (display-graphic-p frame)
    (let ((window (frame-root-window frame)))
      (set-window-buffer window (window-buffer window)))))
