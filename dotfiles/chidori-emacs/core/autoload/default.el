;;; core/autoload/default.el -*- lexical-binding: t -*-

;;;###autoload
(defalias '+default/compile #'project-compile)

;;;###autoload
(defun +default/new-buffer ()
  (interactive)
  (if (modulep! 'evil)
      (call-interactively #'evil-buffer-new)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

;;;###autoload
(defun +default/restart-server ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))
