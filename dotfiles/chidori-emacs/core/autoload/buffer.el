;;; core/autoload/buffer.el ---  function about buffer.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; buffer functions.
;;

;;; Code:
;;;###autoload
(defun +buffer/switch-to-previous ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;###autoload
(defun +buffer/switch-to-scratch ()
  "Switch to the `*scratch*' buffer.  Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;;;###autoload
(defun +buffer/copy-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;;;###autoload
(defun +buffer/copy-clipboard ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;###autoload
(defun +buffer/safe-revert ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

;;;###autoload
(defun +buffer/safe-erase ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

;; found at http://emacswiki.org/emacs/KillingBuffers
;;;###autoload
(defun +buffer/kill-others ()
  "Kill all other buffers."
  (interactive)
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? " (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (message "Buffers deleted!")))

;;; buffer.el ends here
