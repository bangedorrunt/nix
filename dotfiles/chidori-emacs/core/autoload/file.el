;;; core/autoload/file.el ---  function about file.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; file functions.
;;

;;; Code:
(declare-function dired-get-file-for-visit 'dired)
(declare-function recentf-remove-if-non-kept 'recentf)
(declare-function w32-shell-execute 'w32-shell-execute)

;;;###autoload
(defun +file/reload-init ()
  "Reload init file."
  (interactive)
  (run-hooks 'doom-before-reload-hook)
  (load-file user-init-file))

;;;###autoload
(defun +file/open-init ()
  "Open init file."
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun +file/open-keybind ()
  "Open keybind file."
  (interactive)
  (find-file (expand-file-name "lisp/chidori-binding.el" user-emacs-directory)))

;;;###autoload
(defun +file/open-gtd ()
  "Open gtd file."
  (interactive)
  (find-file "~/workspace/notetoself/gtd.org"))

;;;###autoload
(defun +file/open-test ()
  "Open test file."
  (interactive)
  (find-file (read-file-name "select test file: " "~/test/")))

;;;###autoload
(defun +file/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun +file/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;;###autoload
(defun +file/open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path
         (if (eq major-mode 'dired-mode)
             (dired-get-file-for-visit)
           (buffer-file-name))))
    (if file-path
        (cond
         ((eq system-type 'windows-nt)
          (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((eq system-type 'darwin)
          (shell-command (format "open \"%s\"" file-path)))
         ((eq system-type 'gnu/linux)
          (let ((process-connection-type nil))
            (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

;;;###autoload
(defun +file/sudo-edit (&optional file)
  "Edit FILE with sudo user."
  (interactive "P")
  (let* ((file
          (or file
              (read-file-name "File: " nil nil nil (file-name-nondirectory (buffer-file-name)))))
         (host (or (file-remote-p file 'host) "localhost")))
    (find-file
     (concat
      "/"
      (when (file-remote-p file)
        (concat
         (file-remote-p file 'method) ":"
         (let ((user (file-remote-p file 'user)))
           (if user
               (concat user "@" host)
             host))
         "|"))
      "sudo:root@" host ":" (or (file-remote-p file 'localname) file)))))

;;;###autoload
(defun +file/delete ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;;###autoload
(defun +file/rename ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((get-buffer new-name)
          (error "A buffer named '%s' already exists!" new-name))
         (t
          (let ((dir (file-name-directory new-name)))
            (when (and (not (file-exists-p dir))
                       (yes-or-no-p (format "Create directory '%s'?" dir)))
              (make-directory dir t)))
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (when (fboundp 'recentf-add-file)
            (recentf-add-file new-name)
            (recentf-remove-if-non-kept filename))
          (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;;;###autoload
(defun +file/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;;###autoload
(defun +file/copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (file-name-nondirectory buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;;###autoload
(defun +file/line-number (&optional pos absolute)
  "Return buffer line number at position POS ABSOLUTE."
  (save-restriction
    (when absolute
      (widen))
    (let ((opoint (or pos (point))))
      (save-excursion
        (goto-char opoint)
        (string-to-number (format-mode-line "%l"))))))

;;; file.el ends here
