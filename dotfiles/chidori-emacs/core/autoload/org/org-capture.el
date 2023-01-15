;;; core/autoload/org/org-capture.el ---  Custom org configuration.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Custom org configuration.
;;

;;; Code:

(defvar org-capture-initial)

;;;###autoload
(defun +org-init-capture-h ()
  ;; Kill capture buffers by default (unless they've been visited)
  (after! org-capture
    (org-capture-put :kill-buffer t))

  ;; Fix #462: when refiling from org-capture, Emacs prompts to kill the
  ;; underlying, modified buffer. This fixes that.
  (add-hook 'org-after-refile-insert-hook #'save-buffer)

  ;; HACK Doom doesn't support `customize'. Best not to advertise it as an
  ;;      option in `org-capture's menu.
  (defadvice! +org--remove-customize-option-a (fn table title &optional prompt specials)
    :around #'org-mks
    (funcall fn table title prompt
             (remove '("C" "Customize org-capture-templates")
                     specials)))

  (defadvice! +org--capture-expand-variable-file-a (file)
    "If a variable is used for a file path in `org-capture-template', it is used
as is, and expanded relative to `default-directory'. This changes it to be
relative to `org-directory', unless it is an absolute path."
    :filter-args #'org-capture-expand-file
    (if (and (symbolp file) (boundp file))
        (expand-file-name (symbol-value file) org-directory)
      file))

  (add-hook! 'org-capture-mode-hook
    (defun +org-show-target-in-capture-header-h ()
      (setq header-line-format
            (format "%s%s%s"
                    (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                'face 'font-lock-string-face)
                    org-eldoc-breadcrumb-separator
                    header-line-format)))))

;;;###autoload
(defun +org-init-capture-frame-h ()
  (add-hook 'org-capture-after-finalize-hook #'+org-capture-cleanup-frame-h)

  (defadvice! +org-capture-refile-cleanup-frame-a (&rest _)
    :after #'org-capture-refile
    (+org-capture-cleanup-frame-h)))
;;
;;; External frame

(defvar +org-capture-fn #'org-capture
  "Command to use to initiate org-capture.")

;;;###autoload
(defvar +org-capture-frame-parameters
  `((name . "doom-capture")
    (width . 70)
    (height . 25)
    (transient . t)
    ,@(when IS-LINUX
        `((window-system . ,(if (boundp 'pgtk-initialized) 'pgtk 'x))
          (display . ,(or (getenv "WAYLAND_DISPLAY")
                          (getenv "DISPLAY")
                          ":0"))))
    ,(if IS-MAC '(menu-bar-lines . 1)))
  "TODO")

;;;###autoload
(defun +org-capture-cleanup-frame-h ()
  "Closes the org-capture frame once done adding an entry."
  (when (and (+org-capture-frame-p)
             (not org-capture-is-refiling))
    (delete-frame nil t)))

;;;###autoload
(defun +org-capture-frame-p (&rest _)
  "Return t if the current frame is an org-capture frame opened by
`+org-capture/open-frame'."
  (and (equal (alist-get 'name +org-capture-frame-parameters)
              (frame-parameter nil 'name))
       (frame-parameter nil 'transient)))

;;;###autoload
(defun +org-capture/open-frame (&optional initial-input key)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (when (and initial-input (string-empty-p initial-input))
    (setq initial-input nil))
  (when (and key (string-empty-p key))
    (setq key nil))
  (let* ((frame-title-format "")
         (frame (if (+org-capture-frame-p)
                    (selected-frame)
                  (make-frame +org-capture-frame-parameters))))
    (select-frame-set-input-focus frame)  ; fix MacOS not focusing new frames
    (with-selected-frame frame
      (require 'org-capture)
      (condition-case ex
          (letf! ((#'pop-to-buffer #'switch-to-buffer))
            (switch-to-buffer (doom-fallback-buffer))
            (let ((org-capture-initial initial-input)
                  org-capture-entry)
              (when (and key (not (string-empty-p key)))
                (setq org-capture-entry (org-capture-select-template key)))
              (funcall +org-capture-fn)))
        ('error
         (message "org-capture: %s" (error-message-string ex))
         (delete-frame frame))))))

;;;###autoload
(defun +org-capture-available-keys ()
  "TODO"
  (string-join (mapcar #'car org-capture-templates) ""))


;;
;;; Capture targets

;;;###autoload
(defun +org-capture-todo-file ()
  "Expand `+org-capture-todo-file' from `org-directory'.
If it is an absolute path return `+org-capture-todo-file' verbatim."
  (expand-file-name +org-capture-todo-file org-directory))

;;;###autoload
(defun +org-capture-notes-file ()
  "Expand `+org-capture-notes-file' from `org-directory'.
If it is an absolute path return `+org-capture-todo-file' verbatim."
  (expand-file-name +org-capture-notes-file org-directory))

;;; org-capture.el ends here
