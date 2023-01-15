;;; -*- lexical-binding: t; -*-
;;; core/autoload/embark.el

;;;###autoload
(defun +embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun +embark-find-file-h (target)
  (split-window-below)
  (other-window 1)
  (find-file target))

;; See: https://www.reddit.com/r/emacs/comments/pac8kp/comment/ha3xx9j/
;;;###autoload
(defun +embark/describe-current-completion-candidate ()
  (interactive)
  (describe-symbol
   (intern-soft (plist-get (car (embark--targets)) :target))))

;;;###autoload
(defun +embark/split-vertical-current-completion-candidate ()
  (interactive)
  (embark--act #'find-file-other-window (car (embark--targets)) t))

;;;###autoload
(defun +embark/split-horizontal-current-completion-candidate ()
  (interactive)
  (embark--act #'+embark-find-file-h (car (embark--targets)) t))

;;; embark.el ends here
