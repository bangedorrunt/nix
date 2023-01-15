;;; core/autoload/doom/plist.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Library

;;;###autoload
(defun doom-plist-get (plist prop &optional nil-value)
  "Return PROP in PLIST, if it exists. Otherwise NIL-VALUE."
  (if-let (val (plist-member plist prop))
      (cadr val)
    nil-value))

;;;###autoload
(defun doom-plist-merge (from-plist to-plist)
  "Non-destructively merge FROM-PLIST onto TO-PLIST"
  (let ((plist (copy-sequence from-plist)))
    (while plist
      (cl-callf plist-put to-plist (pop plist) (pop plist)))
    to-plist))

;;;###autoload
(defun doom-plist-delete-nil (plist)
  "Delete `nil' properties from a copy of PLIST."
  (let (p)
    (while plist
      (if (car plist)
          (cl-callf plist-put p (car plist) (nth 1 plist)))
      (setq plist (cddr plist)))
    p))

;;;###autoload
(defun doom-plist-keys (plist)
  "Return the keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

;;;###autoload
(defun doom-plist-values (plist)
  "Return the values in PLIST."
  (let (keys)
    (while plist
      (push (cadr plist) keys)
      (setq plist (cddr plist)))
    keys))

(provide 'doom '(plist))
;;; plist.el ends here
