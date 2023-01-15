;;; lisp/chidori-monospace.el --  -*- lexical-binding: t -*-

;; https://git.sr.ht/~woolsweater/.emacs.d/tree/main/item/modules/monospace-mode.el
;;
(require 'face-remap)

(defvar monos-default-spec nil
  "Face spec to use for the `default' face.

This should be set *before* loading `monospace-mode'.")

(defvar monos-fixed-pitch-spec nil
  "Face spec to use for the `fixed-pitch' face.

This should be set *before* loading `monospace-mode'.")

(defvar monos-minibuffer-prompt-patterns '("\\<command\\>:")
  "List of regexes for which `monos-activate-for-minibuffer-prompt' will turn
on `monospace-mode' in the minibuffer.")

(defun monos-activate-for-minibuffer-prompt ()
  "A `minibuffer-setup-hook' to turn on `monospace-mode' when the
prompt matches one of the entries in
`monos-minibuffer-prompt-patterns'."
  (when-let (prompt (minibuffer-prompt))
    (let ((case-fold-search nil))
      (when (seq-contains-p monos-minibuffer-prompt-patterns
                            prompt
                            #'string-match-p)
        (monospace-mode)))))

;;;###autoload
(defun monos-use-fixed-pitch (&rest faces)
  "Apply the fixed-pitch family and height to every face in FACES.

Use this on fonts that inherit from `default' to switch them to
be monospace."
  (let ((family (face-attribute 'fixed-pitch :family nil))
        (height (face-attribute 'fixed-pitch :height nil)))
    (dolist (face faces)
      (set-face-attribute face nil
                          :family family
                          :height height))))

(apply #'set-face-attribute 'default nil monos-default-spec)
(apply #'set-face-attribute 'fixed-pitch nil monos-fixed-pitch-spec)

(defun monos--update-frame-width (face)
  "Update frame width to reflect the active face's size."
  (let ((mwidth (window-font-width nil face))
        (columns (frame-width)))
    (set-frame-width nil (* mwidth columns) nil t)))

;;;###autoload
(define-minor-mode monospace-mode
  "Make the current buffer use a monospace font."
  :lighter ""
  (if monospace-mode
      (buffer-face-set 'fixed-pitch)
    (buffer-face-set)))

(provide 'chidori-monospace)
