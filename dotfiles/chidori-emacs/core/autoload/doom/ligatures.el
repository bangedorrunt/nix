;;; core/autoload/doom/ligatures.el -*- lexical-binding: t; -*-
;;;
;;;###autoload
(defun set-ligatures! (modes &rest plist)
  "Associates string patterns with ligatures or icons in certain major-modes.
  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must either:
  - match keys in
`+ligatures-extra-symbols', and whose values are strings representing the text
to be replaced with that symbol, or
 - be the special property :font-ligatures
  :font-ligatures LIST
    Sets the list of strings that should get transformed by the font into ligatures,
    like \"==\" or \"-->\". LIST is a list of strings.
If the car of PLIST is nil, then unset any
pretty symbols and ligatures previously defined for MODES.
For example, the rule for emacs-lisp-mode is very simple:
  (set-ligatures! 'emacs-lisp-mode
    :lambda \"lambda\"
    :font-ligatures (list \"->\"))
This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
associated with :lambda in `+ligatures-extra-symbols', and will also ligate
\"->\" into the arrow of choice according to your font.
Pretty symbols can be unset for emacs-lisp-mode with:
  (set-ligatures! 'emacs-lisp-mode nil)
Note that this will keep all ligatures in `+ligatures-prog-mode-list' active, as
`emacs-lisp-mode' is derived from `prog-mode'."
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (ensure-list modes))
        (delq! mode +ligatures-extra-alist 'assq)
        (add-to-list 'ligature-ignored-major-modes mode))
    (let ((results)
          (font-ligatures))
      (while plist
        (let ((key (pop plist)))
          (cond
           ((eq key :font-ligatures)
            (setq font-ligatures (pop plist)))
           (t
            (when-let (char (plist-get +ligatures-extra-symbols key))
              (push (cons (pop plist) char) results))))))
      (when font-ligatures
        (with-eval-after-load 'ligature
          (dolist (mode (ensure-list modes))
            (setq ligature-ignored-major-modes (delete mode ligature-ignored-major-modes)))
          (ligature-set-ligatures (ensure-list modes) font-ligatures)))
      (dolist (mode (ensure-list modes))
        (setf (alist-get mode +ligatures-extra-alist)
              (if-let (old-results (alist-get mode +ligatures-extra-alist))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))

;;; ligatures.el ends here
