;;; core/autoload/package.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro package! (package recipe &rest body)
  "Get PACKAGE using RECIPE, then evaluate PACKAGE & BODY with `use-package'.

Example:

    (package! foo :auto :commands (foo-bar foo-spam))

If the RECIPE is :auto, use the recipe provided by [M]ELPA.

If the RECIPE is :builtin or :local, do not search [M]ELPA, only pass BODY to `use-package'. While
there is no functional difference between these two keywords, :builtin should be used for packages
within Emacs while :local should be used for user packages which exist locally. :local packages may
require a :load-path for `use-package' to load properly.

If the BODY contains the keyword :disabled, the package is completely ignored, with an expansion
indicating the package has been disabled.

Usage of this macro allows simplified refactoring when changing packaging systems, as Aero is wont
to do every few years."
  (declare (indent defun)) ; indent like use-package
  (cond
   ((memq :disabled body)
    (format "%s :disabled by Doom package!" package))

   ((or (equal recipe :builtin) (equal recipe :local))
    `(use-package ,package ,@body))
   ;; Use straight
   (t
    `(use-package ,package :straight ,(or (equal recipe :auto) recipe) ,@body))))

;;;###autoload
(defun treesitterp ()
  "Evaluate whether Emacs has treesitter support."
  (and (functionp 'treesit-available-p) (treesit-available-p)))

;;; package.el ends here
