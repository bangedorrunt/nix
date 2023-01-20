;;; core/autoload/corfu.el -*- lexical-binding: t; -*-

;;;; REVIEW integrate with `tempel'
(defvar +corfu-super-capfs
  '(tempel-complete
    :completion
    cape-file
    cape-dabbrev
    cape-dict)
  "A list of global capes to be available at all times.
The key :completion is used to specify where completion candidates should be
placed, otherwise they come first.")

(defvar +corfu-cape-backends
  '(eglot-completion-at-point
    elisp-completion-at-point
    tags-completion-at-point-function)
  "A prioritised list of backend capfs to create a super cape onto from
  `+corfu-super-capfs'.")

;;;###autoload
(defun +corfu--load-capes ()
  "Load all capes specified in `+corfu-global-capes'."
  (interactive)
  (if-let ((capfs (cl-intersection +corfu-cape-backends completion-at-point-functions)))
      (setq-local
       completion-at-point-functions
       (cl-substitute
        (apply #'cape-super-capf (cl-substitute (car capfs) :completion (cl-pushnew :completion +corfu-super-capfs)))
        (car capfs)
        completion-at-point-functions))
    (setq-local
     completion-at-point-functions
     (apply #'cape-super-capf
            (cl-substitute (car completion-at-point-functions) :completion (cl-pushnew :completion +corfu-super-capfs))))))

;;;###autoload
(defun +corfu--setup-hook-h ()
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;;;###autoload
(defun +corfu/move-to-minibuffer ()
  "Move current completions to the minibuffer"
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

;;;###autoload
(defun +corfu-enable-in-minibuffer ()
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (memq this-command '(evil-ex evil-ex-search-forward evil-ex-search-backward))
              (corfu-mode +1))))

;; corfu.el ends here
