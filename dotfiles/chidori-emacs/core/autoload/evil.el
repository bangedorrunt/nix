;;; autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil-escape-a (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

(defun +evil--insert-newline (&optional above _noextranewline)
  (let ((pos (save-excursion (beginning-of-line-text) (point)))
        comment-auto-fill-only-comments)
    (require 'smartparens)
    (evil-narrow-to-field
      (if above
          (if (save-excursion (nth 4 (sp--syntax-ppss pos)))
              (evil-save-goal-column
                (setq evil-auto-indent nil)
                (goto-char pos)
                (let ((ws (abs (skip-chars-backward " \t"))))
                  ;; FIXME oh god why
                  (save-excursion
                    (if comment-line-break-function
                        (funcall comment-line-break-function nil)
                      (comment-indent-new-line))
                    (when (and (derived-mode-p 'c-mode 'c++-mode 'objc-mode 'java-mode 'js2-mode)
                               (eq (char-after) ?/))
                      (insert "*"))
                    (insert
                     (make-string (max 0 (+ ws (skip-chars-backward " \t")))
                                  32)))
                  (insert (make-string (max 1 ws) 32))))
            (evil-move-beginning-of-line)
            (insert (if use-hard-newlines hard-newline "\n"))
            (forward-line -1)
            (back-to-indentation))
        (evil-move-end-of-line)
        (cond ((sp-point-in-comment pos)
               (setq evil-auto-indent nil)
               (if comment-line-break-function
                   (funcall comment-line-break-function nil)
                 (comment-indent-new-line)))
              ;; TODO Find a better way to do this
              ((and (eq major-mode 'haskell-mode)
                    (fboundp 'haskell-indentation-newline-and-indent))
               (setq evil-auto-indent nil)
               (haskell-indentation-newline-and-indent))
              (t
               (insert (if use-hard-newlines hard-newline "\n"))
               (back-to-indentation)))))))

;;;###autoload
(defun +evil--insert-newline-below-and-respect-comments-a (fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-below))
          (evil-insert-state-p)
          (evil-emacs-state-p))
      (funcall fn count)
    (letf! (defun evil-insert-newline-below () (+evil--insert-newline))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall fn count)))))

;;;###autoload
(defun +evil--insert-newline-above-and-respect-comments-a (fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-above))
          (evil-insert-state-p)
          (evil-emacs-state-p))
      (funcall fn count)
    (letf! (defun evil-insert-newline-above () (+evil--insert-newline 'above))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall fn count)))))

;;;###autoload (autoload '+evil-window-split-a (concat chidori-core-dir "autoload/evil") nil t)
(evil-define-command +evil-window-split-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK This ping-ponging between the destination and source windows is to
  ;;      update the window focus history, so that, if you close either split
  ;;      afterwards you won't be sent to some random window.
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'below))
    (unless evil-split-window-below
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload (autoload '+evil-window-vsplit-a (concat chidori-core-dir "autoload/evil") nil t)
(evil-define-command +evil-window-vsplit-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK This ping-ponging between the destination and source windows is to
  ;;      update the window focus history, so that, if you close either split
  ;;      afterwards you won't be sent to some random window.
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'right))
    (unless evil-vsplit-window-right
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload (autoload '+evil-join-a (concat chidori-core-dir "autoload/evil") nil nil)
(defun +evil-join-a (fn beg end)
  "Join the selected lines.

This advice improves on `evil-join' by removing comment delimiters when joining
commented lines, without `fill-region-as-paragraph'.

Adapted from https://github.com/emacs-evil/evil/issues/606"
  (if-let* (((not (= (line-end-position) (point-max))))
            (cend (save-excursion (goto-char end) (line-end-position)))
            (cbeg (save-excursion
                    (goto-char beg)
                    (and (doom-point-in-comment-p
                          (save-excursion
                            (goto-char (line-beginning-position 2))
                            (skip-syntax-forward " \t")
                            (point)))
                         (or (comment-search-backward (line-beginning-position) t)
                             (comment-search-forward  (line-end-position) t)
                             (and (doom-point-in-comment-p beg)
                                  (stringp comment-continue)
                                  (or (search-forward comment-continue (line-end-position) t)
                                      beg)))))))
      (let* ((count (count-lines beg end))
             (count (if (> count 1) (1- count) count))
             (fixup-mark (make-marker)))
        (uncomment-region (line-beginning-position 2)
                          (save-excursion
                            (goto-char cend)
                            (line-end-position 0)))
        (unwind-protect
            (dotimes (_ count)
              (join-line 1)
              (save-match-data
                (when (or (and comment-continue
                               (not (string-empty-p comment-continue))
                               (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                          (and comment-start-skip
                               (not (string-empty-p comment-start-skip))
                               (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                  (replace-match "" t nil nil 1)
                  (just-one-space))))
          (set-marker fixup-mark nil)))
    ;; But revert to the default we're not in a comment, where
    ;; `fill-region-as-paragraph' is too greedy.
    (funcall fn beg end)))

;; Define vig and vag, etc. to look for all paren types
;;;###autoload
(defun +evil-paren-range (count beg end type inclusive)
  "Get minimum range of paren text object.
COUNT, BEG, END, TYPE is used.  If INCLUSIVE is t, the text object is inclusive."
  (let* ((parens '("()" "[]" "{}" "<>"))
         range
         found-range)
    (dolist (p parens)
      (condition-case nil
          (setq range (evil-select-paren (aref p 0) (aref p 1) beg end type count inclusive))
        (error nil))
      (when range
        (cond
         (found-range
          (when (< (- (nth 1 range) (nth 0 range)) (- (nth 1 found-range) (nth 0 found-range)))
            (setf (nth 0 found-range) (nth 0 range))
            (setf (nth 1 found-range) (nth 1 range))))
         (t
          (setq found-range range)))))
    found-range))

;; Very useful, in visual mode, use < and > to indent/unindent the line(s)
;;;###autoload
(defun +evil/shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/insert-newline-below-and-follow ()
  "Insert blank line(s) below current line and follow. Does not change modes."
  (interactive)
    (+evil--insert-newline)
    (evil-normal-state))

;;;###autoload
(defun +evil/insert-newline-above-and-follow ()
  "Insert blank line(s) above current line and follow. Does not change modes."
  (interactive)
    (+evil--insert-newline 'above)
    (evil-normal-state))

;; ;;;###autoload
(defun +evil/insert-newline-below (count)
  "Insert COUNT blank line(s) below current line. Does not change modes."
  (interactive "p")
  (dotimes (_ count)
    (save-excursion (evil-insert-newline-below))))

;;;###autoload
(defun +evil/insert-newline-above (count)
  "Insert COUNT blank line(s) above current line. Does not change modes."
  (interactive "p")
  (dotimes (_ count)
    (save-excursion (evil-insert-newline-above))))

;;;###autoload (autoload '+evil-delete (concat chidori-core-dir "autoload/evil") nil t)
(evil-define-operator +evil-delete (beg end type register yank-handler)
  "A wrapper around `evil-delete' for `wgrep' buffers that will invoke
`wgrep-mark-deletion' on lines you try to delete."
  (interactive "<R><x><y>")
  (condition-case _ex
      (evil-delete beg end type register yank-handler)
    ('text-read-only
     (evil-apply-on-block
      (lambda (beg _)
        (goto-char beg)
        (call-interactively #'wgrep-mark-deletion))
      beg (1- end) nil))))

;; evil.el ends here
