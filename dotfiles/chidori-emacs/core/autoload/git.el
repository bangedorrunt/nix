;;; autoload/git.el -*- lexical-binding: t; -*-
;; HACK Magit complains loudly (but harmlessly) when it can't determine its own
;;      version in a sparse clone. Since I'd rather not compromise on shallow
;;      clones, I've gimped `magit-version' so it doesn't complain (unless
;;      called interactively).
;;;###autoload
(defadvice! +magit--ignore-version-a (fn &rest args)
  :around #'magit-version
  (let ((inhibit-message (not (called-interactively-p 'any))))
    (apply fn args)))

;;;###autoload
(defun +magit-display-buffer-fn (buffer)
  "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))

             ;; Everything else should reuse the current window.
             ((or (not (derived-mode-p 'magit-mode))
                  (not (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode))))
              '(display-buffer-same-window))

             ('(+magit--display-buffer-in-direction))))))

(defun +magit--display-buffer-in-direction (buffer alist)
  "`display-buffer-alist' handler that opens BUFFER in a direction.

This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
  (let ((direction (or (alist-get 'direction alist)
                       +magit-open-windows-in-direction))
        (origin-window (selected-window)))
    (if-let (window (window-in-direction direction))
        (unless magit-display-buffer-noselect
          (select-window window))
      (if-let (window (and (not (one-window-p))
                           (window-in-direction
                            (pcase direction
                              (`right 'left)
                              (`left 'right)
                              ((or `up `above) 'down)
                              ((or `down `below) 'up)))))
          (unless magit-display-buffer-noselect
            (select-window window))
        (let ((window (split-window nil nil direction)))
          (when (and (not magit-display-buffer-noselect)
                     (memq direction '(right down below)))
            (select-window window))
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (unless magit-display-buffer-noselect
      (switch-to-buffer buffer t t)
      (selected-window))))


;;
;;; Auto-revert

(defvar +magit--stale-p nil)

(defun +magit--revert-buffer (buffer)
  (with-current-buffer buffer
    (kill-local-variable '+magit--stale-p)
    (when (and buffer-file-name (file-exists-p buffer-file-name))
      (if (buffer-modified-p (current-buffer))
          (when (bound-and-true-p vc-mode)
            (vc-refresh-state)
            (force-mode-line-update))
        (revert-buffer t t t)))))

;;;###autoload
(defun +magit-mark-stale-buffers-h ()
  "Revert all visible buffers and mark buried buffers as stale.

Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (if (get-buffer-window buffer)
          (+magit--revert-buffer buffer)
        (with-current-buffer buffer
          (setq-local +magit--stale-p t))))))

;;;###autoload
(defun +magit-revert-buffer-maybe-h ()
  "Update `vc' and `git-gutter' if out of date."
  (when +magit--stale-p
    (+magit--revert-buffer (current-buffer))))

;;
;;; Commands

;;;###autoload
(defun +magit/quit (&optional kill-buffer)
  "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
  (interactive "P")
  (let ((topdir (magit-toplevel)))
    (funcall magit-bury-buffer-function kill-buffer)
    (or (cl-find-if (lambda (win)
                      (with-selected-window win
                        (and (derived-mode-p 'magit-mode)
                             (equal magit--default-directory topdir))))
                    (window-list))
        (+magit/quit-all))))

;;;###autoload
(defun +magit/quit-all ()
  "Kill all magit buffers for the current repository."
  (interactive)
  (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
  (+magit-mark-stale-buffers-h))

(defun +magit--kill-buffer (buf)
  "TODO"
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))

;; https://github.com/yqrashawn/yqdotfiles/blob/1580bccc122ef5ba78e5efe2e71ba78fc9f33993/.doom.d/autoload.el#L892
;;;###autoload
(defun +magit/toggle-performance ()
  (interactive)
  (require 'magit)
  (require 'magit-autorevert)
  (if magit-refresh-verbose
      (progn (setq!
              magit-refresh-status-buffer t
              magit-refresh-verbose nil
              auto-revert-buffer-list-filter nil
              magit-diff-highlight-indentation nil
              magit-diff-highlight-trailing t
              magit-diff-highlight-keywords t
              magit-diff-highlight-hunk-body t
              magit-diff-paint-whitespace t
              magit-diff-paint-whitespace-lines t
              magit-diff-refine-hunk t
              magit-revision-insert-related-refs 'mixed
              magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv)
              magit-revision-use-hash-sections 'quicker
              magit-diff-expansion-threshold 20)
             (pushnew! vc-handled-backends 'Git)
             (add-hook! 'magit-refs-sections-hook 'magit-insert-tags)
             (add-hook! 'server-switch-hook 'magit-commit-diff)
             (message "exit magit highperf"))
    (progn (setq!
            magit-refresh-status-buffer nil
            magit-refresh-verbose t
            auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p
            magit-diff-highlight-indentation nil
            magit-diff-highlight-trailing nil
            magit-diff-highlight-keywords nil
            magit-diff-highlight-hunk-body nil
            magit-diff-paint-whitespace-lines nil
            magit-diff-paint-whitespace nil
            magit-diff-refine-hunk nil
            magit-revision-insert-related-refs nil
            vc-handled-backends nil
            magit-section-visibility-indicator nil
            magit-revision-use-hash-sections nil
            magit-diff-expansion-threshold 0.01)
           (delq! 'Git vc-handled-backends)
           (remove-hook! 'magit-refs-sections-hook 'magit-insert-tags)
           (remove-hook! 'server-switch-hook 'magit-commit-diff)
           (setq! magit-git-debug nil)
      (message "enter magit highperf"))))
;; git.el ends here
