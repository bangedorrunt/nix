;;;; lisp/chidori-git.el --- Starting configuration for git -*- lexical-binding: t; -*-
;;

;; Put this before `magit' otherwise it will install new `transient' package
(package! transient :builtin
  :custom
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (transient-levels-file  (concat chidori-etc-dir "transient/levels"))
  (transient-values-file  (concat chidori-etc-dir "transient/values"))
  (transient-history-file (concat chidori-etc-dir "transient/history"))
  :config
  (setq
   transient-highlight-mismatched-keys t
   ;; e.g. will now show since and until on log transient
   transient-default-level 7
   ;; TODO issue with it failing because \.\.\. in history
   transient-save-history t
   transient-mode-line-format nil))

(package! magit :auto
  :defer-incrementally dash f s with-editor git-commit package eieio transient
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  :config
  (noct-handle-popup-same-window magit-status-mode)
  (noct-handle-popup-same-window magit-log-mode)
  (noct-handle-popup-same-window magit-cherry-mode)
  (noct-handle-popup-same-window magit-log-select-mode)
  (noct-handle-popup-other-window magit-revision-mode)
  (noct-handle-popup-other-window-no-select magit-diff-mode)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (setq magit-buffer-name-format "%x%M%v: %t%x"
        magit-diff-paint-whitespace-lines 'both
        magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t
        git-commit-style-convention-checks '(non-empty-second-line
                                             overlong-summary-line)
        git-commit-summary-max-length 50
        git-commit-fill-column 72)
  (defadvice magit-diff (after switch-to-diff activate)
    (other-window 1))
  (add-hook! 'magit-status-mode-hook (toggle-truncate-lines -1))
  (defadvice! +magit-revert-repo-buffers-deferred-a (&rest _)
    :after '(magit-checkout magit-branch-and-checkout)
    ;; Use a more efficient strategy to auto-revert buffers whose git state has
    ;; changed: refresh the visible buffers immediately...
    (+magit-mark-stale-buffers-h))
  ;; ...then refresh the rest only when we switch to them, not all at once.
  (add-hook 'doom-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers. It's disorienting, especially on
  ;; larger projects.
  (defvar +magit--pos nil)
  (add-hook! 'magit-pre-refresh-hook
    (defun +magit--set-window-state-h ()
      (setq-local +magit--pos (list (current-buffer) (point) (window-start)))))
  (add-hook! 'magit-post-refresh-hook
    (defun +magit--restore-window-state-h ()
      (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
        (goto-char (cadr +magit--pos))
        (set-window-start nil (caddr +magit--pos) t)
        (kill-local-variable '+magit--pos))))

  ;; Optimization from officical magit
  ;; (setq magit-refresh-status-buffer nil)
  ;; Committing performance
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  ;; Refs buffer performance
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

  ;; An optimization that particularly affects macOS and Windows users: by
  ;; resolving `magit-git-executable' Emacs does less work to find the
  ;; executable in your PATH, which is great because it is called so frequently.
  ;; However, absolute paths will break magit in TRAMP/remote projects if the
  ;; git executable isn't in the exact same location.
  (add-hook! 'magit-status-mode-hook
    (defun +magit-optimize-process-calls-h ()
      (when-let (path (executable-find magit-git-executable t))
        (setq-local magit-git-executable path))))

  (add-hook! 'magit-diff-visit-file-hook
    (defun +magit-reveal-point-if-invisible-h ()
      "Reveal the point if in an invisible region."
      (if (derived-mode-p 'org-mode)
          (org-reveal '(4))
        (require 'reveal)
        (reveal-post-command))))

  ;; Magit uses `magit-display-buffer-traditional' to display windows, by
  ;; default, which is a little primitive. `+magit-display-buffer' marries
  ;; `magit-display-buffer-fullcolumn-most-v1' with
  ;; `magit-display-buffer-same-window-except-diff-v1', except:
  ;;
  ;; 1. Magit sub-buffers (like `magit-log') that aren't spawned from a status
  ;;    screen are opened as popups.
  ;; 2. The status screen isn't buried when viewing diffs or logs from the
  ;;    status screen.
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer-fn
        magit-bury-buffer-function #'magit-mode-quit-window)

  ;; Position windows

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (map!
   :leader
   "g"   '(:ignore t :wk "git")
   "gL"  #'git-link
   "gs"  #'magit-status
   "gb"  #'magit-blame
   "gl"  '(:ignore t :wk "log")
   "glb" #'magit-log-buffer-file
   "gld" #'magit-log-trace-definition
   "gll" #'magit-log-head
   "gf"  '(:ignore t :wk "files")
   "gfs" #'magit-stage-file
   "gfu" #'magit-unstage-file
   "gm"  '(:ignore t :wk "smerge")
   "gmm" #'smerge-start-session
   "gmu" #'smerge-keep-upper
   "gml" #'smerge-keep-lower
   "gmn" #'smerge-next
   "gmp" #'smerge-prev
   "gma" #'smerge-keep-all
   "gmE" #'smerge-ediff
   "gmC" #'smerge-combine-with-next
   "gmr" #'smerge-refine
   "gmR" #'smerge-resolve
   "gd"  '(:ignore t :wk "diff")
   "gdu" #'smerge-diff-upper-lower
   "gdB" #'smerge-diff-base-upper
   "gdb" #'smerge-diff-base-lower)

  (map!
   (:map magit-mode-map
    :n "q"      #'+magit/quit
    :n "Q"      #'+magit/quit-all)
   (:map magit-blame-mode-map
    :n "q"      #'magit-blame-quit)
   (:map magit-log-mode-map
    :n "q"      #'magit-log-bury-buffer))

  ;; Close transient with ESC
  (map!
   :map transient-map
   "ESC"    #'transient-quit-one
   "q"      #'transient-quit-one
   )
  )

(package! forge :auto
  :after magit
  :config
  (setq forge-database-file (expand-file-name "forge-database.sqlite" chidori-cache-dir))
  (noct-handle-popup-same-window forge-topic-mode)
  (noct-handle-popup-same-window forge-post-mode)
  )

(package! magit-todos :auto
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 10
        magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME" "TEMP"))))

;; NOTE this is culprit that makes `magit-status' slow to a crawl
;;
;; Use delta pager in magit diffs
(package! magit-delta :auto
  :disabled t
  :hook (magit-mode . magit-delta-mode))

;; extremely difficult to style for some reason
(package! blamer :auto
  :commands (blamer-mode)
  :custom
  (blamer-view 'overlay-right)
  (blamer-type 'visual)
  (blamer-max-commit-message-length 180)
  (blamer-author-formatter " ● [%s] ")
  (blamer-commit-formatter "| %s")
  (blamer-idle-time 0.1)
  (blamer-min-offset 15))

(package! smerge :builtin
  :config
  (defun smerge-repeatedly ()
    "Perform smerge actions again and again"
    (interactive)
    (smerge-mode 1)
    (smerge-transient))
  (after! transient
    (transient-define-prefix smerge-transient ()
      [["Move"
        ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (smerge-repeatedly)))
        ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (smerge-repeatedly)))]
       ["Keep"
        ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (smerge-repeatedly)))
        ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (smerge-repeatedly)))
        ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (smerge-repeatedly)))
        ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (smerge-repeatedly)))
        ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (smerge-repeatedly)))]
       ["Diff"
        ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (smerge-repeatedly)))
        ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (smerge-repeatedly)))
        (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (smerge-repeatedly)))
        ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (smerge-repeatedly)))
        ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff)) (smerge-repeatedly)))]
       ["Other"
        ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (smerge-repeatedly)))
        ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (smerge-repeatedly)))
        ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (smerge-repeatedly)))
        ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]])))

(package! diff-hl :auto
  :hook
  (find-file . diff-hl-mode)
  (vc-dir-mode . diff-hl-dir-mode)
  (dired-mode . diff-hl-dired-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (noct-handle-popup "^\\*diff-hl")
  ;; PERF: reduce load on remote
  (defvaralias 'diff-hl-disable-on-remote '+vc-gutter-in-remote-files)
  ;; PERF: A slightly faster algorithm for diffing.
  (setq vc-git-diff-switches '("--histogram"))
  ;; PERF: Slightly more conservative delay before updating the diff
  (setq diff-hl-flydiff-delay 0.5)  ; default: 0.3
  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (setq diff-hl-show-staged-changes nil)
  ;; UX: Refresh git-gutter on ESC or refocusing the Emacs frame.
  (add-hook! '(doom-escape-hook doom-switch-window-hook) :append
    (defun +vc-gutter:update-h (&rest _)
      "Return nil to prevent shadowing other `doom-escape-hook' hooks."
      (ignore (or inhibit-redisplay
                  (and (or (bound-and-true-p diff-hl-mode)
                           (bound-and-true-p diff-hl-dir-mode))
                       (diff-hl-update-once))))))
  ;; UX: Update diff-hl when magit alters git state.
  (after! magit
    (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; FIX: The revert popup consumes 50% of the frame, whether or not you're
  ;;   reverting 2 lines or 20. This resizes the popup to match its contents.
  (defadvice! +vc-gutter--shrink-popup-a (fn &rest args)
    :around #'diff-hl-revert-hunk-1
    (letf! ((refine-mode diff-auto-refine-mode)
            (diff-auto-refine-mode t)
            (defun diff-refine-hunk ()
            (when refine-mode
                (funcall diff-refine-hunk))
            (shrink-window-if-larger-than-buffer)))
    (apply fn args)))

  ;; UX: Don't delete the current hunk's indicators while we're editing
  (after! evil
    (add-hook! 'diff-hl-flydiff-mode-hook
      (defun +vc-gutter:init-flydiff-mode-h ()
        (if (not diff-hl-flydiff-mode)
            (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)
          (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)))))

  ;; FIX: Reverting a hunk causes the cursor to be moved to an unexpected place,
  ;;   often far from the target hunk.
  (defadvice! +vc-gutter--save-excursion-a (fn &rest args)
    "Suppresses unexpected cursor movement by `diff-hl-revert-hunk'."
    :around #'diff-hl-revert-hunk
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt))))

  (unless (display-graphic-p)
    (setq
      ;; diff-hl-margin-symbols-alist
      ;; '((insert . "+")
      ;;   (delete . "-")
      ;;   (change . "~")
      ;;   (unknown . "?")
      ;;   (ignored . "i")))
      diff-hl-margin-symbols-alist
      '((insert . "┃")
        (delete . "┃")
        (change . "┃")
        (unknown . "?")
        (ignored . "i")))
    (diff-hl-margin-mode))
  )

(package! ediff :builtin
  :commands (ediff ediff3)
  :custom
  (ediff-split-window-function #'split-window-horizontally )
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(package! git-link :auto
  :after evil
  :commands (git-link git-link-commit git-link-homepage))

(package! git-gutter :auto
  :blackout t
  :disabled t
  :hook (doom-first-buffer . global-git-gutter-mode)
  :commands (git-gutter:previous-hunk
             git-gutter:next-hunk
             git-gutter:end-of-hunk
             git-gutter:revert-hunk)
  :init
  ;; Disable in Org mode, as per
  ;; <https://github.com/syl20bnr/spacemacs/issues/10555> and
  ;; <https://github.com/syohex/emacs-git-gutter/issues/24>.
  ;; Apparently, the mode-enabling function for global minor modes
  ;; gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I
  ;; don't know why this is the case, but adding `fundamental-mode'
  ;; here fixes the issue.
  (setq git-gutter:disabled-modes
        '(so-long-mode
          image-mode asm-mode
          fundamental-mode image-mode pdf-view-mode))
  ;; Don't use git-gutter in TRAMP, it murders connection bandwidth
  (add-hook 'find-file-hook #'+git:git-gutter-find-file-hook)
  :config
  (noct-handle-popup (rx "*git-gutter:diff*"))
  (setq
   git-gutter:visual-line t
   git-gutter:update-interval 0.02
   ;; PERF: Only enable the backends that are available, so it doesn't have to
   ;;   check when opening each buffer.
   git-gutter:handled-backends
   (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                :key #'symbol-name))
   git-gutter:modified-sign " "
   git-gutter:added-sign " "
   git-gutter:deleted-sign " ")

  ;; Don't prompt when reverting hunk.
  (setq git-gutter:ask-p nil)

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  ;; Shuffle around all the hooks. `git-gutter' puts itself on a bunch
  ;; of different things, but not exactly the right things. Remove all
  ;; its meddling, and then do the right thing (run on window or
  ;; buffer switch after a top-level command, after a buffer revert,
  ;; and after Apheleia runs).
  (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
  (advice-remove #'quit-window #'git-gutter:quit-window)
  (advice-remove #'switch-to-buffer #'git-gutter:switch-to-buffer)

  (defvar radian--git-gutter-last-buffer-and-window nil
    "Cons of current buffer and selected window before last command.
This is used to detect when the current buffer or selected window
changes, which means that `git-gutter' needs to be re-run.")

  (add-hook! 'post-command-hook
   (defun radian--git-gutter-on-buffer-or-window-change ()
    "Update `git-gutter' when current buffer or selected window changes."
    (let ((new (cons (current-buffer) (selected-window))))
      (unless (equal new radian--git-gutter-last-buffer-and-window)
        (setq radian--git-gutter-last-buffer-and-window new)
        ;; Sometimes the current buffer has not gotten updated yet
        ;; after switching window, for example after `quit-window'.
        (with-current-buffer (window-buffer)
          (when git-gutter-mode
            (when buffer-file-name
              (unless (file-remote-p buffer-file-name)
                (git-gutter)))))))))

  ;; FIX: stop git-gutter:{next,previous}-hunk from jumping to random hunks.
  (defadvice! +git-gutter:fix-jumping-to-random-hunks (diffinfos is-reverse)
    :override #'git-gutter:search-near-diff-index
    (cl-position-if (let ((lineno (line-number-at-pos))
                          (fn (if is-reverse #'> #'<)))
                      (lambda (line) (funcall fn lineno line)))
                    diffinfos
                    :key #'git-gutter-hunk-start-line
                    :from-end is-reverse))
  (after! autorevert
    (add-hook! 'after-revert-hook
      "Update `git-gutter' after the buffer is autoreverted."
      (defun radian--git-gutter-after-autorevert ()
        (when git-gutter-mode
          (git-gutter))))
    )
  (after! 'apheleia
    (add-hook! 'apheleia-post-format-hook
      "Update `git-gutter' after Apheleia formats the buffer."
      (defun radian--git-gutter-after-apheleia ()
        (when git-gutter-mode
            (git-gutter))))))

(when (fboundp 'define-fringe-bitmap)
  (eval-when-compile
    (unless (fboundp 'define-fringe-bitmap)
      (fset 'define-fringe-bitmap #'ignore))
    (unless (boundp 'overflow-newline-into-fringe)
      (setq overflow-newline-into-fringe t)))
  (package! git-gutter-fringe :auto
    :disabled t
    :after git-gutter
    :after-call doom-first-buffer-hook
    :config
    (fringe-helper-define
     'radian--git-gutter-blank nil
     "........"
     "........"
     "........"
     "........"
     "........"
     "........"
     "........"
     "........")
    (defadvice! radian--advice-git-gutter-remove-bitmaps (func &rest args)
      "Disable the cutesy bitmap pluses and minuses from `git-gutter-fringe'.
Instead, display simply a flat colored region in the fringe."
      :around #'git-gutter-fr:view-diff-infos
      (letf! ((defun fringe-helper-insert-region (beg end _bitmap &rest args)
                (apply fringe-helper-insert-region
                       beg end 'radian--git-gutter-blank args)))
        (apply func args)))
    ))

(defvar +magit-project-commit-templates-alist nil
  "Alist of toplevel dirs and template strings/functions.")

;; https://tecosaur.github.io/emacs-config/config.html#magit
(after! magit
  (defun +magit-fill-in-commit-template ()
    "Insert template from `+magit-fill-in-commit-template' if applicable."
    (when-let ((template (and (save-excursion (goto-char (point-min)) (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
                              (cdr (assoc (file-name-base (directory-file-name (magit-toplevel)))
                                          +magit-project-commit-templates-alist)))))
      (goto-char (point-min))
      (insert (if (stringp template) template (funcall template)))
      (goto-char (point-min))
      (end-of-line)))
  (add-hook 'git-commit-setup-hook #'+magit-fill-in-commit-template 90))

(after! diff-hl
  (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest args)
    :override #'diff-hl-define-bitmaps
    (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
    (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))
  (defun +vc-gutter:type-face-fn (type _pos)
    (intern (format "diff-hl-%s" type)))
  (defun +vc-gutter:type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (advice-add #'diff-hl-fringe-bmp-from-pos  :override #'+vc-gutter:type-at-pos-fn)
  (advice-add #'diff-hl-fringe-bmp-from-type :override #'+vc-gutter:type-at-pos-fn)
  (setq diff-hl-draw-borders nil)
  (add-hook! 'diff-hl-mode-hook
    (defun +vc-gutter:fix-diff-hl-faces-h ()
      (mapc (doom-rpartial #'set-face-background nil)
            '(diff-hl-insert
              diff-hl-delete
              diff-hl-change)))))

(provide 'chidori-git)
;; chidori-git.el ends here
