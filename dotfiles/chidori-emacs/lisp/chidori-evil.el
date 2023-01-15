;;; lisp/chidori-evil.el --- Evil mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Evil mode configuration, for those who prefer `Vim' keybindings.

;;; Code:

(package! general :auto
  :demand t
  :blackout t
  :init
  (setq-default general-override-states
                '(insert hybrid normal visual motion operator replace emacs))
  :config
  ;; Prevent "X starts with non-prefix key Y" errors except at startup.
  (add-hook 'doom-after-init-hook #'general-auto-unbind-keys)

  ;; PERF: We use a prefix commands instead of general's
  ;;   :prefix/:non-normal-prefix properties because general is incredibly slow
  ;;   binding keys en mass with them in conjunction with :states -- an effective
  ;;   doubling of Doom's startup time!
  (define-prefix-command 'doom/leader 'doom-leader-map)
  (define-key doom-leader-map [override-state] 'all)

  ;; Bind `doom-leader-key' and `doom-leader-alt-key' as late as possible to give
  ;; the user a chance to modify them.
  ;; WARNING don't use `doom-after-module-config-hook'
  (add-hook! 'doom-after-init-hook
    (defun doom-init-leader-keys-h ()
      "Bind `doom-leader-key' and `doom-leader-alt-key'."
      (let ((map general-override-mode-map))
        (if (not (featurep 'evil))
            (progn
              (cond ((equal doom-leader-alt-key "C-c")
                     (set-keymap-parent doom-leader-map mode-specific-map))
                    ((equal doom-leader-alt-key "C-x")
                     (set-keymap-parent doom-leader-map ctl-x-map)))
              (define-key map (kbd doom-leader-alt-key) 'doom/leader))
          (evil-define-key* '(normal visual motion) map (kbd doom-leader-key) 'doom/leader)
          (evil-define-key* '(emacs insert) map (kbd doom-leader-alt-key) 'doom/leader))
        (general-override-mode +1))))

  )

;; Set these defaults before `evil'; use `defvar' so they can be changed prior
;; to loading.
(defvar +evil-want-o/O-to-continue-comments t
  "If non-nil, the o/O keys will continue comment lines if the point is on a
line with a linewise comment.")
(defvar evil-want-C-g-bindings t)
(defvar evil-want-C-i-jump nil)  ; we do this ourselves
(defvar evil-want-C-u-scroll t)  ; moved the universal arg to <leader> u
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode nil)

(package! evil :auto
  :demand t
  ;; :hook (doom-after-module-config . evil-mode)
  :preface
  (setq evil-want-keybinding nil
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
        evil-undo-system 'undo-fu)

  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  (setq-hook! '(magit-mode-hook so-long-minor-mode-hook)
    evil-ex-hl-update-delay 0.25)

  :config
  ;; I find it rather handy to be asked which buffer I want to see after splitting the window
  (setq evil-vsplit-window-right t
        evil-split-window-below t)
  (after! consult
    (defadvice! prompt-for-buffer (&rest _)
      :after '(evil-window-split evil-window-vsplit)
      (consult-buffer)))

  ;; normal states everywhere
  ;; (setq evil-default-state 'normal
  ;;       evil-normal-state-modes (append evil-emacs-state-modes
  ;;                                       evil-normal-state-modes)
  ;;       evil-emacs-state-modes nil
  ;;       evil-motion-state-modes nil)

  ;; Ensure horizontal movement doesn't cross to the next/previous line
  (setq-default evil-cross-lines nil)

  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; PERF: Stop copying the selection to the clipboard each time the cursor
  ;; moves in visual mode. Why? Because on most non-X systems (and in terminals
  ;; with clipboard plugins like xclip.el active), Emacs will spin up a new
  ;; process to communicate with the clipboard for each movement. On Windows,
  ;; older versions of macOS (pre-vfork), and Waylang (without pgtk), this is
  ;; super expensive and can lead to freezing and/or zombie processes.
  ;;
  ;; UX: It also clobbers clipboard managers (see emacs-evil/evil#336).
  (setq evil-visual-update-x-selection-p nil)

  ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
  ;; this itself, so we must.
  (setq-hook! after-change-major-mode-hook evil-shift-width tab-width)

  (after! wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  (add-hook! 'doom-escape-hook
    (defun +evil-disable-ex-highlights-h ()
      "Disable ex search buffer highlights."
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
        t)))

  (after! eldoc
    ;; Allow eldoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (unless noninteractive
    (setq save-silently t)
    (add-hook! 'after-save-hook
      (defun +evil:display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (doom-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))))

  ;; HACK '=' moves the cursor to the beginning of selection. Disable this,
  ;;      since it's more disruptive than helpful.
  (defadvice! +evil--dont-move-cursor-a (fn &rest args)
    :around #'evil-indent
    (save-excursion (apply fn args)))

  ;; HACK Invoking helpful from evil-ex throws a "No recursive edit is in
  ;;      progress" error because, between evil-ex and helpful,
  ;;      `abort-recursive-edit' gets called one time too many.
  ;; (defadvice! +evil--fix-helpful-key-in-evil-ex-a (key-sequence)
  ;;   :before #'helpful-key
  ;;   (when (evil-ex-p)
  ;;     (run-at-time 0.1 nil #'helpful-key key-sequence)
  ;;     (abort-recursive-edit)))

  ;; Make J (evil-join) remove comment delimiters when joining lines.
  (advice-add #'evil-join :around #'+evil-join-a)

  ;; Prevent gw (`evil-fill') and gq (`evil-fill-and-move') from squeezing
  ;; spaces. It doesn't in vim, so it shouldn't in evil.
  (defadvice! +evil--no-squeeze-on-fill-a (fn &rest args)
    :around '(evil-fill evil-fill-and-move)
    (letf! (defun fill-region (from to &optional justify nosqueeze to-eop)
             (funcall fill-region from to justify t to-eop))
      (apply fn args)))

  ;; Focus and recenter new splits
  ;; (advice-add #'evil-window-split  :override #'+evil-window-split-a)
  ;; (advice-add #'evil-window-vsplit :override #'+evil-window-vsplit-a)

  ;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments' to disable)
  (advice-add #'evil-open-above :around #'+evil--insert-newline-above-and-respect-comments-a)
  (advice-add #'evil-open-below :around #'+evil--insert-newline-below-and-respect-comments-a)

  ;; Lazy load evil ex commands
  (delq! 'evil-ex features)
  (add-transient-hook! 'evil-ex (provide 'evil-ex))

  (general-def
    :keymaps doom-minibuffer-maps
    "ESC"    #'abort-recursive-edit)
  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a)

  (map!
   ;; Make movement keys work like they should by remapping next to next-visual, etc.
   :n [remap evil-next-line]     #'evil-next-visual-line
   :n [remap evil-previous-line] #'evil-previous-visual-line
   :n [remap evil-next-line]     #'evil-next-visual-line
   :n [remap evil-previous-line] #'evil-previous-visual-line
   ;; We use SPC as the leader key so it shouldn't do anything when in motion
   :m "SPC" nil
   ;; Undo in region
   :v "u" #'undo)

  (evil-define-text-object +evil-a-paren (count &optional beg end type)
    "Select a paren."
    :extend-selection t
    (+evil-paren-range count beg end type t))

  (evil-define-text-object +evil-inner-paren (count &optional beg end type)
    "Select 'inner' paren."
    :extend-selection nil
    (+evil-paren-range count beg end type nil))

  (map! (:textobj "g" #'+evil-inner-paren #'+evil-a-paren))

  (map!
   :v ">" #'+evil/shift-right
   :v "<" #'+evil/shift-left)

  ;; :q should kill the current buffer rather than quitting Emacs entirely
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  ;; Unless I'm mistaken, there's no Evil backward equivalent to "e", so we'll invent it.
  (map!
   :nvm "C-e"   #'evil-backward-word-end
   :nvm "C-M-e" #'evil-backward-WORD-end)
  ;; Useful for pasting into the minibuffer where Evil modes usually don't properly function
  (map!
   :i "C-y"    #'evil-paste-after
   :i "C-S-y"  #'evil-paste-before)
  (evil-mode +1))

;; Provides defaults for many modes which evil proper overlooks
(package! evil-collection :auto
  :after evil
  :after-call doom-first-input-hook
  :blackout evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; Doesn't do anything for GUI, so don't bother. In TUI, use a line when in insert mode
(unless (display-graphic-p)
  (package! evil-terminal-cursor-changer :auto
    :after evil
    :after-call doom-first-input-hook
    :functions (evil-terminal-cursor-changer-activate)
    :config (evil-terminal-cursor-changer-activate)))

(package! evil-matchit :auto :defer 5
  ;; allows % to jump matching tags
  :after evil
  :after-call doom-first-input-hook
  :blackout t
  :defines global-evil-matchit-mode
  :config (global-evil-matchit-mode 1))

(package! evil-nerd-commenter :auto
  :after evil
  :after-call doom-first-input-hook
  :config
  (map! :nv "gc" #'evilnc-comment-operator))

(package! evil-surround :auto
  :after evil
  :after-call doom-first-input-hook
  :config
  (global-evil-surround-mode))

(package! treemacs-evil :auto
  :defer-incrementally evil treemacs
  :config
  (map!
   :map evil-treemacs-state-map
   [return] #'treemacs-RET-action
   [tab]    #'treemacs-TAB-action
   "TAB"    #'treemacs-TAB-action
   "o v"    #'treemacs-visit-node-horizontal-split
   "o s"    #'treemacs-visit-node-vertical-split))

(package! which-key :auto
  :blackout t
  :hook (doom-first-input . which-key-mode)
  :init
  (setq
   which-key-sort-order 'which-key-key-order-alpha
   which-key-sort-uppercase-first nil
   which-key-add-column-padding 1
   which-key-max-display-columns nil
   which-key-min-display-lines 6
   which-key-idle-delay 0.2
   which-key-special-keys nil) ;; don't truncate special keys
  :config
  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  (add-hook! 'doom-before-reload-hook
    (defun doom-reset-which-key-replacements-h ()
      (setq which-key-replacement-alist (get 'which-key-replacement-alist 'initial-value))))

  (setq-hook! which-key-init-buffer-hook line-spacing 3)
  (which-key-add-key-based-replacements doom-leader-key "<leader>")
  (which-key-add-key-based-replacements doom-localleader-key "<localleader>"))

(package! undo-fu :auto
  :after evil
  :after-call doom-first-buffer-hook
  :custom
  (undo-fu-allow-undo-in-region nil)
  (undo-fu-ignore-keyboard-quit t)
  ;; Customize undo limits to ensure lots of history is retained, essentially
  ;; settings limits to the same values as undo-tree does.
  (undo-limit 80000000)
  (undo-strong-limit 120000000)
  (undo-outer-limit 360000000))

(package! undo-fu-session :auto
  :hook (doom-first-buffer . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-directory (expand-file-name "undo-fu-session/" chidori-cache-dir))
  (undo-fu-session-linear nil)
  (undo-fu-session-compression 'gz)
  (undo-fu-session-file-limit nil)
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'"
                                        "/git-rebase-todo\\'")))

(package! undo-tree :auto
  :disabled t
  :blackout t
  :hook (doom-first-buffer . global-undo-tree-mode)
  :custom
  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree/" chidori-cache-dir))))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)

  :config
  (add-hook! 'before-save-hook (undo-tree-save-history nil t))
  (defadvice! +undo-tree-quiet-message-a (fn &rest args)
    :around #'undo-tree-save-history
    (let ((message-log-max nil)
          (inhibit-message t))
      (apply fn args))))

(provide 'chidori-evil)
;;; chidori-evil.el ends here
