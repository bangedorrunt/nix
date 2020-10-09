;;;; init.el -*- lexical-binding: t; -*-

;;;; HELPER FUNCTIONS
(require '+helper)

;;;; START UP

(setq user-full-name "Thanh Dung TRUONG"
      user-mail-address "braden.truong@gmail.com")

(setq
  ;; Use the develop branch of straight.el on Radian's develop branch.
  ;; (On Radian's master branch, we use the master branch of
  ;; straight.el.)
  straight-repository-branch "develop"

  ;; Improve startup time
  straight-check-for-modifications '(check-on-save find-when-checking)

  straight-cache-autoloads t
  ;; When configuring a feature with `use-package', also tell
  ;; straight.el to install a package of the same name, unless otherwise
  ;; specified using the `:straight' keyword.
  straight-use-package-by-default t

  straight-vc-git-default-clone-depth 1

  ;; Clear out recipe overrides (in case of re-init).
  straight-recipe-overrides nil
  ;; Temporary hack to fix flycheck for straight.el
  straight-fix-flycheck t)

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
;; (if (and (executable-find "watchexec")
;;          (executable-find "python3"))
;;     (setq straight-check-for-modifications '(watch-files find-when-checking))
;;     (setq straight-check-for-modifications
;;         '(find-at-startup find-when-checking)))

;; Package manager bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; (setq use-package-always-defer t)

(use-package gcmh
  :hook
  (emacs-startup . (lambda() (gcmh-mode +1)))
  (focus-out-hook . gcmh-idle-garbage-collect)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 104857600))

(if window-system
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)))

(use-package blackout
  :straight (:host github :repo "raxod502/blackout"))

(use-package no-littering
  :init
  (setq no-littering-etc-directory
    (expand-file-name ".cache/etc/" user-emacs-directory))
  (setq no-littering-var-directory
    (expand-file-name ".cache/var/" user-emacs-directory))
  :config
  (setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Save custom theme to different location
  ;; I don't use this, but when I need to use custom theme, do
  ;; (load custom-file)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq package-user-dir (expand-file-name ".cache/elpa" user-emacs-directory)))

(require '+map!)

(use-package amx)

(defvar +babygau-projects '("~/workspace/notetoself"
                            "~/workspace/fullstack-react-book"
                            "~/workspace/kobopatch-config"
                            "~/dotfiles"))

(use-package projectile
  :init
	(dolist (project +babygau-projects)
	    (projectile-add-known-project project))
	(setq projectile-project-search-path '("~/workspace/")
	      projectile-project-root '("~/workspace/")
	      projectile-ignored-projects '("~/" "/tmp" "~/dotfiles/.emacs.d/.local/straight/repos/")))

;; Completion framework
(use-package company
  :blackout t
  :defer 2
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.15)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  ;; (setq company-frontends '(company-pseudo-tooltip-frontend))
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-minimum company-tooltip-limit)
  (setq company-require-match #'company-explicit-action-p))

(use-package swiper
  :blackout t
  :defer 3)

(use-package ivy
  :blackout t
  :defer 3
  :init
  (setq ivy-use-virtual-buffers t
	      enable-recursive-minibuffers t
	      ivy-use-selectable-prompt t
	      ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
	      ivy-height 10
	      ivy-fixed-height-minibuffer t
	      ivy-count-format "(%d/%d) "
	      ivy-on-del-error-function nil
	      ivy-initial-inputs-alist nil)
  (ivy-mode 1))


(use-package ivy-rich
  :after (ivy counsel)
  ;; Must load after counsel-projectile
  :hook (counsel-projectile-mode . ivy-rich-mode)
  :init
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

;; (use-package flx)
(use-package counsel
  :blackout t
  :defer 2
  :init
  (setq ivy-re-builders-alist
	'((counsel-git-grep . ivy--regex-plus)
	(counsel-rg . ivy--regex-plus)
	(swiper . ivy--regex-plus)
	(swiper-all . ivy--regex-plus)
	(t . ivy--regex-fuzzy)))
  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")))

(use-package counsel-projectile
  :blackout t
  :after (counsel projectile))

;;;; CODE MUST-HAVE TOOLS
;;
(use-package magit
  :defer 4
  :init
    ;; Suppress the message we get about "Turning on
    ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  :config
  ;; Don't try to save unsaved buffers when using Magit. We know
  ;; perfectly well that we need to save our buffers if we want Magit
  ;; to see them.
  (setq magit-save-repository-buffers nil))

;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.
(use-feature vc-hooks
  :config
  ;; Disable VC. This improves performance and disables some annoying
  ;; warning messages and prompts, especially regarding symlinks. See
  ;; https://stackoverflow.com/a/6190338/3538165.
  (setq vc-handled-backends nil))

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(use-feature smerge-mode
  :blackout t)


;;;###autoload
;; Allow using `ESC` to quit out of popups
(defun babygau/transient-bind-escape-to-quit ()
    "Allow using `ESC` to quit out of popups"
    (define-key transient-base-map   (kbd "<escape>") 'transient-quit-one)
    (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)
    (setq transient-substitute-key-function
        'transient-rebind-quit-commands))

;; Package `transient' is the interface used by Magit to display popups.
(use-package transient
  :after magit
  :config
    ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
    ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
    ;; for discussion.
  (babygau/transient-bind-escape-to-quit)
  (transient-bind-q-to-quit))

;; (use-package eglot)

(use-package lsp-mode
  :blackout " LSP"
  :defer 2
  :hook ((rjsx-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
    ;; Disable features that have great potential to be slow.
    (setq lsp-enable-file-watchers nil
          lsp-enable-folding nil
          lsp-enable-text-document-color nil)
    (setq read-process-output-max (* 1024 1024)))
    (use-package lsp-ui
  :commands lsp-ui-mode
  :config
    (use-feature lsp-mode
      :config
      ;; With `lsp-ui', there's no need for the ElDoc integration
      ;; provided by `lsp-mode', and in fact for Bash it is very
      ;; annoying since all the hover information is multiline.
      (setq lsp-eldoc-enable-hover nil)))

(use-package lsp-ivy
  :defer t
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

(use-package tree-sitter
  :straight (tree-sitter :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el")))
(use-package tree-sitter-langs
  :straight (tree-sitter-langs :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries"))
  :after tree-sitter
  :hook (after-init . global-tree-sitter-mode)
  :config
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package flycheck
  :blackout t
  :defer 2
  :config (global-flycheck-mode))

(use-package flycheck-pos-tip
  :blackout t
  :after flycheck)

;;;; UTILITY
;;
;; Undo
(use-package undo-fu
  :blackout t
  :after evil
  :config
    ;; (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
    ;; (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
    (map! :n "u"    'undo-fu-only-undo)
    (map! :n "\C-r" 'undo-fu-only-redo))

;; Better terminal
(use-package vterm
  :defer t
  :config
  (setq vterm-kill-buffer-on-exit t))
(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-reset-window-configration-after-exit t)
  (setq vterm-toggle-fullscreen-p nil)
    (add-to-list 'display-buffer-alist
                 '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;;;; LANGUAGE
;;

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
;; Clojure
(use-package clojure-mode
  :defer t)
(use-package cider
  :defer t)
(use-package clj-refactor
  :defer t)
(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup)
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Lisp
;; (use-package sly)
;; (use-package sly-macrostep)
;; (use-package sly-repl-ansi-color)
;; (use-package highlight-quoted)
(use-package rainbow-delimiters
  :defer 4
  :hook (prog-mode . rainbow-delimiters-mode))

;; Javascript/Typescript

;; (use-package js2-mode
;;   :defer t)
(use-package rjsx-mode)
(use-package typescript-mode)

;; Web
(use-package web-mode
  :defer t)

;; YAML
(use-package yaml-mode
  :defer t)
;; JSON
(use-package json-mode
  :defer t)

;; Neuron
(use-package neuron-mode
  :defer t
  :hook (neuron-mode . company-neuron-setup)
  :config
    (customize-set-variable 'neuron-default-zettelkasten-directory (expand-file-name "~/workspace/notetoself"))
    (defun search-zettelkasten ()
      "Search zettels by content."
      (interactive)
      (progn
          (+ivy-file-search :in (neuron-zettelkasten) :recursive nil :prompt "Search Zettelkasten: ")
          (neuron-mode)))
    (defun find-file-in-zettelkasten ()
      "Find a file in the currently active zettelkasten."
      (interactive)
      (let ((default-directory (neuron-zettelkasten)))
          (counsel-find-file))))

(use-package sublimity
  :defer 3
  :init
    (require 'sublimity-scroll)
    (setq sublimity-scroll-weight 10
	  sublimity-scroll-drift-length 5
	  sublimity-scroll-vertical-frame-delay 0.01)
    (sublimity-mode 1))

;;;; THEME
;; I'm obssessed with beatiful themes

;; FlucUI Theme
;; (use-package flucui-themes
;;   :init (flucui-themes-load-style 'light))

;; DOOM Theme
(use-package doom-themes
  :init
  (load-theme 'doom-nord-light t))


;; Built-in Theme
;; (load-theme 'dichromacy)

(use-package doom-modeline
  :init
    ;; Display real file name
    (setq find-file-visit-truename t)
    (doom-modeline-init))

(set-display-table-slot standard-display-table 5 ?│)
;; (fringe-mode 0)
;; (setq linum-format "%4d \u2502 ")
(setq linum-format "%d ")
(add-hook! prog-mode '(display-line-numbers-mode))

;; element-dark: #15191e
;; iceberg-light: #e8e9ec
(custom-set-faces
  '(default ((t (:background "#e8e9ec"))))
  '(vertical-border ((t (:background "#e8e9ec"))))
  '(ivy-current-match ((t (:background nil :foreground "#18a57e"))))
  '(ivy-highlight-face ((t (:background nil :foreground "#18a57e"))))
  '(ivy-minibuffer-match-face-1 ((t (:background nil :foreground "#d65e7e"))))
  '(ivy-minibuffer-match-face-2 ((t (:background nil))))
  '(ivy-minibuffer-match-face-3 ((t (:background nil))))
  '(ivy-minibuffer-match-face-4 ((t (:background nil))))
  '(ivy-minibuffer-match-highlight ((t (:background nil))))
  '(ivy-confirm-face ((t (:background nil))))
  '(ivy-match-required-face ((t (:background nil))))
  '(ivy-confirm-face ((t (:background nil))))
  '(ivy-yanked-word ((t (:background nil))))
  '(ivy-cursor ((t (:background nil))))
  '(ivy-subdir ((t (:background nil))))
  '(isearch ((t (:background nil :slant italic :weight bold))))
  '(isearch-fail ((t (:background nil :slant italic :weight bold))))
  '(isearch-group-even ((t (:background nil :slant italic :weight bold))))
  '(isearch-group-odd ((t (:background nil :slant italic :weight bold))))
  '(lazy-highlight ((t (:background nil :slant italic :weight bold))))
  '(evil-search-forward ((t (:background nil :slant italic :weight bold))))
  '(evil-ex-search ((t (:background nil :slant italic :weight bold))))
  '(evil-ex-lazy-highlight ((t (:background nil :foreground "#d65e7e" :slant italic :weight bold))))
  '(evil-ex-substitute-matches ((t (:background nil :foreground "#d65e7e" :slant italic :weight bold))))
  '(evil-ex-substitute-replacement ((t (:background nil :foreground "#18a57e" :slant italic :weight bold))))
  '(doom-modeline-debug-visual ((t (:background nil))))
  '(next-error  ((t (:background nil :foreground "#d65e7e"))))
  '(button  ((t (:background nil)))))
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
(set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)

(use-package treemacs
  :defer t
  ;; :hook (treemacs-mode . which-key-mode)
  :init
    (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
    (defun babygau/disable-line-numbers (&optional dummy)
        (display-line-numbers-mode -1))
    (add-hook 'treemacs-mode #'babygau/disable-line-numbers)
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-directory-name-transformer    #'identity
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-extension-regex          treemacs-last-period-regex-value
        treemacs-file-follow-delay             0.2
        treemacs-file-name-transformer         #'identity
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-move-forward-on-expand        nil
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                      'left
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-user-mode-line-format         nil
        treemacs-user-header-line-format       nil
        treemacs-width                         35
        treemacs-workspace-switch-cleanup      nil)
    (setq treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face)
    treemacs-indentation 1)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                (not (null treemacs-python-executable)))
    (`(t . t)
    (treemacs-git-mode 'deferred))
    (`(t . _)
    (treemacs-git-mode 'simple)))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))


;;;; Clipboard integration

;; On macOS, clipboard integration works out of the box in windowed
;; mode but not terminal mode. The following code to fix it was
;; originally based on [1], and then modified based on [2].
;;
;; [1]: https://gist.github.com/the-kenny/267162
;; [2]: https://emacs.stackexchange.com/q/26471/12534
(if IS-MAC
  (unless (display-graphic-p)

    (defvar radian--clipboard-last-copy nil
      "The last text that was copied to the system clipboard.
       This is used to prevent duplicate entries in the kill ring.")

    (eval-and-compile
      (defun radian--clipboard-paste ()
        "Return the contents of the macOS clipboard, as a string."
        (let* (;; Setting `default-directory' to a directory that is
               ;; sure to exist means that this code won't error out
               ;; when the directory for the current buffer does not
               ;; exist.
               (default-directory "/")
               ;; Command pbpaste returns the clipboard contents as a
               ;; string.
               (text (shell-command-to-string "pbpaste")))
          ;; If this function returns nil then the system clipboard is
          ;; ignored and the first element in the kill ring (which, if
          ;; the system clipboard has not been modified since the last
          ;; kill, will be the same) is used instead. Including this
          ;; `unless' clause prevents you from getting the same text
          ;; yanked the first time you run `yank-pop'.
          (unless (string= text radian--clipboard-last-copy)
            text)))

      (defun radian--clipboard-copy (text)
        "Set the contents of the macOS clipboard to given TEXT string."
        (let* (;; Setting `default-directory' to a directory that is
               ;; sure to exist means that this code won't error out
               ;; when the directory for the current buffer does not
               ;; exist.
               (default-directory "/")
               ;; Setting `process-connection-type' makes Emacs use a pipe to
               ;; communicate with pbcopy, rather than a pty (which is
               ;; overkill).
               (process-connection-type nil)
               ;; The nil argument tells Emacs to discard stdout and
               ;; stderr. Note, we aren't using `call-process' here
               ;; because we want this command to be asynchronous.
               ;;
               ;; Command pbcopy writes stdin to the clipboard until it
               ;; receives EOF.
               (proc (start-process "pbcopy" nil "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))
        (setq radian--clipboard-last-copy text)))

    (setq interprogram-paste-function #'radian--clipboard-paste)
    (setq interprogram-cut-function #'radian--clipboard-copy)))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

;;;; Clipboard integration for terminal
(unless (display-graphic-p)
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  ;; (global-set-key [mouse-4] 'scroll-down-line)
  ;; (global-set-key [mouse-5] 'scroll-up-line)
  )

;; BETTER DEFAULT
;;

;; WTF? If I don't define this, I get weird warnings when byte compiling
(setq warning-suppress-types nil)

;; Remove the initial *scratch* message. Start with a blank screen, we
;; know what we're doing.
(setq initial-scratch-message nil)

;; Don't autosave files or create lock/history/backup files. We don't want
;; copies of potentially sensitive material floating around or polluting our
;; filesystem. We rely on git and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

;;
;;; Formatting

;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like go-mode).
(setq-default indent-tabs-mode nil
              tab-width 2
              indent-line-function 'insert-tab)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; An archaic default in the age of widescreen 4k displays? I disagree. We still
;; frequently split our terminals and editor frames, or have them side-by-side,
;; using up more of that newly available horizontal real-estate.
(setq-default fill-column 80)

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant.
(setq require-final-newline t)
;;
;;; Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)

;; Allow UTF or composed text from the clipboard, even in the terminal or on
;; non-X systems (like Windows or macOS), where only `STRING' is used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(blackout 'visual-line-mode)
(blackout 'auto-fill-mode)

;;; Electricity: automatic things
;;;; Autorevert

;; On macOS, Emacs has a nice keybinding to revert the current buffer.
;; On other platforms such a binding is missing; we re-add it here.
;; (bind-key "s-u" #'revert-buffer)

;; Feature `autorevert' allows the use of file-watchers or polling in
;; order to detect when the file visited by a buffer has changed, and
;; optionally reverting the buffer to match the file (unless it has
;; unsaved changes).
(use-feature autorevert
  :defer 2
  :init

  (defun radian--autorevert-silence ()
    "Silence messages from `auto-revert-mode' in the current buffer."
    (setq-local auto-revert-verbose nil))

  :config

  ;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
  ;; We have to do this before turning on `auto-revert-mode' for the
  ;; change to take effect. (Note that if we set this variable using
  ;; `customize-set-variable', all it does is toggle the mode off and
  ;; on again to make the change take effect, so that way is dumb.)
  (setq auto-revert-interval 1)

  (global-auto-revert-mode +1)

  ;; Auto-revert all buffers, not only file-visiting buffers. The
  ;; docstring warns about potential performance problems but this
  ;; should not be an issue since we only revert visible buffers.
  (setq global-auto-revert-non-file-buffers t)

  ;; Since we automatically revert all visible buffers after one
  ;; second, there's no point in asking the user whether or not they
  ;; want to do it when they find a file. This disables that prompt.
  (setq revert-without-query '(".*"))

  (defun radian-autorevert-inhibit-p (buffer)
    "Return non-nil if autorevert should be inhibited for BUFFER."
    (or (null (get-buffer-window))
        (with-current-buffer buffer
          (or (null buffer-file-name)
              (file-remote-p buffer-file-name)))))

  (unless EMACS27+
      (radian-defadvice radian--autorevert-only-visible
          (auto-revert-buffers &rest args)
        :around #'auto-revert-buffers
        "Inhibit `autorevert' for buffers not displayed in any window."
        (radian-flet ((defun buffer-list (&rest args)
                        (cl-remove-if
                         #'radian-autorevert-inhibit-p
                         (apply buffer-list args))))
          (apply auto-revert-buffers args)))
    (radian-defadvice radian--autorevert-only-visible (bufs)
      :filter-return #'auto-revert--polled-buffers
      "Inhibit `autorevert' for buffers not displayed in any window."
      (cl-remove-if #'radian-autorevert-inhibit-p bufs)))

  :blackout auto-revert-mode)

;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.
(use-feature eldoc
  :config

  ;; For Emacs 26 and below, `eldoc--message' is not defined. For
  ;; Emacs 27 and above, `eldoc-message' is obsolete.
  (with-no-warnings
    (radian-defadvice radian--advice-eldoc-no-trample (func &rest args)
      :around #'eldoc-print-current-symbol-info
      "Prevent `eldoc' from trampling on existing messages."
      (radian-flet ((defun eldoc-message (&optional string)
                      (if string
                          (funcall eldoc-message string)
                        (setq eldoc-last-message nil)))
                    (defun eldoc--message (&optional string)
                      (if string
                          (funcall eldoc--message string)
                        (setq eldoc-last-message nil))))
        (apply func args))))

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!

  (radian-defadvice radian--advice-eldoc-better-display-message-p (&rest _)
    :override #'eldoc--message-command-p
    "Make ElDoc smarter about when to display its messages.
By default ElDoc has a customizable whitelist of commands that it
will display its messages after. The idea of this is to not
trample on messages that other commands may have printed.
However, this is a hopeless endeavour because there are a
virtually unlimited number of commands that don't conflict with
ElDoc. A better approach is to simply check to see if a message
was printed, and only have ElDoc display if one wasn't."
    (member (current-message) (list nil eldoc-last-message)))

  :blackout t)

(use-feature elisp-mode
  :blackout ((lisp-interaction-mode . "Lisp-Interaction")
             (emacs-lisp-mode . `("ELisp"
                                  (lexical-binding
                                   ""
                                   (:propertize
                                    "/d" face warning))))))


(provide 'init.el)
;;; init.el ends here
