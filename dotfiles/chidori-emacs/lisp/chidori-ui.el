;;; lisp/chidori-ui.el -*- lexical-binding: t; -*-

;;; Code:

;;;; All about that font

;;;;; Enable rendering SF symbols on macOS.
(when (memq system-type '(darwin))
  (set-fontset-font t nil "SF Compact Text" nil 'append))

;;;; Configuration for `monospace'
(defvar monos-default-spec '(:family "SF Pro Display" :height 220))
(defvar monos-fixed-pitch-spec '(:family "SF Mono" :height 220))

(dolist (face '(mode-line mode-line-inactive))
  (set-face-attribute face nil :height 151))

(set-face-attribute 'fill-column-indicator nil
                    :family "SF Mono")

;;;;; Special settings for the GUI minibuffer

(when (display-graphic-p)

  ;; Use a different font for some non-interactive prompt elements
  (mapc
   ;; Other fonts worth trying maybe:
   ;; - Hoefler Text
   ;; - Bodoni 72 Book
   ;; - Optima
   ;; - Charter
   (lambda (face)
     (set-face-attribute face nil
                         :family "SF Pro Text"
                         :slant 'italic
                         :height 170))
   '(minibuffer-prompt)))

;;;;; Make sure SF Symbols and Octicons will be displayed.
(when (display-graphic-p)
  (dolist (font-pua-points '(("SF Pro Text" . (#x100000 . #x102000))
                             ("github-octicons" . (#xf000 . #xf27c))))
    (let ((family (car font-pua-points))
          (range (cdr font-pua-points)))
      (set-fontset-font t range family nil 'prepend))))

(package! monospace-mode :builtin
  :hook (((prog-mode . monospace-mode)
          (nxml-mode . monospace-mode)
          (conf-mode . monospace-mode)
          (ediff-mode . monospace-mode)
          (eval-expression-minibuffer-setup . monospace-mode)
          (minibuffer-setup . monos-activate-for-minibuffer-prompt))))

;;;;
;;;; Configuration for general UI
;;;;

(package! dashboard :auto
  ;; :disabled t
  :hook (doom-load-theme . dashboard-setup-startup-hook)
  :config
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Chidori Emacs")
  ;; Set the banner
  (setq dashboard-startup-banner (thread-last user-emacs-directory (expand-file-name "misc/emacs-e-dashboard-300.png")))
  (setq dashboard-projects-backend 'project-el)

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-init-info t)
  (dashboard-refresh-buffer)
  ;; (dashboard-setup-startup-hook)
  )

(package! modus-themes :builtin
  :init
  (setq
   modus-themes-custom-auto-reload t
   modus-themes-italic-constructs t
   modus-themes-bold-constructs nil
   modus-themes-completions '((matches . (extrabold))
                              (selection . (semibold italic text-also)))
   modus-themes-headings '((1 . (variable-pitch 1.5))
                           (2 . (1.3))
                           (agenda-date . (1.3))
                           (agenda-structure . (variable-pitch light 1.8))
                           (t . (1.1))))
  :config
  (defconst modus-vivendi-palette
    '((bg-main     "#14141f")
      (bg-dim      "#181825")
      (bg-active   "#181825")
      (bg-inactive "#11111b")
      (border      "#181825")
      )
    "Use basic catppuccin pallete")

  (setq modus-vivendi-palette-overrides modus-vivendi-palette)

  (setq modus-themes-common-palette-overrides
        `((bg-region bg-sage)
          (fg-region fg-main)
          (bg-mode-line-active bg-active)
          (bg-mode-line-inactive bg-inactive)
          (fg-mode-line-active fg-main)
          (border-mode-line-active bg-inactive)
          (border-mode-line-inactive bg-dim)
          (overline-heading-1 bg-main)
          (overline-heading-2 bg-main)
          (overline-heading-3 bg-main)
          (overline-heading-4 bg-main)
          (bg-heading-1 bg-main)
          (bg-heading-2 bg-main)
          (bg-heading-3 bg-main)
          (bg-heading-4 bg-main)
          (fg-heading-1 blue-warmer)
          (fg-heading-2 yellow-cooler)
          (fg-heading-3 cyan-cooler)
          (comment green-cooler)
          (string green)
          ,@modus-themes-preset-overrides-faint
          ))

  (add-hook 'modus-themes-after-load-theme-hook #'+modus-themes-custom-faces)
  )

(package! all-the-icons :auto
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)
  :preface
  (add-hook! 'after-setting-font-hook
    (defun doom-init-all-the-icons-fonts-h ()
      (when (fboundp 'set-fontset-font)
        (dolist (font (list "Weather Icons"
                            "github-octicons"
                            "FontAwesome"
                            "all-the-icons"
                            "file-icons"
                            "Material Icons"))
          (set-fontset-font t 'unicode font nil 'append)))))
  :config
  (cond ((daemonp)
         (defadvice! doom--disable-all-the-icons-in-tty-a (fn &rest args)
           "Return a blank string in tty Emacs, which doesn't support multiple fonts."
           :around '(all-the-icons-octicon
                     all-the-icons-material
                     all-the-icons-faicon
                     all-the-icons-fileicon
                     all-the-icons-wicon
                     all-the-icons-alltheicon)
           (if (or (not after-init-time) (display-multi-font-p))
               (apply fn args)
             "")))
        ((not (display-graphic-p))
         (defadvice! doom--disable-all-the-icons-in-tty-a (&rest _)
           "Return a blank string for tty users."
           :override '(all-the-icons-octicon
                       all-the-icons-material
                       all-the-icons-faicon
                       all-the-icons-fileicon
                       all-the-icons-wicon
                       all-the-icons-alltheicon)
           ""))))

(package! doom-modeline :auto
  ;; NOTE vterm-module compilation is triggered during first fresh installation
  ;;; and this will interupt `doom-first-buffer-hook' initializtion, use
  ;;; `doom-after-init-hook' will fix this issue
  :hook (doom-after-init . doom-modeline-mode)
  :init
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type
        (cond (IS-MAC 2)
              (IS-WINDOWS 1)
              (0)))

  ;; Fix modeline icons in daemon-spawned graphical frames. We have our own
  ;; mechanism for disabling all-the-icons, so we don't need doom-modeline to do
  ;; it for us. However, this may cause unwanted padding in the modeline in
  ;; daemon-spawned terminal frames. If it bothers you, you may prefer
  ;; `doom-modeline-icon' set to `nil'.
  (when (daemonp)
    (setq doom-modeline-icon t))
  :config
  ;; HACK Fix #4102 due to empty all-the-icons return value (caused by
  ;;      `doom--disable-all-the-icons-in-tty-a' advice) in tty daemon frames.
  (defadvice! +modeline-disable-icon-in-daemon-a (fn &rest args)
    :around #'doom-modeline-propertize-icon
    (when (display-graphic-p)
      (apply fn args)))

  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'after-setting-font-hook #'+modeline-resize-for-font-h)
  ;; (add-hook 'modus-themes-after-load-theme-hook #'doom-modeline-refresh-bars)

  ;; Some functions modify the buffer, causing the modeline to show a false
  ;; modified state, so force them to behave.
  (defadvice! +modeline--inhibit-modification-hooks-a (fn &rest args)
    :around #'ws-butler-after-save
    (with-silent-modifications (apply fn args)))
  (doom-modeline-mode))

(package! sfsymbols-modeline :builtin
  :defer-incrementally doom-modeline
  :config (sfsymbols-modeline-mode))

(package! hl-todo :auto
  :hook (doom-first-buffer . global-hl-todo-mode))

(package! treemacs :auto
  :defer 3
  :init
  (setq
   treemacs-no-png-images t
   treemacs-resize-icons 44 ;; retina display
   treemacs-follow-after-init t
   ;; treemacs-is-never-other-window t
   treemacs-display-in-side-window nil ;; compatible with evil
   treemacs-sorting 'alphabetic-case-insensitive-asc
   treemacs-persist-file (expand-file-name "treemacs-persist" chidori-cache-dir)
   treemacs-last-error-persist-file (expand-file-name "treemacs-last-error-persist" chidori-cache-dir))
  :config
  (treemacs-follow-mode -1)
  (treemacs-project-follow-mode)
  (map! :leader "tt" #'treemacs))

(package! helpful :auto
  :after-call doom-first-input-hook
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)
  (map!
   [remap describe-function] #'helpful-callable
   [remap describe-command]  #'helpful-command
   [remap describe-variable] #'helpful-variable
   [remap describe-key]      #'helpful-key
   [remap describe-symbol]   #'helpful-symbol)
  (defun doom-use-helpful-a (fn &rest args)
    "Force FN to use helpful instead of the old describe-* commands."
    (letf! ((#'describe-function #'helpful-function)
            (#'describe-variable #'helpful-variable))
      (apply fn args)))
  :config
  ;; REVIEW See Wilfred/elisp-refs#35. Remove once fixed upstream.
  (unless (version< emacs-version "29")
    (defvar read-symbol-positions-list nil))
  ;; HACK `help-fns--autoloaded-p's signature changed on Emacs 29. This
  ;; suppresses the error until it is addressed upstream. Basically we just
  ;; override the function to ignore the second argument.
  (unless (version< emacs-version "29")
    (advice-add #'help-fns--autoloaded-p :around
                (lambda (fn sym &rest args)
                  (apply fn (list sym)))))

  (map!
   :map helpful-mode-map
   :n "q"   #'kill-this-buffer
   :n "?"   #'describe-mode)

  (map!
   :leader
   "hdf" #'helpful-function
   "hdF" #'describe-face
   "hda" #'helpful-symbol
   "hdb" #'describe-bindings
   "hdv" #'helpful-variable
   "hdm" #'helpful-macro
   "hdM" #'describe-mode
   "hdk" #'helpful-key
   "hdK" #'describe-keymap
   "hdc" #'helpful-callable
   "hdC" #'describe-char
   "hdp" #'describe-package)
  )

;; A simple confirmation prompt when killing Emacs. But only prompt when there
;; are real buffers open.
(setq confirm-kill-emacs #'doom-quit-p)
;; Prompt for confirmation when deleting a non-empty frame; a last line of
;; defense against accidental loss of work.
(global-set-key [remap delete-frame] #'doom/delete-frame-with-prompt)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)
;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;
;;; Buffers

(defadvice! doom--switch-to-fallback-buffer-maybe-a (&rest _)
  "Switch to `doom-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
  :before-until #'kill-current-buffer
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window)
           t)
          ((eq buf (doom-fallback-buffer))
           (message "Can't kill the fallback buffer.")
           t)
          ((doom-real-buffer-p buf)
           (let ((visible-p (delq (selected-window) (get-buffer-window-list buf nil t))))
             (unless visible-p
               (when (and (buffer-modified-p buf)
                          (not (y-or-n-p
                                (format "Buffer %s is modified; kill anyway?"
                                        buf))))
                 (user-error "Aborted")))
             (let ((inhibit-redisplay t)
                   buffer-list-update-hook)
               (when (or ;; if there aren't more real buffers than visible buffers,
                      ;; then there are no real, non-visible buffers left.
                      (not (cl-set-difference (doom-real-buffer-list)
                                              (doom-visible-buffers)))
                      ;; if we end up back where we start (or previous-buffer
                      ;; returns nil), we have nowhere left to go
                      (memq (switch-to-prev-buffer nil t) (list buf 'nil)))
                 (switch-to-buffer (doom-fallback-buffer)))
               (unless visible-p
                 (with-current-buffer buf
                   (restore-buffer-modified-p nil))
                 (kill-buffer buf)))
             (run-hooks 'buffer-list-update-hook)
             t)))))

(package! fringe :builtin
  :hook (doom-first-buffer . fringe-mode)
  :config
  ;; Show final newline location with angle bracket in the fringe
  (setq
   indicate-buffer-boundaries '((bottom . left))
   default-frame-alist '((left-fringe . 8)
                         (right-fringe . 0))
   fringe-indicator-alist (assoc-delete-all 'truncation fringe-indicator-alist))

  ;; UI: make the fringe small enough that the diff bars aren't too domineering,
  ;;   while leaving enough room for other indicators.
  (fringe-mode '8)
  ;; UI: the gutter looks less cramped with some space between it and  buffer.
  (setq-default fringes-outside-margins t)
  (add-hook!
   'after-make-frame-functions
   '(+ui-set-up-display-for-terminal
     +ui-disable-not-useful-fringes
     +ui-configure-fringes-and-margins))
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(package! display-line-numbers :builtin
  :hook (doom-first-buffer . global-display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)
  ;; Preserve line numbering from buffer start when "narrowed" (only for
  ;; display-line-numbers-mode)
  (display-line-numbers-widen t)
  :config
  (defun +evil--display-line-numbers ()
    (setq display-line-numbers t))
  (defun +evil--display-relative-line-numbers ()
    (setq display-line-numbers 'relative))
  ;; Toggle line number between evil modes
  (add-hook 'evil-insert-state-entry-hook #'+evil--display-line-numbers)
  (add-hook 'evil-insert-state-exit-hook #'+evil--display-relative-line-numbers))

;; Allow visual lines
(package! simple :builtin
  :blackout visual-line-mode
  :hook (doom-first-buffer . global-visual-line-mode)
  :custom
  ;; move via visual lines
  (line-move-visual t))

;; Use native smooth scrolling
(package! pixel-scroll :builtin
  :hook (doom-first-buffer . pixel-scroll-precision-mode)
  :custom (pixel-scroll-precision-interpolation-factor 10))

;; Nicer glyphs
(when (and (require 'disp-table nil 'noerror) standard-display-table)
  (set-display-table-slot standard-display-table 'truncation ?…)
  (set-display-table-slot standard-display-table 'wrap ?↘)
  (set-display-table-slot standard-display-table 'selective-display
                          (string-to-vector " …")))

;;
;;;; Windows/frames
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

;;;; Tabs for window layouts (tab-bar.el and prot-tab.el)
;;
(package! tab-bar :builtin
  :hook (doom-after-init . tab-bar-mode)
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
  (setq tab-bar-auto-width-max '(120 . 20)) ; Emacs 29
  ;; Same concept as `winner-mode'.  See the `prot-tab-winner-undo' and
  ;; its counterpart.
  (tab-bar-history-mode 1))

;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(setq use-short-answers t)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Quite often there are superfluous files I’m not that interested in.
;; There’s no good reason for them to take up space.
;; Let’s add a mechanism to ignore them.
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter)

  (setq treemacs-file-ignore-extensions
        '(;; LaTeX
          "aux"
          "ptc"
          "fdb_latexmk"
          "fls"
          "synctex.gz"
          "toc"
          ;; LaTeX - glossary
          "glg"
          "glo"
          "gls"
          "glsdefs"
          "ist"
          "acn"
          "acr"
          "alg"
          ;; LaTeX - pgfplots
          "mw"
          ;; LaTeX - pdfx
          "pdfa.xmpi"
          ))
  (setq treemacs-file-ignore-globs
        '(;; LaTeX
          "*/_minted-*"
          ;; AucTeX
          "*/.auctex-auto"
          "*/_region_.log"
          "*/_region_.tex")))

;; Already loaded at early-init
(load-theme 'modus-vivendi)
(+modus-themes-custom-faces)

(provide 'chidori-ui)
;;; chidori-ui.el ends here
