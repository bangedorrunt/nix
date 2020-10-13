;;;; +map!.el -*- lexical-binding: t; -*-

;;;; SPACEMACS
;;
(cond (IS-MAC
       (setq mac-command-modifier     'super
             mac-option-modifier      'meta
             ns-command-modifier      'super
             ns-option-modifier       'meta
             ns-right-option-modifier 'none))
      (IS-WINDOWS
       (setq w32-lwindow-modifier 'super
             w32-rwindow-modifier 'super)))

(defvar doom-leader-key "SPC"
  "The leader prefix key for Evil users.")
(defvar doom-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")
(defvar doom-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")
(defvar doom-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users.")
(defvar doom-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

;; Once upon a time, I used `:hook (after-init evil-mode)'
;; That was a big mistake, and make `map!' is deffective
;; because `evil' is loaded too late
(use-package evil
  :init
  (setq evil-search-module 'evil-search
        ;; `evil' already has built-in undo-redo feature
        evil-undo-system 'undo-redo
        evil-magic 'very-magic
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-want-C-u-scroll t
        ;; This variable explained why I couldn't use C-z
        evil-toggle-key ""
        ;; Disable evil state displayed above modeline
        evil-echo-state nil)
  (setq evil-shift-width 2)
  (evil-mode))

(use-package evil-surround
  ;; `:commands' automatically defer package
  ;; :commands (global-evil-surround-mode
  ;;            evil-surround-edit
  ;;            evil-Surround-edit
  ;;            evil-surround-region)
  :config (global-evil-surround-mode 1))

;; (use-package evil-embrace)

;; (use-package evil-textobj-anyblock)
;; (use-package evil-visualstar)
(use-package evil-nerd-commenter)


;; NOTE: I have to use this extention
;; to quit on `Ivy` buffers
(use-package evil-collection
  :init
  (evil-collection-init))

;; Need it for universal escape
;; (use-package evil-escape
;;   :straight (:host github :repo "hlissner/evil-escape")
;;   :after evil)

(use-package evil-magit
  :after (evil magit)
  :init
  (setq evil-magit-state 'normal))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package general
  :init
  ;; Convenience aliases
  (defalias 'define-key! #'general-def)
  (defalias 'undefine-key! #'general-unbind))

;; HACK `map!' uses this instead of `define-leader-key!' because it consumes
;; 20-30% more startup time, so we reimplement it ourselves.
(defmacro doom--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (doom-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key doom-leader-map (general--kbd ,key)
                       ,bdef)
                    forms))
            (when-let (desc (cadr (memq :which-key udef)))
              (prependq!
               wkforms `((which-key-add-key-based-replacements
                           (general--concat t doom-leader-alt-key ,key)
                           ,desc)
                         (which-key-add-key-based-replacements
                           (general--concat t doom-leader-key ,key)
                           ,desc))))))))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.
Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.
See `doom-leader-key' and `doom-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'doom-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.
Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.
See `doom-localleader-key' and `doom-localleader-alt-key' to change the
localleader prefix."
  (if (featurep 'evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix doom-localleader-key
        :non-normal-prefix doom-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix doom-localleader-alt-key
      ,@args)))

;; We use a prefix commands instead of general's :prefix/:non-normal-prefix
;; properties because general is incredibly slow binding keys en mass with them
;; in conjunction with :states -- an effective doubling of Doom's startup time!
(define-prefix-command 'doom/leader 'doom-leader-map)
(define-key doom-leader-map [override-state] 'all)


;; Bind `doom-leader-key' and `doom-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook! 'after-init-hook
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

; ;
;;; Packages

(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-idle-delay 0.3
        )
  :config
  ;; General improvements to which-key readability
  (which-key-add-key-based-replacements doom-leader-key "<leader>")
  (which-key-add-key-based-replacements doom-localleader-key "<localleader>"))
;;
;;; `map!' macro

(defvar doom-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun doom--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.
For example, :nvi will map to (list 'normal 'visual 'insert). See
`doom-evil-state-alist' to customize this."
  (cl-loop for l across (doom-keyword-name keyword)
           if (assq l doom-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))


;; specials
(defvar doom--map-forms nil)
(defvar doom--map-fn nil)
(defvar doom--map-batch-forms nil)
(defvar doom--map-state '(:dummy t))
(defvar doom--map-parent-state nil)
(defvar doom--map-evil-p nil)
(after! evil (setq doom--map-evil-p t))

(defun doom--map-process (rest)
  (let ((doom--map-fn doom--map-fn)
        doom--map-state
        doom--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (doom--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (doom--map-commit)
                  (setq doom--map-fn 'doom--define-leader-key))
                 (:localleader
                  (doom--map-commit)
                  (setq doom--map-fn 'define-localleader-key!))
                 (:after
                  (doom--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (doom--map-set :keymaps `(quote ,(doom-enlist (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (doom-enlist (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (doom--map-nested (list (intern (doom-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (doom-enlist (pop rest))
                    (let ((keymap (intern (format "doom-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            doom--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (doom-enlist (pop rest))
                    (doom--map-set (if doom--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          doom--map-forms)))
                 (_
                  (condition-case _
                      (doom--map-def (pop rest) (pop rest)
                                     (doom--map-keyword-to-states key)
                                     desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((doom--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (doom--map-commit)
    (macroexp-progn (nreverse (delq nil doom--map-forms)))))

(defun doom--map-append-keys (prop)
  (let ((a (plist-get doom--map-parent-state prop))
        (b (plist-get doom--map-state prop)))
    (if (and a b)
        ;; Fix #4074: map! with meta/hyper modifier prefixes 
        ;; `(general--concat nil ,a ,b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun doom--map-nested (wrapper rest)
  (doom--map-commit)
  (let ((doom--map-parent-state (doom--map-state)))
    (push (if wrapper
              (append wrapper (list (doom--map-process rest)))
            (doom--map-process rest))
          doom--map-forms)))

(defun doom--map-set (prop &optional value)
  (unless (equal (plist-get doom--map-state prop) value)
    (doom--map-commit))
  (setq doom--map-state (plist-put doom--map-state prop value)))

(defun doom--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (doom-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state doom--map-batch-forms)))
  t)

(defun doom--map-commit ()
  (when doom--map-batch-forms
    (cl-loop with attrs = (doom--map-state)
             for (state . defs) in doom--map-batch-forms
             if (or doom--map-evil-p (not state))
             collect `(,(or doom--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) doom--map-forms))
    (setq doom--map-batch-forms nil)))

(defun doom--map-state ()
  (let ((plist
         (append (list :prefix (doom--map-append-keys :prefix)
                       :infix  (doom--map-append-keys :infix)
                       :keymaps
                       (append (plist-get doom--map-parent-state :keymaps)
                               (plist-get doom--map-state :keymaps)))
                 doom--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.
If evil isn't loaded, evil-specific bindings are ignored.
States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')
  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').
Properties
  :leader [...]                   an alias for (:prefix doom-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]
  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds."
  (doom--map-process rest))

;;;; Universal escape mode aka C-g
;; In honor of doom-emacs creator

;;;###autoload
(defun +evil-escape-a (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

(add-hook! 'doom-escape-hook
    (defun +evil-disable-ex-highlights-h ()
        "Disable ex search buffer highlights."
        (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
                t)))

;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
(advice-add #'evil-force-normal-state :after #'+evil-escape-a)

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape ()
  "Run `doom-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
            ;; quit the minibuffer if open.
            (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'doom-escape-hook))
        ;; Also abort macros
        ((or defining-kbd-macro executing-kbd-macro) t)
        ;; Back to the default
        ((keyboard-escape-quit))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(map!
  ;; [escape]   #'evil-escape
  :n "o"     #'+evil/insert-newline-below
  :n "O"     #'+evil/insert-newline-above
  :n "gcc"   #'evilnc-comment-or-uncomment-lines
  :v "gc"    #'evilnc-comment-or-uncomment-lines)
(map!
  :leader
  ":"        #'counsel-M-x
  ";"        #'counsel-rg
  "u"        #'universal-argument
  "."        #'counsel-find-file
  ","        #'ivy-switch-buffer
  "SPC"      #'counsel-fzf
  "/"        #'swiper
  "TAB"      #'eval-last-sexp

  (:prefix ("q" . "quits")
	"q"        #'save-buffers-kill-terminal
  "r"        #'+default/restart-server
	"Q"        #'evil-quit-all-with-error-code)

  (:prefix ("b" . "buffers")
	"s"        #'basic-save-buffer
	"d"        #'kill-current-buffer
	"b"        #'ivy-switch-buffer
	"p"        #'previous-buffer
	"n"        #'next-buffer
	"x"        #'(lambda ()
                 (switch-to-buffer (other-buffer)))
	"N"        #'evil-buffer-new
	"z"        #'bury-buffer)

  (:prefix ("f" . "files")
	"n"        #'new-file
	"D"        #'delete-file
	"r"        #'rename-file
	"d"        #'move-file-to-trash
	"f"        #'counsel-find-file)

  (:prefix ("c" . "code")
  "c"        #'evilnc-comment-or-uncomment-lines
  "f"        #'lsp-format-buffer
  "j"        #'lsp-ivy-workspace-symbol
  "J"        #'lsp-ivy-global-workspace-symbol
  "r"        #'lsp-rename)

  (:prefix ("h" . "help")
	"?"        #'help-command
  "i"        #'info
  "c"        #'counsel-faces
	"f"        #'counsel-describe-function
  "k"        #'describe-key
	"v"        #'counsel-describe-variable)

  (:prefix ("e" . "error")
  "l"        #'counsel-flycheck
  "n"        #'flycheck-next-error
  "p"        #'flycheck-previous-error
  "a"        #'counsel-flycheck-errors-action)

  (:prefix ("g" . "git")
  "/"        #'magit-dispatch
  "'"        #'forge-dispatch
  "b"        #'magit-branch-checkout
  "g"        #'magit-status
  "G"        #'magit-status-here
  "D"        #'magit-file-delete
  "B"        #'magit-blame-addition
  "C"        #'magit-clone
  "F"        #'magit-fetch
  "L"        #'magit-log
  "S"        #'magit-stage-file
  "U"        #'magit-unstage-file)

  (:prefix ("p" . "project-manager")
	"b"        #'counsel-projectile-switch-to-buffer
	"f"        #'counsel-projectile-find-file
	"p"        #'counsel-projectile-switch-project
	"!"        #'projectile-run-shell-command-in-root
	"a"        #'projectile-add-known-project
	"e"        #'projectile-edit-dir-locals
	"d"        #'projectile-remove-known-project
	"g"        #'projectile-configure-project
	"i"        #'projectile-invalidate-cache
	"k"        #'projectile-kill-buffers
	"o"        #'projectile-find-other-file
	"r"        #'counsel-recentf
	"R"        #'projectile-run-project
	"s"        #'projectile-save-project-buffers
	"T"        #'projectile-test-project)

  (:prefix ("w" . "windows")
	"h"        #'evil-window-left
	"j"        #'evil-window-down
	"k"        #'evil-window-up
	"l"        #'evil-window-right
	"s"        #'evil-window-split
	"v"        #'evil-window-vsplit
	"w"        #'other-window
	"c"        #'delete-window)

  (:prefix ("x" . "edit-texts")
	"a"        #'align)

  (:prefix ("t" . "toggle")
	"t"        #'vterm-toggle
	"s"        #'treemacs)

  (:prefix ("z" . "zettel-mode")
	"z"        #'neuron-new-zettel
  "e"        #'neuron-edit-zettel
  "w"        #'neuron-rib-watch
  "g"        #'neuron-rib-generate
  "o"        #'neuron-open-zettel
  "O"        #'neuron-open-index
  "j"        #'neuron-open-daily-notes
  "t"        #'neuron-query-tags
  "r"        #'neuron-refresh
  "c"        #'neuron-edit-zettelkasten-configuration))
(provide '+map!)
;;; +map!.el ends here
