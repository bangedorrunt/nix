;;; -*- lexical-binding: t; -*-
;;; lisp/chidori-vertico.el --- vertico/consult/embark/orderless configuration

(package! popon (:type git :repo "https://codeberg.org/akib/emacs-popon"))

(package! embark :auto
  :blackout t
  :defer-incrementally vertico consult
  :config
;;;; General setup
  ;; Popup
  (noct-handle-popup (rx "*Embark Actions*"))
  (noct-handle-window (rx "*Embark Collect" (0+ any))
    '(display-buffer-same-window))

  ;; Which-key integration
  ;; Don't display extra embark buffer
  (setq which-key-use-C-h-commands nil)
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-quit-after-action t)

  (setq embark-indicators
        '(+embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defadvice! +embark-which-key-prompt-a (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    :around #'embark-completing-read-prompter
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'+embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  ;; Override presenting variable list as a Custom buffer.
  (add-to-list 'embark-exporters-alist '(variable . embark-export-apropos))

;;;; Keybindings
  (map!
   :map minibuffer-local-map
   [remap describe-bindings] 'embark-bindings
   "C-;"     #'embark-act
   "C-c C-;" #'embark-export
   "C-c C-l" #'embark-collect
   "C-c C-e" #'+vertico/embark-export-write
   "C-/"     #'+embark/describe-current-completion-candidate
   "C-v"     #'+embark/split-vertical-current-completion-candidate
   "C-s"     #'+embark/split-horizontal-current-completion-candidate)
  (map! :leader "a" #'embark-act))

;; Adds support for exporting a list of grep results to an honest grep-mode
;; buffer,on which you can even use wgrep if you wish
(package! embark-consult :auto
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(package! marginalia :auto
  :general ("M-A" #'marginalia-cycle)
  :hook (doom-after-init . marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(package! orderless :auto
  :after-call doom-first-input-hook
  :commands (orderless-filter)
  :config
;;;; General setup
  (setq completion-styles '(orderless flex))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles basic partial-completion))
          ;; Explicitly specify eglot to use orderless
          (eglot (styles orderless))))

  (setq orderless-matching-styles '(orderless-initialism orderless-regexp))
  ;; Default: orderless-component-separator "[ &]"
  (setq orderless-component-separator #'noct-split-orderless-component)
  (setq orderless-style-dispatchers
        (list #'noct-orderless-word-dispatcher
              #'noct-orderless-style-dispatcher
              #'oantolin-not-containing-dispatcher)))

(package! vertico (:host github :repo "minad/vertico"
                   :includes (vertico-repeat vertico-directory vertico-buffer)
                   :files ("*.el" "extensions/*.el"))
  :hook (doom-first-input . vertico-mode)
  :config
  (setq vertico-resize nil)
  (setq vertico-count 17)
  (setq vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! +vertico-suppress-completion-help-a (fn &rest args)
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args)))
;;;; Keybindings
  (map!
   :leader
   ";"   #'+vertico/project-search
   "*"   #'+vertico/search-symbol-at-point
   "/"   #'consult-line
   "."   #'consult-fd
   "ff"  #'consult-fd
   "ee"  #'consult-flymake
   "p;"  #'+vertico/project-search)
  (map!
   :map vertico-map
   "M-SPC" #'noct-toggle-orderless-separator
   ;; "ESC"   #'minibuffer-keyboard-quit
   "M-RET" #'vertico-exit))


(package! consult (:host github :repo "minad/consult"
                   :includes consult-org)
  :after vertico
  :init
  (map!
   [remap bookmark-jump]                  #'consult-bookmark
   [remap evil-show-marks]                #'consult-mark
   [remap evil-show-jumps]                #'+vertico/jump-list
   [remap evil-show-registers]            #'consult-register
   [remap goto-line]                      #'consult-goto-line
   [remap imenu]                          #'consult-imenu
   [remap locate]                         #'consult-locate
   [remap load-theme]                     #'consult-theme
   [remap man]                            #'consult-man
   [remap recentf-open-files]             #'consult-recent-file
   [remap switch-to-buffer]               #'consult-buffer
   [remap switch-to-buffer-other-window]  #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]   #'consult-buffer-other-frame
   [remap yank-pop]                       #'consult-yank-pop)
  :config
;;;; General setup
  (setq consult-project-root-function #'consult--default-project-function)
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
         --smart-case --no-heading --line-number --hidden -g \"!vendor\" -g \"!.git\" -g \"!*.svg\" .")
  (setq consult-narrow-key "<")
  (setq consult-line-numbers-widen t)
  (setq consult-async-min-input 2)
  (setq consult-async-refresh-delay  0.15)
  (setq consult-async-input-throttle 0.2)
  (setq consult-async-input-debounce 0.1)

  (defadvice! +vertico-consult--recent-file-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly"
    :before #'consult-recent-file
    (recentf-mode +1))

  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  ;; Turn of preview
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.5 any))
  (consult-customize consult-theme
   :preview-key '(:debounce 0.5 any)))

(package! wgrep :auto
  :after (vertico embark))

;;;; A few more useful configurations...
;;
(package! emacs :builtin
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defadvice! +vertico-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(provide 'chidori-vertico)
;;; chidori-vertico.el ends here
