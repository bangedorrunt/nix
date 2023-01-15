;;; lisp/chidori-binding.el -*- lexical-binding: t; -*-

;;;; Reasonble default
(after! which-key
  (monos-use-fixed-pitch 'which-key-key-face
                         'which-key-note-face
                         'which-key-special-key-face
                         'which-key-command-description-face
                         'which-key-docstring-face
                         'which-key-separator-face
                         'which-key-group-description-face
                         'which-key-highlighted-command-face
                         'which-key-local-map-description-face))
(after! diff-mode
  (monos-use-fixed-pitch 'diff-added
                         'diff-changed
                         'diff-context
                         'diff-function
                         'diff-indicator-added
                         'diff-indicator-changed
                         'diff-indicator-removed
                         'diff-nonexistent
                         'diff-refine-added
                         'diff-refine-changed
                         'diff-refine-removed
                         'diff-removed))

(after! magit
  (monos-use-fixed-pitch 'magit-diff-added
                         'magit-diff-added-highlight
                         'magit-diff-base
                         'magit-diff-base-highlight
                         'magit-diff-conflict-heading
                         'magit-diff-context
                         'magit-diff-context-highlight
                         'magit-diff-hunk-heading
                         'magit-diff-hunk-heading-highlight
                         'magit-diff-hunk-heading-selection
                         'magit-diff-hunk-region
                         'magit-diff-lines-boundary
                         'magit-diff-lines-heading
                         'magit-diff-our
                         'magit-diff-our-highlight
                         'magit-diff-removed
                         'magit-diff-removed-highlight
                         'magit-diff-revision-summary
                         'magit-diff-revision-summary-highlight
                         'magit-diff-their
                         'magit-diff-their-highlight
                         'magit-diff-whitespace-warning
                         'magit-diffstat-added
                         'magit-diffstat-removed
                         'magit-hash))

(after! markdown-mode
  (monos-use-fixed-pitch 'markdown-inline-code-face
                         'markdown-code-face))

(after! rainbow-delimiters
  ;;TODO: Ideally just setting the base face would be sufficient here.
  (monos-use-fixed-pitch 'rainbow-delimiters-base-face
                         'rainbow-delimiters-depth-1-face
                         'rainbow-delimiters-depth-2-face
                         'rainbow-delimiters-depth-3-face
                         'rainbow-delimiters-depth-4-face
                         'rainbow-delimiters-depth-5-face
                         'rainbow-delimiters-depth-6-face
                         'rainbow-delimiters-depth-7-face
                         'rainbow-delimiters-depth-8-face
                         'rainbow-delimiters-depth-9-face
                         'rainbow-delimiters-mismatched-face
                         'rainbow-delimiters-unmatched-face))

(after! vertico-quick
  (monos-use-fixed-pitch 'vertico-quick1
                         'vertico-quick2))

(after! which-func
  (monos-use-fixed-pitch 'which-func)
  (set-face-attribute
   'which-func nil
   :height (face-attribute 'mode-line :height)))


;;
;;;; Global keybind settings

(cond
 (IS-MAC
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))
 (IS-WINDOWS
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

(defalias '+buffer/kill   #'kill-this-buffer)
(defalias '+buffer/switch #'consult-buffer)
;; (defalias 'consult-fd     #'project-find-file)

(after! eldoc
  (eldoc-add-command #'doom/escape))

;; Use tmux binding if in terminal
(when (display-graphic-p)
  (map!
    "C--" #'+editor/decrease-font-size
    "C-=" #'+editor/increase-font-size
    "C-h" #'windmove-left
    "C-j" #'windmove-down
    "C-k" #'windmove-up
    "C-l" #'windmove-right
    ))

(map!
 [remap keyboard-quit] #'doom/escape)

(map!
 :map special-mode-map
 :n "q"   #'quit-window)

(map!
 :n "o"   #'+evil/insert-newline-below-and-follow
 :n "O"   #'+evil/insert-newline-above-and-follow)

(map!
 :leader
 "SPC" #'project-find-file
 "TAB" #'eval-last-sexp
 "ESC" #'keyboard-quit
 "C-g" #'keyboard-quit
 ","   #'+buffer/switch-to-previous
 ":"   #'execute-extended-command
 "/"   #'consult-line

 "b"   '(:ignore t :wk "buffer")
 "TAB" #'+buffer/switch-to-previous
 "bK"  #'+buffer/kill-others
 "bP"  #'+buffer/copy-clipboard
 "bR"  #'+buffer/safe-revert
 "bY"  #'+buffer/copy-to-clipboard
 "bb"  #'+buffer/switch
 "bd"  #'+buffer/kill
 "be"  #'+buffer/safe-erase
 "bh"  #'+buffer/switch-to-scratch
 "bj"  #'bookmark-jump
 "bk"  #'+buffer/kill
 "bm"  #'bookmark-set
 "bn"  #'evil-next-buffer
 "bp"  #'evil-prev-buffer
 "bs"  #'bookmark-save
 "bw"  #'read-only-mode

 "c"   '(:ignore t :wk "code")

 "f"   '(:ignore t :wk "files")
 "fCd" #'+file/unix2dos
 "fCu" #'+file/dos2unix
 "fD"  #'+file/delete
 "fE"  #'+file/sudo-edit
 "fR"  #'+file/rename
 "fS"  #'evil-write-all
 "fY"  #'+file/copy-buffer-filename
 "fb"  #'bookmark-jump
 "feg" #'+file/open-gtd
 "fei" #'+file/open-init
 "fek" #'+file/open-keybind
 "fet" #'+file/open-test
 "ff"  #'consult-fd
 "fj"  #'dired-jump
 "fl"  #'find-file-literally
 "fo"  #'+file/open-in-external-app
 "fr"  #'consult-recent-file
 "fs"  #'save-buffer
 "fw"  #'consult-ripgrep
 "fy"  #'+file/show-and-copy-buffer-filename

 "h"   '(:ignore t :wk "help/manual")
 "hd"  '(:ignore t :wk "describe")
 "hI"  #'info-apropos
 "hM"  #'woman
 "hi"  #'info
 "hm"  #'man

 "hp"  '(:ignore t :wk "packages")
 "hpP" #'straight-pull-all
 "hpR" #'straight-rebuild-all
 "hpc" #'straight-check-all
 "hpf" #'straight-fetch-all
 "hpg" #'straight-get-recipe
 "hpp" #'straight-pull-package-and-deps
 "hpr" #'straight-rebuild-package
 "hpx" #'straight-prune-build

 "e"   '(:ignore t :wk "error")

 "p"   '(:ignore t :wk "project")
 "pb"  #'consult-project-buffer
 "pr"  #'xref-find-definitions
 "pf"  #'project-find-file
 "pp"  #'project-switch-project
 "p:"  #'project-shell-command
 "p&"  #'project-async-shell-command
 "p'"  #'project-eshell
 "p%"  #'project-query-replace-regexp

 "q"   '(:ignore t :wk "quit")
 "qK"  #'save-buffers-kill-emacs
 "qQ"  #'evil-quit-all-with-error-code
 "qq"  #'save-buffers-kill-terminal

 "t"   '(:ignore t :wk "toggle")

 "w"   #'evil-window-map

 "vU"  #'upcase-dwim
 "vu"  #'downcase-dwim

 "x"   '(:ignore t :wk "edit")
 "xa"  #'align-regexp)

(provide 'chidori-binding)
;;; chidori-binding.el ends here
