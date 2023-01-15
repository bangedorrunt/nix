;;; lisp/chidori-vterm.el -*- lexical-binding: t; -*-

(package! vterm :auto
  :when (display-graphic-p)
  :custom
  ;; (term-prompt-regexp "^[^#$%»>\\n]*[#$%»>] *")
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (vterm-max-scrollback 5000)
  ;; 5000 lines of scrollback, instead of 1000
  (vterm-kill-buffer-on-exit t)
  (vterm-toggle-fullscreen-p nil)
  :init
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :config
  (setq-hook! vterm-mode-hook
    confirm-kill-processes nil ;; don't prompt about dying processes when killing vterm
    hscroll-margin 0) ;; prevent premature horizontal scrolling
  (map!
   :map vterm-mode-map
   "C-h" nil
   "C-l" nil
   "C-j" nil
   "C-k" nil))

(package! multi-vterm :auto
  :when (display-graphic-p)
  :defer-incrementally t
  :custom (multi-vterm-dedicated-window-height-percent 30)
  :config
  (add-hook! 'vterm-mode-hook
    (setq-local evil-insert-state-cursor 'bar)
    (evil-insert-state))

  (map!
   "C-`" #'multi-vterm-dedicated-toggle)
  (map!
   :leader
   "`"   #'multi-vterm-dedicated-toggle
   "ts"  #'multi-vterm-dedicated-toggle
   "p`"  #'multi-vterm-project)
  (map!
   :map vterm-mode-map
   :localleader
   "c"   #'multi-vterm
   "n"   #'mutli-vterm-next
   "p"   #'multi-vterm-prev))

(package! sh-script :builtin
  :mode ("\\.\\(sh\\|bash\\|zsh\\|zsh-theme\\)\\'" . sh-mode)
  :config
  (defun indent-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph) (indent-region (region-beginning) (region-end)))))

(provide 'chidori-vterm)
