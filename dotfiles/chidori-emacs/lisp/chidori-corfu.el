;;; lisp/chidori-corfu.el --- Completion Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup completion packages.  Completion in this sense is more like
;; narrowing, allowing the user to find matches based on minimal
;; inputs and "complete" the commands, variables, etc from the
;; narrowed list of possible choices.

;;; Code:
(package! corfu (:files (:defaults "extensions/*.el"))
  :hook (doom-first-buffer . global-corfu-mode)
  :custom
  (corfu-excluded-modes '(erc-mode
                          circe-mode
                          help-mode
                          gud-mode
                          vterm-mode))
  (corfu-auto t)
  (corfu-separator #'noct-split-orderless-component)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-cycle t)
  (completion-cycle-threshold 1)
  (corfu-preview-current nil) ;; disable current candidate preview
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete)
  (corfu-max-width 80)
  (corfu-preselect-first nil)
  :config
  ;; So I can use corefu-separator in minibuffer
  (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer)

  (after! evil
    (advice-add 'corfu--setup :after #'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after #'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map))
  (when (featurep 'evil-collection)
    (setq evil-collection-corfu-key-themes '(default magic-return)))

  (defadvice! +corfu--org-return (orig)
    :around '+org/return
    (if (and corfu-mode
             (>= corfu--index 0))
        (corfu-insert)
      (funcall orig)))

  (unless (display-graphic-p)
    (corfu-terminal-mode))

  (map!
   :map corfu-map
   "C-SPC"   #'corfu-insert-separator
   "C-n"     #'corfu-next
   "C-p"     #'corfu-previous
   "C-e"     #'corfu-quit
   "M-m"     #'+corfu/move-to-minibuffer
   "C-x C-k" #'cape-dict
   "C-x s"   #'cape-ispell
   "C-x C-n" #'cape-keyword
   "C-x C-f" #'cape-file)
  )

(package! corfu-terminal
  (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after corfu
  :functions corfu-terminal-mode)

(package! corfu-doc-terminal
  :disabled t
  (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :after corfu
  :functions corfu-doc-terminal-mode)

;;;; Corfu
(package! corfu-popupinfo :local
  :disabled t
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (eldoc-add-command #'corfu-insert)
  (map!
   :map corfu-map
   "M-p" #'corfu-popupinfo-scroll-down
   "M-n" #'corfu-popupinfo-scroll-up
   "M-d" #'corfu-popupinfo-toggle))

(package! corfu-history :local
  :after corfu
  :hook (corfu-mode . +corfu--setup-hook-h)
  )

(package! corfu-quick :local
  :after corfu
  :general (corfu-map
            "M-q"  #'corfu-quick-complete
            "C-q"  #'corfu-quick-insert))

(package! corfu-echo :local
  :after corfu
  :hook (corfu-mode . corfu-echo-mode))

(package! corfu-info :local
  :after corfu)

(package! dabbrev :builtin
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(package! cape :auto
  :demand t
  :commands (cape-dabbrev
             cape-file
             cape-history
             cape-keyword
             cape-tex
             cape-sgml
             cape-rfc1345
             cape-abbrev
             cape-ispell
             cape-dict
             cape-symbol
             cape-line)
  :hook ((prog-mode eglot-managed-mode emacs-lisp-mode org-mode) . +corfu--load-capes)
  :init
  (add-hook! 'latex-mode-hook
    (defun +corfu--latex-set-capfs ()
      (add-to-list 'completion-at-point-functions #'cape-tex)))
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)


  ;; DEPRECATED >= emacs@29
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  ;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  ;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  :config
  ;; Enhances speed on large projects, for which many buffers may be open.
  (setq cape-dabbrev-check-other-buffers nil)
  (map! [remap dabbrev-expand] 'cape-dabbrev))

;;;; Snippet
(package! tempel (:host github :repo "minad/tempel")
  :after cape
  :init
  ;;;; NOTE we don't have to setup completion at point here, see `autoload/corfu'
  (map!
   :map tempel-map
   :i "C-n" #'tempel-next
   :i "C-p" #'tempel-previous))

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; HACK: fix snippet stealin TAB
;; Override :config default mapping by waiting for after corfu is loaded
(add-hook! 'doom-after-init-hook
  (map!
   :i [tab]   nil
   :i "TAB"   nil
   :i "C-SPC" #'completion-at-point
   :i "C-@"   #'completion-at-point))

(provide 'chidori-corfu)
;;; chidori-corfu.el ends here
