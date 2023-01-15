;;; lisp/chidori-pdf.el -*- lexical-binding: t; -*-

;; Commentary
;; Configuration for the pdf-tools package to read pdf files

;;; Code:
(package! pdf-tools (:host github :repo "vedang/pdf-tools")
  :mode (("\\.pdf$" . pdf-view-mode))
  :commands (pdf-view-mode)
  :init
  ;; initialise
  (pdf-loader-install :no-query)
  (map!
   :map pdf-view-mode-map
   ;; Navigation
   "j"  #'pdf-view-next-line-or-next-page
   "k"  #'pdf-view-previous-line-or-previous-page
   "l"  #'pdf-view-next-page-command
   "h"  #'pdf-view-previous-page-command
   "g"  #'pdf-view-first-page
   "G"  #'pdf-view-last-page
   "t"  #'pdf-view-goto-page
   "l"  #'pdf-view-goto-label
   ;; Search
   "/"  #'isearch-forward
   "?"  #'isearch-backward
   ;; Action
   "-"  #'pdf-view-shrink
   "+"  #'pdf-view-enlarge
   "="  #'pdf-view-fit-page-to-window
   "r"  #'pdf-view-revert-buffer
   "o"  #'pdf-links-action-perform
   "O"  #'pdf-outline
   "!"  #'+pdf:no-filter)
  :config
  ;; HiDPI
  (setq pdf-view-use-scaling t)

  (defun +pdf:no-filter ()
    "View pdf without colour filter."
    (interactive)
    (pdf-view-midnight-minor-mode -1))

  (defun +pdf:midnight-mode ()
    "View pdf with colour filter."
    (interactive)
    (pdf-view-midnight-minor-mode))

  (defun +pdf:color-theme ()
    (if (eq active-theme 'light-theme)
        (+pdf:no-filter)
      (+pdf:midnight-mode)))

  ;; tex hook
  ;; see https://github.com/politza/pdf-tools#auto-revert
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; other hooks
  (add-hook! 'pdf-view-mode-hook
    (+pdf:color-theme)
    (blink-cursor-mode -1)
    (linum-mode -1)
    (line-number-mode -1)
    (column-number-mode -1)
    (auto-revert-mode -1)))

(provide 'chidori-pdf)
;;; chidori-pdf.el ends here
