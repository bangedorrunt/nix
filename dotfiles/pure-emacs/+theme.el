;;; +theme.el -*- lexical-binding: t; -*-

;;;; THEME
;; I'm obssessed with beatiful themes

;; FlucUI Theme
;; (use-package flucui-themes
;;   :init (flucui-themes-load-style 'dark))

;; DOOM Theme
(use-package doom-themes
  :init (load-theme 'doom-nord-light t)
  :custom-face
  ;; (default ((t (:background "#191b22"))))
  ;; (vertical-border ((t (:background "#191b22"))))
  (line-number ((t (:background "#dfe2e7"))))
  (ivy-current-match ((t (:background "#dfe2e7" :foreground "#18a57e"))))
  (ivy-highlight-face ((t (:background nil :foreground "#18a57e"))))
  (ivy-minibuffer-match-face-1 ((t (:background nil :foreground "#d65e7e"))))
  (ivy-minibuffer-match-face-2 ((t (:background nil))))
  (ivy-minibuffer-match-face-3 ((t (:background nil))))
  (ivy-minibuffer-match-face-4 ((t (:background nil))))
  (ivy-minibuffer-match-highlight ((t (:background nil))))
  (ivy-confirm-face ((t (:background nil))))
  (ivy-match-required-face ((t (:background nil))))
  (ivy-confirm-face ((t (:background nil))))
  (ivy-yanked-word ((t (:background nil))))
  (ivy-cursor ((t (:background nil))))
  (ivy-subdir ((t (:background nil))))
  (isearch ((t (:background nil :slant italic :weight bold))))
  (isearch-fail ((t (:background nil :slant italic :weight bold))))
  (isearch-group-even ((t (:background nil :slant italic :weight bold))))
  (isearch-group-odd ((t (:background nil :slant italic :weight bold))))
  (highlight ((t (:background nil :slant italic :weight bold))))
  (lazy-highlight ((t (:background nil :slant italic :weight bold))))
  (evil-search-forward ((t (:background nil :slant italic :weight bold))))
  (evil-ex-search ((t (:background nil :slant italic :weight bold))))
  (evil-ex-lazy-highlight ((t (:background nil :foreground "#d65e7e" :slant italic :weight bold))))
  (evil-ex-substitute-matches ((t (:background nil :foreground "#d65e7e" :slant italic :weight bold))))
  (evil-ex-substitute-replacement ((t (:background nil :foreground "#18a57e" :slant italic :weight bold))))
  (doom-modeline-debug-visual ((t (:background nil))))
  (next-error  ((t (:background nil :foreground "#d65e7e"))))
  (button  ((t (:background nil)))))


(use-package doom-modeline
  :init
    ;; Display real file name
    (setq find-file-visit-truename t)
    (doom-modeline-init))


;; Built-in Theme
;; (load-theme 'dichromacy)


;; (fringe-mode 0)
;; (setq linum-format "%4d \u2502 ")

;; element-dark: #15191e
;; iceberg-light: #e8e9ec
;; (custom-set-faces
;;   '(default ((t (:background "#e8e9ec"))))
;;   '(vertical-border ((t (:background "#e8e9ec"))))
;;   '(line-number ((t (:background "#dfe2e7"))))
;;   '(ivy-current-match ((t (:background nil :foreground "#18a57e"))))
;;   '(ivy-highlight-face ((t (:background nil :foreground "#18a57e"))))
;;   '(ivy-minibuffer-match-face-1 ((t (:background nil :foreground "#d65e7e"))))
;;   '(ivy-minibuffer-match-face-2 ((t (:background nil))))
;;   '(ivy-minibuffer-match-face-3 ((t (:background nil))))
;;   '(ivy-minibuffer-match-face-4 ((t (:background nil))))
;;   '(ivy-minibuffer-match-highlight ((t (:background nil))))
;;   '(ivy-confirm-face ((t (:background nil))))
;;   '(ivy-match-required-face ((t (:background nil))))
;;   '(ivy-confirm-face ((t (:background nil))))
;;   '(ivy-yanked-word ((t (:background nil))))
;;   '(ivy-cursor ((t (:background nil))))
;;   '(ivy-subdir ((t (:background nil))))
;;   '(isearch ((t (:background nil :slant italic :weight bold))))
;;   '(isearch-fail ((t (:background nil :slant italic :weight bold))))
;;   '(isearch-group-even ((t (:background nil :slant italic :weight bold))))
;;   '(isearch-group-odd ((t (:background nil :slant italic :weight bold))))
;;   '(lazy-highlight ((t (:background nil :slant italic :weight bold))))
;;   '(evil-search-forward ((t (:background nil :slant italic :weight bold))))
;;   '(evil-ex-search ((t (:background nil :slant italic :weight bold))))
;;   '(evil-ex-lazy-highlight ((t (:background nil :foreground "#d65e7e" :slant italic :weight bold))))
;;   '(evil-ex-substitute-matches ((t (:background nil :foreground "#d65e7e" :slant italic :weight bold))))
;;   '(evil-ex-substitute-replacement ((t (:background nil :foreground "#18a57e" :slant italic :weight bold))))
;;   '(doom-modeline-debug-visual ((t (:background nil))))
;;   '(next-error  ((t (:background nil :foreground "#d65e7e"))))
;;   '(button  ((t (:background nil)))))
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
(set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)

(use-package treemacs
  :defer t
  :config
  (setq treemacs-collapse-dirs               (if treemacs-python-executable 3 0)
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
    (treemacs-git-mode 'simple))))

(use-package treemacs-projectile
  :after (treemacs projectile))

(add-hook! prog-mode '(display-line-numbers-mode))
(setq linum-format "%d ")
(set-display-table-slot standard-display-table 5 ?│)

(provide '+theme)
;;; +theme.el ends here
