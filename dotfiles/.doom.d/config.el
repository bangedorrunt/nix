;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(defvar +babygau-projects '(
                            "~/workspace"
                            "~/workspace/notetoself"
                            ))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thanh Dung TRUONG"
      user-mail-address "braden.truong@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Operator Mono SSm Lig" :size 22 :weight 'light ))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-spectrum)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Set line spacing
;; Ref: https://github.com/syl20bnr/spacemacs/issues/10502
(defun set-bigger-spacing ()
  (setq-local default-text-properties '(line-spacing 0.25 line-height 1.25)))
(add-hook 'text-mode-hook 'set-bigger-spacing)
(add-hook 'prog-mode-hook 'set-bigger-spacing)

;; Custom theme
;; emacs-doom used transparent background color in terminal
;; (custom-set-faces!
;;         '(default :background "#17181c"))

(custom-set-faces!
        '(default :background "#17181c")
        '(markdown-code-face :background nil)
        '(markdown-blockquote-face :background nil)
        '(org-hide :background nil :foreground nil)
        '(org-block :background nil)
        '(org-level-1 :background nil)
        '(org-level-2 :background nil)
        '(org-level-3 :background nil)
        '(org-level-4 :background nil)
        '(org-level-5 :background nil)
        '(org-level-6 :background nil)
        '(org-level-7 :background nil)
        '(org-level-8 :background nil)
        '(solaire-default-face :background "#17181c")
        '(solaire-minibuffer-face :background "#17181c")
        '(solaire-org-hide-face :background "#17181c"))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Disable solaire-mode first
;; (after! solaire-mode
;;   (solaire-global-mode -1))

;; Disable tildes
(fringe-mode 0)

;;;; Which key settings
(setq which-key-idle-delay 0.3)

(after! projectile
  (dolist (project +babygau-projects)
    (projectile-add-known-project project)))
