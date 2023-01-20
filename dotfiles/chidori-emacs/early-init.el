;;; early-init.el -*- lexical-binding: t; -*-

;; Use native --no-titlebar
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Use native transparent titlebar
;; (when (memq window-system '(mac ns))
;;   (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
;;   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; (setq inhibit-compacting-font-caches t)

;; Disable useless UI features by default.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))


;; Avoid white screen when startup
;; NOTE modus-theme v4 is built-in since emacs@30
(load-theme 'modus-vivendi :no-confirm)

;; Maximize the Emacs frame on startup
(push '(fullscreen . maximized) default-frame-alist)

;; Always use utf-8 for everything, I'll change it on the fly if I need something else for some
;; reason.
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (customize-set-variable 'native-comp-async-report-warnings-errors 'silent)
  ;; Make native compilation happens asynchronously
  (customize-set-variable 'native-comp-deferred-compilation t)
  (customize-set-variable 'native-comp-speed 2)
  (customize-set-variable 'inhibit-automatic-native-compilation t))

(unless noninteractive
  (customize-set-variable 'byte-compile-warnings nil))

;; No littering
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Prevent stale Elisp bytecode file from shadowing more up-to-date source
;; files.
(setq load-prefer-newer t)


;; Define load-file directories
(defvar chidori-core-dir (expand-file-name "core/" user-emacs-directory))
(defvar chidori-autoload-dir (expand-file-name "core/autoload/" user-emacs-directory))
(defvar chidori-doom-dir (expand-file-name "core/autoload/doom/" user-emacs-directory))
(defvar chidori-org-dir (expand-file-name "core/autoload/org/" user-emacs-directory))
(defvar chidori-lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(defvar chidori-site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))
(defvar chidori-themes-dir (expand-file-name "themes/" user-emacs-directory))
(defvar chidori-etc-dir (expand-file-name "etc/" user-emacs-directory))
(defvar chidori-var-dir (expand-file-name "var/" user-emacs-directory))
(defvar chidori-cache-dir (expand-file-name "cache/" user-emacs-directory))

;; Make sure dirs are created
(defsubst make-dir-if-not-exists (dir)
  (unless (file-exists-p dir)
    (make-directory dir)))
(mapc 'make-dir-if-not-exists `(,chidori-etc-dir ,chidori-var-dir ,chidori-cache-dir))
;; Actually add them all to the load-path.
(defsubst add-to-load-path-if-exists (dir)
  (when (file-exists-p dir) (add-to-list 'load-path dir)))
(mapc 'add-to-load-path-if-exists
      `(,chidori-lisp-dir ,chidori-site-lisp-dir))

;; Enable the auto-compile package.
;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

;; We use straight not package.el for all package loading.
;; So we don't need package.el loaded at startup (or at all).
(customize-set-variable 'package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Do not allow loading from the package cache (same reason).
(customize-set-variable 'package-quickstart nil)

;; Local Variables:
;; no-native-compile: t
;; End:
