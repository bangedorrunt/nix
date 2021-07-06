;;; early-init.el --- -*- lexical-binding: t -*-
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(setq comp-speed 3)

(setq load-prefer-newer t)

;; Every file opened and loaded by Emacs will run through this list
;; to check for a proper handler for the file, but during startup, it
;; won’t need any of them.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq site-run-file nil)

;; Prevent package.el from modifying this file
;; Disabled by straight.el by default
(setq package-enable-at-startup nil)

;; Change `packages.el' path
(setq package-user-dir (expand-file-name ".cache/elpa" user-emacs-directory))

;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; (if window-system
;;   (progn 
;;     (set-frame-font "Operator Mono SSm Lig-22" nil t)
;;     (set-face-font 'fixed-pitch-serif "Operator Mono SSm Lig")
;;     (defun set-bigger-spacing ()
;;       (setq-local default-text-properties '(line-spacing 0.25 line-height 1.50)))
;;     (add-hook 'text-mode-hook 'set-bigger-spacing)
;;     (add-hook 'prog-mode-hook 'set-bigger-spacing)))

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

(setq load-path (cons "~/dotfiles/pure-emacs" load-path))

;; Temporary fix for eln cache
;; Official gccemacs might not need this
(when (boundp 'comp-eln-load-path)
  (let ((eln-cache-dir (expand-file-name ".cache/eln-cache/"
                                         user-emacs-directory))
        (find-exec (executable-find "find")))
    (setcar comp-eln-load-path eln-cache-dir)
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete" "-or"
                    "-name" "*.eln.tmp" "-size" "0" "-delete"))))

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
