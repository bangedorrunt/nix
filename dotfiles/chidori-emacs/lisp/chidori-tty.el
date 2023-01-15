;;; lisp/chidori-tty.el -*- lexical-binding: t -*-

;;; Commentary:
;; macOS and Linux compatible with terminal

;; Code:

(unless (display-graphic-p)

  (package! tmux-pane :auto
    :after evil
    :hook (doom-first-input . tmux-pane-mode))

;;;; This doesn't work on my iMac
  ;; leave here for reserve only
  (package! clipetty (:host github :repo "spudlyo/clipetty")
    :disabled t
    :after evil
    :hook (doom-first-buffer . global-clipetty-mode)
    :config
    ;; Tmux integration
    (setq clipetty-tmux-ssh-tty "tmux show-environment SSH_TTY"))

  (package! xclip (:host github :repo "emacs-straight/xclip")
    :hook (doom-first-buffer . xclip-mode))

  ;; Some terminals offer two different cursors: a "visible" static cursor and a
  ;; "very visible" blinking one. By default, Emacs uses the very visible cursor
  ;; and will switch back to it when Emacs is started or resumed. A nil
  ;; `visible-cursor' prevents this.
  (setq visible-cursor nil)

  ;; Enable the mouse in terminal Emacs
  (add-hook 'tty-setup-hook #'xterm-mouse-mode))

(provide 'chidori-tty)
;;; chidori-tty ends here
