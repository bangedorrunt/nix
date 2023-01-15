;;; core/lisp/denote.el -*- lexical-binding: t; -*-

(package! denote
  (:type git :host sourcehut :repo "protesilaos/denote")
  :after org
  :hook (doom-first-buffer . denote-modules-global-mode)
  :config
  (setq denote-directory (expand-file-name "~/workspace/notetoself/"))
  (setq denote-known-keywords '("emacs" "nvim" "elisp" "macOS"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  ;; (setq denote-file-type 'text) ; Org is the default, set others here like I do
  (setq denote-excluded-directories-regexp nil)

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more disciplined
  ;; workflow.
  (setq denote-allow-multi-word-keywords nil)

  (setq denote-date-format nil) ; read its doc string

  ;; By default, we fontify backlinks in their bespoke buffer.
  (setq denote-link-fontify-backlinks t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files you want to buttonise
  ;; existing buttons upon visiting the file (Org renders links as
  ;; buttons right away).
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; We use different ways to specify a path for demo purposes.
  ;; (setq
  ;;  denote-dired-directories
  ;;  (list denote-directory
  ;;        (thread-last denote-directory (expand-file-name "attachments"))
  ;;        ))

  ;; Generic (great if you rename files Denote-style in lots of places):
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Here is a custom, user-level command from one of the examples we
  ;; show in this manual.  We define it here and add it to a key binding
  ;; below.  The manual: <https://protesilaos.com/emacs/denote>.
  (defun prot/denote-journal ()
    "Create an entry tagged 'journal', while prompting for a title."
    (interactive)
    (denote
     (denote--title-prompt)
     '("journal")))

  ;; Create a Denote template
  (after! org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (setq org-capture-templates
          (doct-add-to org-capture-templates
                       '("Denote" :keys "N"
                         :type plain
                         :icon ("sticky-note-o" :set "faicon" :color "lblue")
                         :file denote-last-path
                         :template denote-org-capture
                         :no-save t
                         :immediate-finish nil
                         :kill-buffer t
                         :jump-to-captured t)
                       'append)))
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (map!
   :leader
   "n"  '(:ignore t :wk "denote")
   "nj" #'prot/denote-journal
   "nn" #'denote
   "nN" #'denote-type
   "nd" #'denote-date
   "ns" #'denote-subdirectory)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.

  (map!
   :leader
   "ni"  #'denote-link ; "insert" mnemonic
   "nI"  #'denote-link-add-links
   "nb"  #'denote-link-backlinks
   "nf"  '(:ignore t :wk "find note")
   "nff" #'denote-link-find-file
   "nfb" #'denote-link-find-backlink)
  ;; Note that `denote-rename-file' can work from any context, not
  ;; just Dired buffers.  That is why we bind it here to the
  ;; `global-map'.
  ;;
  ;; Also see `denote-rename-file-using-front-matter' further below.
  (map!
   :leader
   "nr" #'denote-rename-file)

  ;; Key bindings specifically for Dired.
  (map!
   :map dired-mode-map
   "C-c C-d C-i" #'denote-link-dired-marked-notes
   "C-c C-d C-r" #'denote-dired-rename-marked-files))

(package! consult-notes :auto
  :hook (denote-modules-mode . consult-notes-denote-mode)
  :init
  (setq consult-notes-use-rg t)
  (setq consult-notes-max-relative-age (* 60 60 24 7))
  :config
  (map!
   :leader
   "n." #'consult-notes
   "n;" #'consult-notes-search-in-all-notes))



(provide 'chidori-denote)
;;; chidori-denote.el ends here
