;;; core/autoload/org/org-export.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +org-init-export-h ()
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t)

  (when (featurep 'markdown-mode)
    (add-to-list 'org-export-backends 'md))

  (package! ox-hugo :auto
    :after ox)

  (package! ox-pandoc :auto
    :when (executable-find "pandoc")
    :after ox
    :init
    (add-to-list 'org-export-backends 'pandoc)
    (setq org-pandoc-options
          '((standalone . t)
            (mathjax . t)
            (variable . "revealjs-url=https://revealjs.com"))))

  (defadvice! +org--dont-trigger-save-hooks-a (fn &rest args)
    "Exporting and tangling trigger save hooks; inadvertantly triggering
mutating hooks on exported output, like formatters."
    :around '(org-export-to-file org-babel-tangle)
    (let (before-save-hook after-save-hook)
      (apply fn args)))

  (defadvice! +org--fix-async-export-a (fn &rest args)
    :around '(org-export-to-file org-export-as)
    (let ((old-async-init-file org-export-async-init-file)
          (org-export-async-init-file (make-temp-file "doom-org-async-export")))
      (doom-file-write
       org-export-async-init-file
       `((setq org-export-async-debug ,(or org-export-async-debug debug-on-error)
               load-path ',load-path)
         (unwind-protect
             (let ((init-file ,old-async-init-file))
               (if init-file
                   (load init-file nil t)
                 (load ,early-init-file nil t)
                 (require 'doom-start)))
           (delete-file load-file-name))))
      (apply fn args))))

(defun +org--yank-html-buffer (buffer)
  (with-current-buffer buffer
    (require 'ox-clip)
    (cond ((or IS-WINDOWS IS-MAC)
           (shell-command-on-region
            (point-min)
            (point-max)
            (cond (IS-WINDOWS ox-clip-w32-cmd)
                  (IS-MAC     ox-clip-osx-cmd))))
          (IS-LINUX
           (let ((html (buffer-string)))
             (with-temp-file (make-temp-file "ox-clip-md" nil ".html")
               (insert html))
             (apply #'start-process "ox-clip" "*ox-clip*"
                    (split-string ox-clip-linux-cmd " ")))))))

;;
;;; Commands

;;;###autoload
(defun +org/export-to-clipboard (backend)
  "Exports the current buffer/selection to the clipboard.

Prompts for what BACKEND to use. See `org-export-backends' for options."
  (interactive
   (list (intern (completing-read "Export to: " (progn (require 'ox) org-export-backends)))))
  (require 'ox)
  (let* ((org-export-show-temporary-export-buffer nil)
         (buffer (org-export-to-buffer backend "*Formatted Copy*" nil nil t t)))
    (unwind-protect
        (with-current-buffer buffer
          (kill-new (buffer-string)))
      (kill-buffer buffer))))

;;;###autoload
(defun +org/export-to-clipboard-as-rich-text (beg end)
  "Export the current buffer to HTML then copies it to clipboard as rich text.

Supports org-mode, markdown-mode, and gfm-mode buffers. In any other mode,
htmlize is used (takes what you see in Emacs and converts it to html, text
properties and font-locking et all)."
  (interactive "r")
  (pcase major-mode
    ((or `markdown-mode `gfm-mode)
     (+org--yank-html-buffer (markdown)))
    (_
     ;; Omit after/before-string overlay properties in htmlized regions, so we
     ;; don't get fringe characters for things like flycheck or git-gutter.
     (letf! (defun htmlize-add-before-after-strings (_beg _end text) text)
       (ox-clip-formatted-copy beg end)))))



;;;;; prot-org.el extensions
;;;; org-export

(declare-function org-html-export-as-html "org")
(declare-function org-texinfo-export-to-info "org")

;;;###autoload
(defun prot-org-ox-html ()
  "Streamline HTML export."
  (interactive)
  (org-html-export-as-html nil nil nil t nil))

;;;###autoload
(defun prot-org-ox-texinfo ()
  "Streamline Info export."
  (interactive)
  (org-texinfo-export-to-info))

;; Doom naming convention
;;;###autoload
(defalias '+org/export-to-html #'prot-org-ox-html)
;;;###autoload
(defalias '+org/export-to-texinfo #'prot-org-ox-texinfo)

;;;; org-id

(declare-function org-id-add-location "org")
(declare-function org-with-point-at "org")
(declare-function org-entry-get "org")
(declare-function org-id-new "org")
(declare-function org-entry-put "org")

;; Copied from this article (with minor tweaks from my side):
;; <https://writequit.org/articles/emacs-org-mode-generate-ids.html>.
(defun prot-org--id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present already.
PREFIX will be passed through to `org-id-new'.  In any case, the
CUSTOM_ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (format "%s" (buffer-file-name (buffer-base-buffer))))
        id)))))

(declare-function org-map-entries "calendar")

;;;###autoload
(defun prot-org-id-headlines ()
  "Add missing CUSTOM_ID to all headlines in current file."
  (interactive)
  (org-map-entries
   (lambda () (prot-org--id-get (point) t))))

;; Doom naming convention
;;;###autoload
(defalias '+org/id-headlines #'prot-org-id-headlines)
