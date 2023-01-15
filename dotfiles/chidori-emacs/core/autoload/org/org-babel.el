;;; core/autoload/org/org-babel.el -*- lexical-binding: t; -*-

(defvar +org-babel-native-async-langs '(python)
  "Languages that will use `ob-comint' instead of `ob-async' for `:async'.")

(defvar +org-babel-mode-alist
  '((c . C)
    (cpp . C)
    (C++ . C)
    (D . C)
    (elisp . emacs-lisp)
    (sh . shell)
    (bash . shell)
    (matlab . octave)
    (rust . rustic-babel)
    (amm . ammonite))
  "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, (fish . shell) will cause #+begin_src fish blocks to load
ob-shell.el when executed.")

(defvar +org-babel-load-functions ()
  "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

;;;###autoload
(defun +org-init-babel-h ()
  (setq org-src-preserve-indentation t)  ; use native major-mode indentation
  (setq org-src-tab-acts-natively t)     ; we do this ourselves
  ;; You don't need my permission (just be careful, okay?)
  (setq org-confirm-babel-evaluate nil)
  (setq org-link-elisp-confirm-function nil)
  ;; Show src buffer in popup, and don't monopolize the frame
  (setq org-src-window-setup 'other-window)
  ;; Our :lang common-lisp module uses sly, so...
  (setq org-babel-lisp-eval-fn #'sly-eval)

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; Don't process babel results asynchronously when exporting org, as they
  ;; won't likely complete in time, and will instead output an ob-async hash
  ;; instead of the wanted evaluation results.
  (after! ob
    (add-to-list 'org-babel-default-lob-header-args '(:sync)))

  (defadvice! +org-babel-disable-async-maybe-a (fn &optional orig-fn arg info params)
    "Use ob-comint where supported, disable async altogether where it isn't.

We have access to two async backends: ob-comint or ob-async, which have
different requirements. This advice tries to pick the best option between them,
falling back to synchronous execution otherwise. Without this advice, they die
with an error; terrible UX!

Note: ob-comint support will only kick in for languages listed in
`+org-babel-native-async-langs'.

Also adds support for a `:sync' parameter to override `:async'."
    :around #'ob-async-org-babel-execute-src-block
    (if (null orig-fn)
        (funcall fn orig-fn arg info params)
      (let* ((info (or info (org-babel-get-src-block-info)))
             (params (org-babel-merge-params (nth 2 info) params)))
        (if (or (assq :sync params)
                (not (assq :async params))
                (member (car info) ob-async-no-async-languages-alist)
                ;; ob-comint requires a :session, ob-async does not, so fall
                ;; back to ob-async if no :session is provided.
                (unless (member (alist-get :session params) '("none" nil))
                  (unless (memq (let* ((lang (nth 0 info))
                                       (lang (cond ((symbolp lang) lang)
                                                   ((stringp lang) (intern lang)))))
                                  (or (alist-get lang +org-babel-mode-alist)
                                      lang))
                                +org-babel-native-async-langs)
                    (message "Org babel: %s :session is incompatible with :async. Executing synchronously!"
                             (car info))
                    (sleep-for 0.2))
                  t))
            (funcall orig-fn arg info params)
          (funcall fn orig-fn arg info params)))))

  ;; HACK Fix #6061. Seems `org-babel-do-in-edit-buffer' has the side effect of
  ;;   deleting side windows. Should be reported upstream! This advice
  ;;   suppresses this behavior wherever it is known to be used.
  (defadvice! +org-fix-window-excursions-a (fn &rest args)
    "Suppress changes to the window config anywhere
`org-babel-do-in-edit-buffer' is used."
    :around #'evil-org-open-below
    :around #'evil-org-open-above
    :around #'org-indent-region
    :around #'org-indent-line
    (save-window-excursion (apply fn args)))

  (defadvice! +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
    "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
    :after #'org-return
    (when (and indent
               org-src-tab-acts-natively
               (org-in-src-block-p t))
      (save-window-excursion
        (org-babel-do-in-edit-buffer
         (call-interactively #'indent-for-tab-command)))))

  (after! ob-ditaa
    ;; TODO Should be fixed upstream
    (let ((default-directory (org-find-library-dir "org-contribdir")))
      (setq org-ditaa-jar-path     (expand-file-name "scripts/ditaa.jar")
            org-ditaa-eps-jar-path (expand-file-name "scripts/DitaaEps.jar")))))

;;;###autoload
(defun +org-init-babel-lazy-loader-h ()
  "Load babel libraries lazily when babel blocks are executed."
  (defun +org--babel-lazy-load (lang &optional async)
    (cl-check-type lang (or symbol null))
    (unless (cdr (assq lang org-babel-load-languages))
      (when async
        ;; ob-async has its own agenda for lazy loading packages (in the child
        ;; process), so we only need to make sure it's loaded.
        (require 'ob-async nil t))
      (prog1 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                 (require (intern (format "ob-%s" lang)) nil t)
                 (require lang nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))))

  (defadvice! +org--export-lazy-load-library-h (&optional element)
    "Lazy load a babel package when a block is executed during exporting."
    :before #'org-babel-exp-src-block
    (+org--babel-lazy-load-library-a (org-babel-get-src-block-info nil element)))

  (defadvice! +org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    :before #'org-src--get-lang-mode
    (or (cdr (assoc lang org-src-lang-modes))
        (+org--babel-lazy-load lang)))

  ;; This also works for tangling
  (defadvice! +org--babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    :after-while #'org-babel-confirm-evaluate
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (+org--babel-lazy-load
       lang (and (not (assq :sync (nth 2 info)))
                 (assq :async (nth 2 info))))
      t))

  (advice-add #'org-babel-do-load-languages :override #'ignore))


;;;###autoload
(defun +org-eval-handler (beg end)
  "TODO"
  (save-excursion
    (if (not (cl-loop for pos in (list beg (point) end)
                      if (save-excursion (goto-char pos) (org-in-src-block-p t))
                      return (goto-char pos)))
        (message "Nothing to evaluate at point")
      (let* ((element (org-element-at-point))
             (block-beg (save-excursion
                          (goto-char (org-babel-where-is-src-block-head element))
                          (line-beginning-position 2)))
             (block-end (save-excursion
                          (goto-char (org-element-property :end element))
                          (skip-chars-backward " \t\n")
                          (line-beginning-position)))
             (beg (if beg (max beg block-beg) block-beg))
             (end (if end (min end block-end) block-end))
             (lang (or (org-eldoc-get-src-lang)
                       (user-error "No lang specified for this src block"))))
        (cond ((and (string-prefix-p "jupyter-" lang)
                    (require 'jupyter nil t))
               (jupyter-eval-region beg end))
              ((let ((major-mode (org-src-get-lang-mode lang)))
                 (+eval/region beg end))))))))


;;;###autoload
(defun +org-lookup-definition-handler (identifier)
  "TODO"
  (when (org-in-src-block-p t)
    (let ((mode (org-src-get-lang-mode
                 (or (org-eldoc-get-src-lang)
                     (user-error "No lang specified for this src block")))))
      (cond ((and (eq mode 'emacs-lisp-mode)
                  (fboundp '+emacs-lisp-lookup-definition))
             (+emacs-lisp-lookup-definition identifier)
             'deferred)
            ((user-error "Definition lookup in SRC blocks isn't supported yet"))))))

;;;###autoload
(defun +org-lookup-references-handler (identifier)
  "TODO"
  (when (org-in-src-block-p t)
    (user-error "References lookup in SRC blocks isn't supported yet")))

;;;###autoload
(defun +org-lookup-documentation-handler (identifier)
  "TODO"
  (when (org-in-src-block-p t)
    (let ((mode (org-src-get-lang-mode
                 (or (org-eldoc-get-src-lang)
                     (user-error "No lang specified for this src block"))))
          (info (org-babel-get-src-block-info t)))
      (cond ((string-prefix-p "jupyter-" (car info))
             (and (require 'jupyter nil t)
                  (call-interactively #'jupyter-inspect-at-point)
                  (display-buffer (help-buffer))
                  'deferred))
            ((and (eq mode 'emacs-lisp-mode)
                  (fboundp '+emacs-lisp-lookup-documentation))
             (+emacs-lisp-lookup-documentation identifier)
             'deferred)
            ((user-error "Documentation lookup in SRC blocks isn't supported yet"))))))


;;
;;; Commands

;;;###autoload
(defun +org/remove-result-blocks (remove-all)
  "Remove all result blocks located after current point."
  (interactive "P")
  (let ((pos (point)))
    (org-babel-map-src-blocks nil
      (if (or remove-all (< pos end-block))
          (org-babel-remove-result)))))


;;
;;; Hooks

;;;###autoload
(defun +org-clear-babel-results-h ()
  "Remove the results block for the org babel block at point."
  (when (and (org-in-src-block-p t)
             (org-babel-where-is-src-block-result))
    (org-babel-remove-result)
    t))
