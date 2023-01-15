;;; core/autoload/doom/project.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-project-p (&optional project)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (doom-project-root project) t))

;;;; NOTE Doom's using projectile, but I prefer built in `project'
;;;###autoload
(defun doom-project-root (&optional project)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (if project
      (project-root project)
    (project-root (project-current t))))

;;;###autoload
(defun doom-root-override (project)
  "Find PROJECT's root by searching for a '.project.el' file.
If this file exists, it marks the project root. For convenient compatibility with Projectile, '.projectile' is also considered a project root marker."
  (let ((root (or (locate-dominating-file project ".project.el")
                  (locate-dominating-file project ".projectile")))
        (backend (ignore-errors (vc-responsible-backend project))))
    (when root (if (version<= emacs-version "28")
                   (cons 'vc root)
                 (list 'vc backend root)))))

;;;; NOTE use built in `project' function
;;;###autoload
(defalias 'doom-project-browse #'project-find-file)

;;;; https://git.sr.ht/~woolsweater/.emacs.d
;;;###autoload
(defun +project/find-file-other-tab (&optional arg)
  "Choose a file in the current project using completion and
show it in a new native tab.

With prefix ARG, open the file in its own new frame.

The file's base name is used as the candidate, with the
project-relative path as a decorating prefix."
  (interactive "P")
  (let* ((proj (project-current t))
         (proj-root (project-root proj))
         (files (project-files proj (list proj-root)))
         (candidates (+project-file-completion-candidates
                      files proj-root))
         (prompt (concat "􀎾 Jump to project file: "))
         (completion-extra-properties
          `(:affixation-function
            ,(apply-partially
              #'mapcar
              (lambda (candidate)
                (list candidate
                      (if-let ((dir
                                (get-text-property 0 'proj-path candidate)))
                          (propertize dir 'face 'dired-directory)
                        (propertize "(root)/" 'face 'dired-ignored))
                      "")))))
         ;; This makes the path retrieval possible; without it the property
         ;; would be stripped off the value returned from `completing-read'.
         (minibuffer-allow-text-properties t)
         (selection (completing-read prompt
                                     candidates
                                     nil
                                     'require-match))
         ;; Set tab behavior _after_ completion so that it doesn't affect the
         ;; completion buffer.
         (mac-frame-tabbing (not arg)))
    ;; Strip duplicate marker before trying to use the file name
    (when (get-text-property (1- (length selection)) 'proj-dup selection)
      (setq selection (substring selection 0 (1- (length selection)))))
    (let* ((proj-path (or (get-text-property 0 'proj-path selection) ""))
           (dir (expand-file-name proj-path proj-root)))
      (find-file-other-frame (expand-file-name selection dir))
      (set-window-dedicated-p nil t))))

;;;###autoload
(defsubst +project-file-completion-candidates (files root)
  "Transform FILES into completion candidates with project ROOT.

The completion value is the filename with no directory parts. The
project-relative path is attached as a text property under
`proj-path' so that it can be retrieved and used to reconstruct
the full path after selection."
  (let* ((seen (make-hash-table :size (length files) :test #'equal))
         (transform
          (lambda (file)
            (let* ((relative-file (file-relative-name file root))
                   (candidate (file-name-nondirectory relative-file))
                   (project-path (file-name-directory relative-file))
                   (count (gethash candidate seen 0)))
              (when (< 0 count)
                ;; Add an invisible, unmatchable character to de-duplicate the
                ;; candidate in the completion system. Inspired by Consult's
                ;; tofu technique.
                (setq candidate
                      (concat candidate
                              (propertize (char-to-string (+ #x200000 count))
                                          'invisible t 'proj-dup t))))
              (puthash candidate (1+ count) seen)
              (if-let (dir project-path)
                  (propertize candidate 'proj-path dir)
                candidate)))))
    (mapcar transform files)))

;;;###autoload
(defun +project/open-in-terminal (proj)
    (interactive (list
                  (or (project-current t)
                      (user-error "You must select a project"))))
    (let ((display-buffer-alist
           (cons '("\\*Async shell command\\*" . display-buffer-no-window)
                 display-buffer-alist)))
      (async-shell-command (concat "open -a Wezterm"
                                   " "
                                   (project-root proj)))))

;; project.el ends here
