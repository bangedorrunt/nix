;;; core/autoload/eval.el -*- lexical-binding: t; -*-

;;
;; REPLs

(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/open-repl-other-window' and filled with the `:repl' setting.")

;;;###autoload
(defun set-repl-handler! (modes command &rest plist)
  "Defines a REPL for MODES.

MODES is either a single major mode symbol or a list of them. COMMAND is a
function that creates and returns the REPL buffer.

COMMAND can either be a function that takes no arguments, or an interactive
command that will be called interactively. COMMANDS must return either the repl
buffer or a function that takes no arguments and returns the repl buffer.

PLIST is a property list that map special attributes to this repl. These are
recognized:

  :persist BOOL
    If non-nil, this REPL won't be killed when its window is closed.
  :send-region FUNC
    A function that accepts a BEG and END, and sends the contents of the region
    to the REPL. Defaults to `+eval/send-region-to-repl'.
  :send-buffer FUNC
    A function of no arguments that sends the contents of the buffer to the REPL.
    Defaults to `+eval/region', which will run the :send-region specified function
    or `+eval/send-region-to-repl'."
  (declare (indent defun))
  (dolist (mode (ensure-list modes))
    (setf (alist-get mode +eval-repls)
          (cons command plist))))


;;
;; Evaluation

;;;###autoload
(defvar +eval-runners nil
  "Alist mapping major modes to interactive runner functions.")

;;;###autoload
(defun set-eval-handler! (modes command)
  "Define a code evaluator for major mode MODES with `quickrun'.

MODES can be list of major mode symbols, or a single one.

1. If MODE is a string and COMMAND is the string, MODE is a file regexp and
   COMMAND is a string key for an entry in `quickrun-file-alist'.
2. If MODE is not a string and COMMAND is a string, MODE is a major-mode symbol
   and COMMAND is a key (for `quickrun--language-alist'), and will be registered
   in `quickrun--major-mode-alist'.
3. If MODE is not a string and COMMAND is an alist, see `quickrun-add-command':
   (quickrun-add-command MODE COMMAND :mode MODE).
4. If MODE is not a string and COMMANd is a symbol, add it to
   `+eval-runners', which is used by `+eval/region'."
  (declare (indent defun))
  (dolist (mode (ensure-list modes))
    (cond ((symbolp command)
           (push (cons mode command) +eval-runners))
          ((stringp command)
           (after! quickrun
             (push (cons mode command)
                   (if (stringp mode)
                       quickrun-file-alist
                     quickrun--major-mode-alist))))
          ((listp command)
           (after! quickrun
             (quickrun-add-command
               (or (cdr (assq mode quickrun--major-mode-alist))
                   (string-remove-suffix "-mode" (symbol-name mode)))
               command :mode mode))))))



;;;;

;;;###autoload
(defun +eval-display-results-in-popup (output &optional _source-buffer)
  "Display OUTPUT in a popup buffer."
  (let ((output-buffer (get-buffer-create "*doom eval*"))
        (origin (selected-window)))
    (with-current-buffer output-buffer
      (setq-local scroll-margin 0)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (if (fboundp '+word-wrap-mode)
          (+word-wrap-mode +1)
        (visual-line-mode +1)))
    (when-let (win (display-buffer output-buffer))
      (fit-window-to-buffer
       win (/ (frame-height) 2)
       nil (/ (frame-width) 2)))
    (select-window origin)
    output-buffer))

;;;###autoload
(defun +eval-display-results-in-overlay (output &optional source-buffer)
  "Display OUTPUT in a floating overlay next to the cursor."
  (require 'eros)
  (let* ((this-command #'+eval/buffer-or-region)
         (prefix eros-eval-result-prefix)
         (lines (split-string output "\n"))
         (prefixlen (length prefix))
         (len (+ (apply #'max (mapcar #'length lines))
                 prefixlen))
         (col (- (current-column) (window-hscroll)))
         (next-line? (or (cdr lines)
                         (< (- (window-width)
                               (save-excursion (goto-char (point-at-eol))
                                               (- (current-column)
                                                  (window-hscroll))))
                            len)))
         (pad (if next-line?
                  (+ (window-hscroll) prefixlen)
                0))
         (where (if next-line?
                    (line-beginning-position 2)
                  (line-end-position)))
         eros-eval-result-prefix
         eros-overlays-use-font-lock)
    (with-current-buffer (or source-buffer (current-buffer))
      (eros--make-result-overlay
          (concat (make-string (max 0 (- pad prefixlen)) ?\s)
                  prefix
                  (string-join lines (concat "\n" (make-string pad ?\s))))
        :where where
        :duration eros-eval-result-duration))))

;;;###autoload
(defun +eval-display-results (output &optional source-buffer)
  "Display OUTPUT in an overlay or a popup buffer."
  (funcall (if (or current-prefix-arg
                   (with-temp-buffer
                     (insert output)
                     (or (>= (count-lines (point-min) (point-max))
                             +eval-popup-min-lines)
                         (>= (string-width
                              (buffer-substring (point-min)
                                                (save-excursion
                                                  (goto-char (point-min))
                                                  (line-end-position))))
                             (window-width))))
                   (not (require 'eros nil t)))
               #'+eval-display-results-in-popup
             #'+eval-display-results-in-overlay)
           output source-buffer)
  output)


;;
;;; Commands

(defvar quickrun-option-cmdkey)
;;;###autoload
(defun +eval/buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (let ((quickrun-option-cmdkey (bound-and-true-p quickrun-option-cmdkey)))
    (if (or (assq major-mode +eval-runners)
            (and (fboundp '+eval--ensure-in-repl-buffer)
                 (ignore-errors
                   (get-buffer-window (or (+eval--ensure-in-repl-buffer)
                                          t))))
            (and (require 'quickrun nil t)
                 (equal (setq
                         quickrun-option-cmdkey
                         (quickrun--command-key
                          (buffer-file-name (buffer-base-buffer))))
                        "emacs")
                 (alist-get 'emacs-lisp-mode +eval-runners)))
        (if-let ((buffer-handler (plist-get (cdr (alist-get major-mode +eval-repls)) :send-buffer)))
            (funcall buffer-handler)
          (+eval/region (point-min) (point-max)))
      (quickrun))))

;;;###autoload
(defun +eval/region (beg end)
  "Evaluate a region between BEG and END and display the output."
  (interactive "r")
  (let ((load-file-name buffer-file-name)
        (load-true-file-name
         (or buffer-file-truename
             (if buffer-file-name
                 (file-truename buffer-file-name)))))
    (cond ((and (fboundp '+eval--ensure-in-repl-buffer)
                (ignore-errors
                  (get-buffer-window (or (+eval--ensure-in-repl-buffer)
                                         t))))
           (funcall (or (plist-get (cdr (alist-get major-mode +eval-repls)) :send-region)
                        #'+eval/send-region-to-repl)
                    beg end))
          ((let ((runner
                  (or (alist-get major-mode +eval-runners)
                      (and (require 'quickrun nil t)
                           (equal (setq
                                   lang (quickrun--command-key
                                         (buffer-file-name (buffer-base-buffer))))
                                  "emacs")
                           (alist-get 'emacs-lisp-mode +eval-runners))))
                 lang)
             (if runner
                 (funcall runner beg end)
               (let ((quickrun-option-cmdkey lang))
                 (quickrun-region beg end))))))))

;;;###autoload
(defun +eval/line-or-region ()
  "Evaluate the current line or selected region."
  (interactive)
  (if (use-region-p)
      (call-interactively #'+eval/region)
    (+eval/region (line-beginning-position) (line-end-position))))

;;;###autoload
(defun +eval/buffer-or-region ()
  "Evaluate the region if it's active, otherwise evaluate the whole buffer.

If a REPL is open the code will be evaluated in it, otherwise a quickrun
runner will be used."
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'+eval/region
     #'+eval/buffer)))

;;;###autoload
(defun +eval/region-and-replace (beg end)
  "Evaluation a region between BEG and END, and replace it with the result."
  (interactive "r")
  (let (lang)
    (cond
     ((or (eq major-mode 'emacs-lisp-mode)
          (and (require 'quickrun nil t)
               (equal (setq
                       lang (quickrun--command-key
                             (buffer-file-name (buffer-base-buffer))))
                      "emacs")))
      (kill-region beg end)
      (condition-case nil
          (prin1 (eval (read (current-kill 0)))
                 (current-buffer))
        (error (message "Invalid expression")
               (insert (current-kill 0)))))
     ((let ((quickrun-option-cmdkey lang))
        (quickrun-replace-region beg end))))))

;; eval.el ends here
