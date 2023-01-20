;;; core/autoload/orderless.el -*- lexical-binding: t; -*-

;;;;; noct's custom dispatcher
;;;; SEE: https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/awaken.org#orderless

;;;###autoload
(defvar noct-orderless-separator "[ &]")

;; Necessary at the time of writing for index-based rules to work
;;;###autoload
(defun noct-split-orderless-component (str)
  (let ((components (split-string str noct-orderless-separator)))
    (when (remove "" components)
      components)))

;; TODO ideally should immediately refresh;
;; right now need to type more to get new results
;;;###autoload
(defun noct-toggle-orderless-separator ()
  "Toggle the orderless separator."
  (interactive)
  (let ((normal-separator "[ &]")
        (allow-spaces-separator "&"))
    (if (equal noct-orderless-separator normal-separator)
        (setq noct-orderless-separator allow-spaces-separator)
      (setq noct-orderless-separator normal-separator))
    ;; TODO may not be necessary
    (setq orderless-component-separator
          #'noct-split-orderless-component))
  (message "Toggled orderless separator"))

;; Style dispatchers
;;;###autoload
(defconst noct-regexp-to-styles-alist
  `((,(rx (or (and bol "r;")
              (and bol "regexp;")))
     orderless-regexp)
    (,(rx (or (and bol "f;")
              (and ";f" eol)))
     orderless-flex))
  "Alist of regexp to list of styles to use for an orderless component.")

;;;###autoload
(cl-defun noct-orderless-specified-styles (pattern)
  "Return a list of styles and a PATTERN without the style specifier."
  (dolist (style-specifier noct-regexp-to-styles-alist)
    (let ((re (car style-specifier))
          (styles (cdr style-specifier)))
      (when (string-match re pattern)
        (cl-return-from noct-orderless-specified-styles
          (cons styles
                (let ((start (match-beginning 0))
                      (end (match-end 0)))
                  (concat (substring pattern 0 start)
                          (substring pattern end)))))))))

;;;###autoload
(defun noct-orderless-style-dispatcher (pattern &rest _)
  "Set style for PATTERN if it matches `noct-regexp-to-styles-alist'."
  (noct-orderless-specified-styles pattern))

;;;###autoload
(defconst noct-word-dispatch-pattern (rx ",")
  "Pattern to mark a full word starting/ending with the surrounding letters.")

;;;###autoload
(defun noct-orderless-word-dispatcher (pattern &rest _)
  "Replace `noct-word-dispatch-pattern' with regexp to pass a full word.)
The character before, if there is one, is the first character in the word. The
character after, if there is one, marks the final character in the word. For
example, if `noct-dispatch-pattern' is \"..\" then \"o..s\" would match
\"orderless\"."
  (pcase-let ((`(,styles . ,styless-pattern)
               (noct-orderless-specified-styles pattern)))
    (setq pattern (or styless-pattern pattern))
    (when (and (or (not styles) (memq 'orderless-regexp styles))
               (string-match noct-word-dispatch-pattern pattern))
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (left-anchor (unless (= start 0)
                            (substring pattern (1- start) start)))
             (right-anchor (unless (= end (length pattern))
                             (substring pattern end (1+ end)))))
        (cons 'orderless-regexp
              (concat
               ;; "regexp;"
               (if left-anchor
                   (substring pattern 0 (1- start))
                 "")
               (rx word-start)
               (or left-anchor "")
               (rx (0+ word))
               (or right-anchor "")
               (rx word-end)
               (if right-anchor
                   (substring pattern (1+ end))
                 "")))))))

;; https://github.com/oantolin/emacs-config/blob/da6b98a3343d68c02af7c05153f50cc194d46888/disabled.el#L46
;;;###autoload
(defun oantolin-not-containing-dispatcher (literal _index &rest _)
  (when (string-prefix-p "!" literal)
    (cons
     'orderless-regexp
     (rx-to-string
      `(seq
        (group string-start)
        (zero-or-more
         (or ,@(cl-loop for i from 1 below (length literal)
                        collect `(seq ,(substring literal 1 i)
                                      (or (not ,(aref literal i))
                                          string-end)))))
        string-end)))))


;;;;; prot's custom dispatcher
;;;;

;;;; Style dispatchers
;;;###autoload
(defun prot-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

;;;###autoload
(defun prot-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

;;;###autoload
(defun prot-orderless-flex-dispatcher (pattern _index _total)
  "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

;;;; Initialisms

;; All of the following are a copy of code that was removed from
;; orderless.el.  I was using it, so I want to keep it, at least until
;; some new version is provided upstream.

;;;###autoload
(defun orderless--strict-*-initialism (component &optional anchored)
  "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
  (orderless--separated-by
   '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
   (cl-loop for char across component collect `(seq word-start ,char))
   (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
   (when (eq anchored 'both)
     '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

;;;###autoload
(defun orderless-strict-initialism (component)
  "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
  (orderless--strict-*-initialism component))

;;;###autoload
(defun orderless-strict-leading-initialism (component)
  "Match a COMPONENT as a strict initialism, anchored at start.
See `orderless-strict-initialism'.  Additionally require that the
first initial appear in the first word of the candidate."
  (orderless--strict-*-initialism component 'start))

;;;###autoload
(defun orderless-strict-full-initialism (component)
  "Match a COMPONENT as a strict initialism, anchored at both ends.
See `orderless-strict-initialism'.  Additionally require that the
first and last initials appear in the first and last words of the
candidate, respectively."
  (orderless--strict-*-initialism component 'both))

;;; orderless.el ends here
