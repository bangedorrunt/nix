;;; core/autoload/popup.el -*- lexical-binding: t; -*-

;; https://web.archive.org/web/20160409014815/https://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
(defmacro noct-match-major-mode (mode)
  "Create a function that returns whether the current `major-mode' is MODE."
  (let ((name (intern (format "noct-match-%s" mode))))
    `(progn
       (defun ,name (buffer-or-name _action)
         (ignore-errors
           (let ((buffer (get-buffer buffer-or-name)))
             (eq ',mode (buffer-local-value 'major-mode buffer)))))
       #',name)))

(defun noct-display-and-select-buffer (func buffer alist)
  "Call FUNC with BUFFER and ALIST.
Select the window afterwards if possible. This is modified from
`shackle--display-buffer-reuse'. Additionally set the window to be fixed size."
  (let ((window (funcall func buffer alist)))
    (when (and window (window-live-p window))
      (select-window window t))
    ;; TODO this breaks slots; doesn't work for non-side windows
    ;; (with-current-buffer buffer
    ;;   (setq window-size-fixed t))
    window))

(defun noct-display-buffer-reuse-window (buffer alist)
  "Call `display-buffer-reuse-window' with BUFFER and ALIST.
Select the window afterwards if possible."
  (noct-display-and-select-buffer #'display-buffer-reuse-window buffer alist))

(defun noct-display-buffer-in-side-window (buffer alist)
  "Call `display-buffer-in-side-window' with BUFFER and ALIST.
Select the window afterwards if possible."
  (noct-display-and-select-buffer #'display-buffer-in-side-window buffer alist))

(defun noct-display-buffer-in-side-window-no-header (buffer alist)
  "`noct-display-buffer-in-side-window' but don't have a header line.
Having a header line in some buffers will cause text to be cut off at the
bottom (e.g. transient and frog menu)."
  (noct-display-buffer-in-side-window buffer alist)
  (setf (buffer-local-value 'header-line-format buffer) nil))

(defun noct-display-buffer-same-window (buffer alist)
  "Call `display-buffer-same-window' with BUFFER and ALIST.
Select the window afterwards if possible."
  (noct-display-and-select-buffer #'display-buffer-same-window buffer alist))

(defun shackle--split-some-window (frame alist)
  "Return a window if splitting any window was successful.
This function tries using the largest window on FRAME for
splitting, if all windows are the same size, the selected one is
taken, in case this fails, the least recently used window is used
for splitting.  ALIST is passed to `window--try-to-split-window'
internally."
  (or (window--try-to-split-window (get-largest-window frame t) alist)
      (window--try-to-split-window (get-lru-window frame t) alist)))

(defun shackle--display-buffer-popup-window (buffer alist)
  "Display BUFFER in a popped up window.
This is a stripped down version of `shackle--display-buffer-popup-window'.
ALIST is passed to `shackle--window-display-buffer' internally.
If PLIST contains the :other key with t as value, reuse the next
available window if possible."
  (let ((window (if (not (one-window-p))
                    (next-window nil 'nominibuf)
                  (shackle--split-some-window (selected-frame) alist))))
    (window--display-buffer buffer window 'window alist)))

;;;###autoload
(defun noct-display-buffer-creating-other-window (buffer alist)
  "Call `display-buffer-in-other-window' with BUFFER and ALIST.
If another window does not exist, create it. Select the window afterwards if
possible."
  (noct-display-and-select-buffer #'shackle--display-buffer-popup-window
                                  buffer alist))

;;;###autoload
(defmacro noct-handle-window (condition &rest body)
  "Display windows matching CONDITION with the settings in BODY."
  (declare (indent 1) (debug t))
  (let ((condition (if (and (symbolp condition)
                            (string-match "-mode$" (symbol-name condition)))
                       `(noct-match-major-mode ,condition)
                     condition)))
    `(cl-pushnew
      (list ,condition ,@body)
      display-buffer-alist
      :test 'equal)))

;;;###autoload
(defmacro noct-handle-popup (condition &optional slot)
  "Display popups matching CONDITION in a side window at the top.
When SLOT is non-nil, display popup buffers in that SLOT in the side window."
  `(noct-handle-window ,condition
     '(noct-display-buffer-reuse-window noct-display-buffer-in-side-window)
     '(side . top)
     '(slot . ,slot)
     '(window-height . 0.5)))

;;;###autoload
(defmacro noct-handle-popup-no-header (condition &optional slot)
  "Display popups matching CONDITION in a side window at the top.
Remove the header line. This handles some buffers where text would be cut off
when there is a header line. When SLOT is non-nil, display popup buffers in that
SLOT in the side window."
  `(noct-handle-window ,condition
     '(noct-display-buffer-reuse-window
       noct-display-buffer-in-side-window-no-header)
     '(side . top)
     '(slot . ,slot)
     '(window-height . 0.5)))

;;;###autoload
(defmacro noct-handle-popup-same-window (condition)
  "Display popups matching CONDITION in the current window."
  `(noct-handle-window ,condition
     '(noct-display-buffer-reuse-window noct-display-buffer-same-window)))

(defmacro noct-handle-popup-other-window (condition)
  "Display popups matching CONDITION in the other window.
Create another window if one doesn't exist"
  `(noct-handle-window ,condition
     '(noct-display-buffer-reuse-window
       noct-display-buffer-creating-other-window)))

;;;###autoload
(defmacro noct-handle-popup-other-window-no-select (condition)
  "Display popups matching CONDITION in the other window without selecting it.
Create another window if one doesn't exist"
  `(noct-handle-window ,condition
     'shackle--display-buffer-popup-window))

(defun noct-side-window-p ()
  "Return non-nil if the selected window is a side window."
  (window-parameter (selected-window) 'window-side))
;;; popup.el ends here
