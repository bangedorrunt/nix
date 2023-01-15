;;; core/autoload/edit.el -*- lexical-binding: t; -*-

;; https://sachachua.com/blog/2006/09/emacs-changing-the-font-size-on-the-fly/
;;;###autoload
(defun +editor/increase-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height (ceiling (* 1.10 (face-attribute 'default :height)))))
;;;###autoload
(defun +editor/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height (floor (* 0.9 (face-attribute 'default :height)))))

;;;###autoload
(defun +editor/smooth-scroll-quarter-page-down ()
  "Smooth scroll down"
  (interactive)
  (let ((half-height (/ (window-height) 4)))
    (unless (pos-visible-in-window-p (point-max))
      (pixel-scroll-precision-interpolate (* 5 (- half-height))))
    (pixel-scroll-precision-interpolate (* 5 (- half-height)))))

;;;###autoload
(defun +editor/smooth-scroll-quarter-page-up ()
  "Smooth scroll up"
  (interactive)
  (let ((half-height (/ (window-height) 4)))
    (unless (pos-visible-in-window-p (point-min))
      (pixel-scroll-precision-interpolate (* 5 half-height)))
    (pixel-scroll-precision-interpolate (* 5 half-height))))

;; edit.el ends here
