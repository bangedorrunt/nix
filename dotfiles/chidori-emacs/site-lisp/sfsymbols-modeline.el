;;; sfsymbols-modeline.el --- Use SF Symbols icons in Doom modeline ;; -*- lexical-binding: t; -*-


;;; Commentary:
;;;
;;; This is just hacks, really: using advice to intercept `doom-modeline''s
;;; internal requests to itself for icons and return SF Symbols alternatives.
;;; This requires that the default fontset has an SF font registered for the
;;; PUA range where the SF Symbols icons are located.

;;; Code:

(defconst sfsymbols-modeline--preferred-font "SF Compact Text"
  "The font to use for the SF Symbols in the modeline.")

(defun sfsymbols-modeline-buffer-file-state-icon (icon _unicode _text face)
  "Override the `doom-modeline' file state icon.

This covers state like read-only, dirty buffer, and narrowing."
  (let ((parent-face (or (plist-get face :inherit) face)))
    (propertize
     (pcase icon
       ("lock" "􀈍")  ;; pencil.slash
       ("save" "􀈌")  ;; pencil.circle.fill
       ("do_not_disturb_alt" "􀬔")  ;; questionmark.folder
       ("vertical_align_center" "􀐷")  ;; rectangle.compress.vertical
       (_ (progn
            (user-error "Unknown buffer state icon name '%s'" icon)
            " ")))
     'face
     `(:inherit ,parent-face
       :family ,sfsymbols-modeline--preferred-font
       :height 1.2
       :weight extra-light))))

(defun sfsymbols-modeline-lsp-icon (_text face)
  "Override the `doom-modeline' LSP server icon."
  ;; Various other possibilites to try?
  ;; - 􀖀 antenna.radiowaves.left.and.right
  ;; - 􀨤 externaldrive.connected.to.line.below
  ;; - 􀥭 puzzlepiece.extension
  ;; - 􀳇 cone
  ;; - 􀪔 radio
  ;; - 􀓎 hare
  ;; - 􀯐 brain
  ;; - 􀬛 phone.connection
  ;; - 􀍾 speedometer
  ;; - 􀎐 tuningfork
  ;; - 􀴌 torus
  ;; - 􀡷 powerplug
  ;; - 􀥲 leaf
  ;; - 􀾕 point.filled.topleft.down.curvedto.point.bottomright.up (or similar)
  ;; - 􀬗 gyroscope
  ;; - 􀬚 atom
  ;; - 􀡓 cylinder.split.1x2
  ;; - 􀑃 waveform.path
  ;; - 􀛭 lightbulb
  ;; or just something arbitrary like
  ;; - 􀷙 circle.hexagongrid.circle
  (propertize "􀤏"  ;; scroll
              'face
              `(:inherit ,face
                :family ,sfsymbols-modeline--preferred-font
                :height 1.0
                :weight extra-light)))

(defun sfsymbols-modeline-vcs-icon (icon &optional _unicode _text face _voffset)
  "Override the `doom-modeline' version control state icon."
  (propertize
   (pcase icon
     ("git-compare" "􀣗")  ;; doc.badge.plus
     ("git-merge" "􀖄")  ;; arrow.triangle.merge
     ("arrow-down" "􀅀") ;; arrow.down.to.line
     ("alert" "􀢤")  ;; exclamationmark.arrow.triangle.2.circlepath
     ("git-branch" "􀙠")  ;; arrow.triangle.branch
     (_ (progn
          (user-error "Unknown vcs icon name '%s'" icon)

          "􀙠")))  ;; Fallback to arrow.triangle.branch
   'face
   `(:inherit ,face
     :family ,sfsymbols-modeline--preferred-font
     :height 1.0
     :weight extra-light)))

(defun sfsymbols-modeline-checker-icon (icon _unicode _text face)
  "Override the `doom-modeline' Flycheck/Flymake state icon."
  (propertize
   (concat
    (pcase icon
      ((or "block" "do_not_disturb_alt") "􀘯")  ;; exclamationmark.octagon
      ("check" "􁁚")  ;; checkmark.diamond
      ("access_time" "􀹴")  ;; clock.badge.checkmark
      ;; 'debug' face is used for "no checker found" and
      ;; 'urgent' for an error within the checker itself.
      ("sim_card_alert" (if (eq face 'doom-modeline-debug)
                            "􀿪"  ;; questionmark.app.dashed
                          "􀇾"))  ;; exclamationmark.triangle
      ("pause" "􀊗")  ;; pause.circle
      ("priority_high" "􀢒")  ;; exclamationmark.2
      (_ (progn
           (user-error "Unknown checker icon name '%s'" icon)
           " ")))
    "  ")
   'face
   `(:inherit ,face
     :family ,sfsymbols-modeline--preferred-font
     :weight extra-light
     :height 1.1)))

(defun sfsymbols-modeline-debug-icon (face &rest _args)
  "Override the `doom-modeline' debugger state icon."
  (propertize "􀌚"  ;; ant
              'face
              `(:inherit ,face
                :family ,sfsymbols-modeline--preferred-font
                :weight extra-light
                :height 1.0)))

;; This doesn't seem worth enabling given its grossness.
;; (defun sfsymbols-modeline-intercept-macro-icon
;;     (_icon-set icon-name _unicode _text &rest args)
;;   "'before-until' advice for `doom-modeline-icon' to override
;; *just* the icons for macro recording."
;;   (when-let ((icon (pcase icon-name
;;                      ("fiber_manual_record" "􀢙")  ;; record.circle
;;                      ("triangle-right" "􀊃"))))  ;; play
;;     (propertize icon
;;                 'face
;;                 `(:inherit ,(or (plist-get args :face) 'mode-line)
;;                   :font ,sfsymbols-modeline--preferred-font
;;                   :weight extra-light
;;                   :height 1.1))))

(defconst sfsymbols-modeline--override-defs
  '((doom-modeline-buffer-file-state-icon .
     sfsymbols-modeline-buffer-file-state-icon)
    (doom-modeline-lsp-icon .
     sfsymbols-modeline-lsp-icon)
    (doom-modeline-vcs-icon .
     sfsymbols-modeline-vcs-icon)
    (doom-modeline-checker-icon .
     sfsymbols-modeline-checker-icon)
    (doom-modeline-debug-icon .
     sfsymbols-modeline-debug-icon))
  "The `doom-modeline' functions that will be overridden, paired
  with their `sfsymbols-modeline' counterpart.")

(defvar doom-modeline--font-width-cache)

;;;###autoload
(define-minor-mode sfsymbols-modeline-mode
  "Toggle the SF Symbols icon overrides for `doom-modeline'."
  :group 'sfsymbols-modeline
  :global t
  :lighter nil
  (when (display-graphic-p)
    (unless (find-font (font-spec
                        :family
                        sfsymbols-modeline--preferred-font))
      (user-error "Cannot find font '%s'; SF Symbols modeline will not work"
                  sfsymbols-modeline--preferred-font))
    (dolist (override-def sfsymbols-modeline--override-defs)
      (let ((doom-fn (car override-def))
            (sfsymbols-fn (cdr override-def)))
        (if sfsymbols-modeline-mode
            (advice-add doom-fn :override sfsymbols-fn)
          (advice-remove doom-fn sfsymbols-fn))))
    (if sfsymbols-modeline-mode
        (let ((width (doom-modeline--font-width)))
          ;; doom-modeline seems to measure a slightly too-small width
          (setf (cdar doom-modeline--font-width-cache) (+ 0.5 width)))
      (doom-modeline-refresh-font-width-cache))))

(provide 'sfsymbols-modeline)
;;; sfsymbols-modeline.el ends here
