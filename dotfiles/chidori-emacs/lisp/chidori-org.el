;;; lisp/chidori-org.el  -*- lexical-binding: t; -*-

;;; commentary
;; Org module structured by @prot plus many hacks from Doom and @tecosaur

(defvar org-directory nil
  "Default directory for Org mode")

(defvar org-agenda-format-date nil)
(defvar org-agenda-time-grid nil)

(defvar +org-capture-todo-file
  (thread-last org-directory (expand-file-name "inbox.org"))
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-notes-file
  (thread-last org-directory (expand-file-name "inbox.org"))
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-journal-file
  (thread-last org-directory (expand-file-name "diary.org"))
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-projects-file
  (thread-last org-directory (expand-file-name "gtd.org"))
  "Default, centralized target for org-capture templates.")

(package! org (org-contrib
               :host github :repo "emacsmirror/org-contrib")
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :preface
  (setq org-persist-directory (concat chidori-cache-dir "org/persist/"))
  (setq org-publish-timestamp-directory (concat chidori-cache-dir "org/timestamps/"))
  (setq org-preview-latex-image-directory (concat chidori-cache-dir "org/latex/"))
  ;; Recognize a), A), a., A., etc -- must be set before org is loaded.
  (setq org-list-allow-alphabetical t)

  ;; Make most of the default modules opt-in to lighten its first-time load
  ;; delay. I sincerely doubt most users use them all.
  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))

  ;; Add our general hooks after the submodules, so that any hooks the
  ;; submodules add run after them, and can overwrite any defaults if necessary.
  (add-hook! 'org-mode-hook
             ;; `show-paren-mode' causes flickering with indent overlays made by
             ;; `org-indent-mode', so we turn off show-paren-mode altogether
             #'doom-disable-show-paren-mode-h
             ;; disable `show-trailing-whitespace'; shows a lot of false positives
             #'doom-disable-show-trailing-whitespace-h
             #'doom-disable-line-numbers-h
             #'+org-enable-auto-reformat-tables-h
             #'+org-enable-auto-update-cookies-h
             #'+org-make-last-point-visible-h)

  (add-hook! 'org-load-hook
             #'+org-init-attachments-h
             #'+org-init-babel-h
             #'+org-init-babel-lazy-loader-h
             #'+org-init-capture-h
             #'+org-init-capture-frame-h
             #'+org-init-custom-links-h
             #'+org-init-export-h
             #'+org-init-hacks-h
             #'+org-init-smartparens-h)

  ;; Wait until an org-protocol link is opened via emacsclient to load
  ;; `org-protocol'. Normally you'd simply require `org-protocol' and use it,
  ;; but the package loads all of org for no compelling reason, so...
  (defadvice! +org--server-visit-files-a (fn files &rest args)
    "Advise `server-visit-files' to load `org-protocol' lazily."
    :around #'server-visit-files
    (if (not (cl-loop with protocol =
                      (if IS-WINDOWS
                          ;; On Windows, the file arguments for `emacsclient'
                          ;; get funnelled through `expand-file-path' by
                          ;; `server-process-filter'. This substitutes
                          ;; backslashes with forward slashes and converts each
                          ;; path to an absolute one. However, *all* absolute
                          ;; paths on Windows will match the regexp ":/+", so we
                          ;; need a more discerning regexp.
                          (regexp-quote
                           (or (bound-and-true-p org-protocol-the-protocol)
                               "org-protocol"))
                        ;; ...but since there is a miniscule possibility users
                        ;; have changed `org-protocol-the-protocol' I don't want
                        ;; this behavior for macOS/Linux users.
                        "")
                      for var in files
                      if (string-match-p (format "%s:/+" protocol) (car var))
                      return t))
        (apply fn files args)
      (require 'org-protocol)
      (apply #'org--protocol-detect-protocol-server fn files args)))
  (after! org-protocol
    (advice-remove 'server-visit-files #'org--protocol-detect-protocol-server))

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not byte-compile-current-file))
    (run-hooks 'org-load-hook))
  :config
  ;;;; General settings
  ;;
  (setq org-directory (convert-standard-filename "~/workspace/notetoself"))
  (setq org-imenu-depth 7)
  (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers t)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist    ; CHANGED in Org 9.3, Emacs 27.1
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)

  ;;;; refile, todo
  ;;
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)")))
  (setq org-todo-keyword-faces
        '(("WAIT" . '(bold org-todo))
          ("MAYBE" . '(shadow org-todo))
          ("CANCEL" . '(bold org-done))))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-priority-faces
        '((?A . '(bold org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

  ;;;; tags
  ;;
  (setq org-tag-alist ; I don't really use those, but whatever
        '(("meeting")
          ("admin")
          ("emacs")
          ("nvim")
          ("modus")
          ("politics")
          ("economics")
          ("philosophy")
          ("book")
          ("movie")
          ("essay")
          ("mail")
          ("purchase")
          ("hardware")
          ("software")
          ("website")))

  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)

  ;;;; log
  ;;
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-read-date-prefer-future 'time)

  ;;;; links
  ;;
  (setq org-link-keep-stored-after-insertion nil)
  ;; TODO 2021-10-15 org-link-make-description-function


  ;;;; agenda
  ;;;;; Basic agenda setup
  (setq org-default-notes-file (thread-last org-directory (expand-file-name "gtd.org")))
  (setq org-agenda-files `(,org-directory "~/Documents"))
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

  ;; NOTE 2021-12-07: In my `prot-org.el' (see further below), I add
  ;; `org-agenda-to-appt' to various relevant hooks.
  ;;
  ;; Create reminders for tasks with a due date when this file is read.
  ;; (run-at-time (* 60 5) nil #'org-agenda-to-appt)

  ;;;;; General agenda view options
  ;; NOTE 2021-12-07: Check further below my `org-agenda-custom-commands'
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?—)
  ;;;;; Advanced agenda view options
  (setq org-agenda-format-date #'prot-org-agenda-format-date-aligned)

  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ,prot-org-custom-daily-agenda
           ((org-agenda-fontify-priorities nil)
            (org-agenda-dim-blocked-tasks nil)))
          ("P" "Plain text daily agenda and top priorities"
           ,prot-org-custom-daily-agenda
           ((org-agenda-with-colors nil)
            (org-agenda-prefix-format "%t %s")
            (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
            (org-agenda-fontify-priorities nil)
            (org-agenda-remove-tags t))
           ("agenda.txt"))))

  ;;;;; Agenda marks
  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-persistent-marks nil)

  ;;;;; Agenda diary entries
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time nil)
  (setq org-agenda-include-diary nil)

  ;;;;; Agenda follow mode
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-follow-indirect t)

  ;;;;; Agenda multi-item tasks
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

  ;;;;; Agenda filters and restricted views
  (setq org-agenda-persistent-filter nil)
  (setq org-agenda-restriction-lock-highlight-subtree t)

  ;;;;; Agenda items with deadline and scheduled timestamps
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time nil)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string
        (concat "Now " (make-string 70 ?-)))
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0600 0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " ....." "-----------------"))
  (setq org-agenda-default-appointment-duration nil)

  ;;;;; Agenda global to-do list
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

  ;;;;; Agenda tagged items
  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column -100)

  ;;;;; Agenda entry
  ;; NOTE I do not use this right now.  Leaving everything to its
  ;; default value.
  (setq org-agenda-start-with-entry-text-mode nil)
  (setq org-agenda-entry-text-maxlines 5)
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "    > ")

  ;;;;; Agenda logging and clocking
  ;; NOTE I do not use these yet, though I plan to.  Leaving everything
  ;; to its default value for the time being.
  (setq org-agenda-log-mode-items '(closed clock))
  (setq org-agenda-clock-consistency-checks
        '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                         ("4:00")
                         :default-face ; This should definitely be reviewed
                         ((:background "DarkRed")
                          (:foreground "white"))
                         :overlap-face nil :gap-face nil :no-end-time-face nil
                         :long-face nil :short-face nil)))
  (setq org-agenda-log-mode-add-notes t)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode nil)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
  (setq org-agenda-search-view-always-boolean nil)
  (setq org-agenda-search-view-force-full-words nil)
  (setq org-agenda-search-view-max-outline-level 0)
  (setq org-agenda-search-headline-for-time t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-cmp-user-defined nil)
  (setq org-agenda-sort-notime-is-late t) ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

  ;;;;; Agenda column view
  ;; NOTE I do not use these, but may need them in the future.
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)
  (setq org-agenda-auto-exclude-function nil)
  (setq org-agenda-bulk-custom-functions nil)

  ;;;;; Agenda habits
  (require 'org-habit)
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 9)

  ;;;; code blocks
  ;;
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

  ;;;; export
  ;;
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (require 'ox-texinfo)
  (require 'ox-md)
  ;; FIXME how to remove everything else?
  (setq org-export-backends '(html texinfo md))

  ;;;; IDs
  ;;
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id)

  ;;;; Hooks and key bindings
  ;;
  ;; Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
  ;; between the two.
  (add-hook 'doom-escape-hook #'+org-remove-occur-highlights-h)
  (map! :leader
   "o"   '(:ignore t :wk "org")
   "oa" #'org-agenda
   "ob" #'org-switchb
   "oc" #'org-capture
   "ol" #'org-store-link
   "om" #'org-tags-view
   "on" #'org-capture
   "oN" #'org-capture-goto-target
   "oo" #'org-clock-goto
   "ot" #'org-todo-list
   "os" #'+default/org-notes-search
   "oS" #'+default/org-notes-headlines
   "ov" #'org-search-view
   "oy" #'+org/export-to-clipboard
   )

  (map!
   :map org-mode-map
   ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
   ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
   ;; that, and should probably be PRed to org.
   [tab]        #'org-cycle

   "C-c C-S-l"  #'+org/remove-link
   "C-c C-i"    #'org-toggle-inline-images
   ;; textmate-esque newline insertion
   "S-RET"      #'+org/shift-return
   "C-RET"      #'+org/insert-item-below
   "C-S-RET"    #'+org/insert-item-above
   "C-M-RET"    #'org-insert-subheading
   [C-return]   #'+org/insert-item-below
   [C-S-return] #'+org/insert-item-above
   [C-M-return] #'org-insert-subheading
   (:when IS-MAC
          [s-return]   #'+org/insert-item-below
          [s-S-return] #'+org/insert-item-above
          [s-M-return] #'org-insert-subheading)
   ;; Org-aware C-a/C-e
   [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
   [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line

   :localleader
   "#"   #'org-update-statistics-cookies
   "'"   #'org-edit-special
   "*"   #'org-ctrl-c-star
   "+"   #'org-ctrl-c-minus
   ","   #'org-switchb
   "@"   #'org-cite-insert
   "."   #'consult-org-heading
   "/"   #'consult-org-agenda
   "A"   #'org-archive-subtree
   "e"   #'org-export-dispatch
   "f"   #'org-footnote-action
   "h"   #'org-toggle-heading
   "i"   #'org-toggle-item
   "I"   #'org-id-get-create
   "k"   #'org-babel-remove-result
   "K"   #'+org/remove-result-blocks
   "n"   #'org-store-link
   "o"   #'org-set-property
   "q"   #'org-set-tags-command
   "t"   #'org-todo
   "T"   #'org-todo-list
   "x"   #'org-toggle-checkbox
   "a"    '(:ignore t :wk "attachments")
   "aa"  #'org-attach
   "ad"  #'org-attach-delete-one
   "aD"  #'org-attach-delete-all
   "af"  #'+org/find-file-in-attachments
   "al"  #'+org/attach-file-and-insert-link
   "an"  #'org-attach-new
   "ao"  #'org-attach-open
   "aO"  #'org-attach-open-in-emacs
   "ar"  #'org-attach-reveal
   "aR"  #'org-attach-reveal-in-emacs
   "au"  #'org-attach-url
   "as"  #'org-attach-set-directory
   "aS"  #'org-attach-sync
   "ac"  #'org-download-screenshot
   "ap"  #'org-download-clipboard
   "aP"  #'org-download-yank
   "b"    '(:ignore t :wk "tables")
   "b-"  #'org-table-insert-hline
   "ba"  #'org-table-align
   "bb"  #'org-table-blank-field
   "bc"  #'org-table-create-or-convert-from-region
   "be"  #'org-table-edit-field
   "bf"  #'org-table-edit-formulas
   "bh"  #'org-table-field-info
   "bs"  #'org-table-sort-lines
   "br"  #'org-table-recalculate
   "bR"  #'org-table-recalculate-buffer-tables
   "bd"   '(:ignore t :wk "delete")
   "bdc" #'org-table-delete-column
   "bdr" #'org-table-kill-row
   "bi"   '(:ignore t :wk "insert")
   "bic" #'org-table-insert-column
   "bih" #'org-table-insert-hline
   "bir" #'org-table-insert-row
   "biH" #'org-table-hline-and-move
   "bt"   '(:ignore t :wk "toggle")
   "btf" #'org-table-toggle-formula-debugger
   "bto" #'org-table-toggle-coordinate-overlays
   "c"    '(:ignore t :wk "clock")
   "cc"  #'org-clock-cancel
   "cd"  #'org-clock-mark-default-task
   "ce"  #'org-clock-modify-effort-estimate
   "cE"  #'org-set-effort
   "cg"  #'org-clock-goto
   "cG"  (cmd! (org-clock-goto 'select))
   "cl"  #'+org/toggle-last-clock
   "ci"  #'org-clock-in
   "cI"  #'org-clock-in-last
   "co"  #'org-clock-out
   "cr"  #'org-resolve-clocks
   "cR"  #'org-clock-report
   "ct"  #'org-evaluate-time-range
   "c="  #'org-clock-timestamps-up
   "c-"  #'org-clock-timestamps-down
   "d"    '(:ignore t :wk "date/deadline")
   "dd"  #'org-deadline
   "ds"  #'org-schedule
   "dt"  #'org-time-stamp
   "dT"  #'org-time-stamp-inactive
   "g"    '(:ignore t :wk "goto")
   "gg"  #'consult-org-heading
   "gG"  #'consult-org-agenda
   "gc"  #'org-clock-goto
   "gC"  (cmd! (org-clock-goto 'select))
   "gi"  #'org-id-goto
   "gr"  #'org-refile-goto-last-stored
   "gv"  #'+org/goto-visible
   "gx"  #'org-capture-goto-last-stored
   "l"    '(:ignore t :wk "links")
   "lc"  #'org-cliplink
   "ld"  #'+org/remove-link
   "li"  #'org-id-store-link
   "ll"  #'org-insert-link
   "lL"  #'org-insert-all-links
   "ls"  #'org-store-link
   "lS"  #'org-insert-last-stored-link
   "lt"  #'org-toggle-link-display
   "P"    '(:ignore t :wk "publish")
   "Pa"  #'org-publish-all
   "Pf"  #'org-publish-current-file
   "Pp"  #'org-publish
   "PP"  #'org-publish-current-project
   "Ps"  #'org-publish-sitemap
   "r"    '(:ignore t :wk "refile")
   "r."  #'+org/refile-to-current-file
   "rc"  #'+org/refile-to-running-clock
   "rl"  #'+org/refile-to-last-location
   "rf"  #'+org/refile-to-file
   "ro"  #'+org/refile-to-other-window
   "rO"  #'+org/refile-to-other-buffer
   "rv"  #'+org/refile-to-visible
   "rr"  #'org-refile
   "rR"  #'org-refile-reverse ; to all `org-refile-targets'
   "s"    '(:ignore t :wk "tree/subtree")
   "sa"  #'org-toggle-archive-tag
   "sb"  #'org-tree-to-indirect-buffer
   "sc"  #'org-clone-subtree-with-time-shift
   "sd"  #'org-cut-subtree
   "sh"  #'org-promote-subtree
   "sj"  #'org-move-subtree-down
   "sk"  #'org-move-subtree-up
   "sl"  #'org-demote-subtree
   "sn"  #'org-narrow-to-subtree
   "sr"  #'org-refile
   "ss"  #'org-sparse-tree
   "sA"  #'org-archive-subtree
   "sN"  #'widen
   "sS"  #'org-sort
   "p"    '(:ignore t :wk "priority")
   "pd"  #'org-priority-down
   "pp"  #'org-priority
   "pu"  #'org-priority-up)

  (map!
   :after org-agenda
   :map org-agenda-mode-map
   :m "C-SPC" #'org-agenda-show-and-scroll-up
   :localleader
   "d"   '(:ignore t :wk "date/deadline")
   "dd" #'org-agenda-deadline
   "ds" #'org-agenda-schedule
   "c"   '(:ignore t :wk "clock")
   "cc" #'org-agenda-clock-cancel
   "cg" #'org-agenda-clock-goto
   "ci" #'org-agenda-clock-in
   "co" #'org-agenda-clock-out
   "cr" #'org-agenda-clockreport-mode
   "cs" #'org-agenda-show-clocking-issues
   "p"   '(:ignore t :wk "priority")
   "pd" #'org-agenda-priority-down
   "pp" #'org-agenda-priority
   "pu" #'org-agenda-priority-up
   "pq" #'org-agenda-set-tags
   "pr" #'org-agenda-refile
   "pt" #'org-agenda-todo))

;;; Custom extensions (prot-org.el)
(after! org
;;;; Tweaks
  ;; Automatic indent detection in org files is meaningless
  (add-to-list 'doom-detect-indentation-excluded-modes 'org-mode)

  (set-eval-handler! 'org-mode #'+org-eval-handler)
  (set-lookup-handlers! 'org-mode
    :definition #'+org-lookup-definition-handler
    :references #'+org-lookup-references-handler
    :documentation #'+org-lookup-documentation-handler)

  ;; Save target buffer after archiving a node.
  (setq org-archive-subtree-save-file-p t)

  ;; Don't number headings with these tags
  (setq org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
        org-num-skip-tags '("noexport" "nonum"))

  ;; Prevent modifications made in invisible sections of an org document, as
  ;; unintended changes can easily go unseen otherwise.
  (setq org-catch-invisible-edits 'smart)

  ;; Global ID state means we can have ID links anywhere. This is required for
  ;; `org-brain', however.
  (setq org-id-locations-file-relative t)

  ;; HACK `org-id' doesn't check if `org-id-locations-file' exists or is
  ;;      writeable before trying to read/write to it.
  (defadvice! +org--fail-gracefully-a (&rest _)
    :before-while '(org-id-locations-save org-id-locations-load)
    (file-writable-p org-id-locations-file))

  (add-hook 'org-open-at-point-functions #'doom-set-jump-h)
  ;; HACK For functions that dodge `org-open-at-point-functions', like
  ;;   `org-id-open', `org-goto', or roam: links.
  (advice-add #'org-mark-ring-push :around #'doom-set-jump-a)

  ;; Add the ability to play gifs, at point or throughout the buffer. However,
  ;; 'playgifs' is stupid slow and there's not much I can do to fix it; use at
  ;; your own risk.
  (add-to-list 'org-startup-options '("inlinegifs" +org-startup-with-animated-gifs at-point))
  (add-to-list 'org-startup-options '("playgifs"   +org-startup-with-animated-gifs t))
  (add-hook! 'org-mode-local-vars-hook
    (defun +org-init-gifs-h ()
      (remove-hook 'post-command-hook #'+org-play-gif-at-point-h t)
      (remove-hook 'post-command-hook #'+org-play-all-gifs-h t)
      (pcase +org-startup-with-animated-gifs
        (`at-point (add-hook 'post-command-hook #'+org-play-gif-at-point-h nil t))
        (`t (add-hook 'post-command-hook #'+org-play-all-gifs-h nil t)))))

;;;; Look and feel
  (set-ligatures! 'org-mode
    :name "#+NAME:"
    :name "#+name:"
    :src_block "#+BEGIN_SRC"
    :src_block "#+begin_src"
    :src_block_end "#+END_SRC"
    :src_block_end "#+end_src"
    :quote "#+BEGIN_QUOTE"
    :quote "#+begin_quote"
    :quote_end "#+END_QUOTE"
    :quote_end "#+end_quote")

  )

;;;; org-modern
(package! org-modern :auto
  :hook (org-mode . global-org-modern-mode)
  :config
  (setq org-modern-label-border 1)
  (setq org-modern-variable-pitch nil)
  (setq org-modern-timestamp t)
  (setq org-modern-table t)
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0)
  (setq org-modern-list ; I swap the defaults for + and *
        '((?- . "•")
          (?+ . "▸")
          (?* . "◦")))
  ;; I don't use those in documents anyway, and if I ever do I need to
  ;; remember what their standard looks are.
  (setq org-modern-internal-target nil)
  (setq org-modern-radio-target nil)
  ;; Org styling, hide markup etc.
  (setq org-pretty-entities t)
  (setq org-ellipsis "…")
  ;; NOTE 2022-03-05: The variables that are commented out are the
  ;; defaults.

  ;; (setq org-modern-star ["◉""○""◈""◇""⁕"])
  ;; (setq org-modern-hide-stars 'leading)
  ;; (setq org-modern-checkbox
  ;;       '((?X . #("▢✓" 0 2 (composition ((2)))))
  ;;         (?- . #("▢–" 0 2 (composition ((2)))))
  ;;         (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; (setq org-modern-horizontal-rule t)
  ;; (setq org-modern-priority t)
  ;; (setq org-modern-todo t)
  ;; (setq org-modern-tag t)
  ;; (setq org-modern-block t)
  ;; (setq org-modern-keyword t)
  ;; (setq org-modern-statistics t)
  ;; (setq org-modern-progress ["○""◔""◐""◕""●"])

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;;; Calendar
(package! calendar :builtin
  :config
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)
  (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

  (require 'solar)
  (setq calendar-latitude 35.17         ; Not my actual coordinates
        calendar-longitude 33.36)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0200")
  (setq calendar-daylight-time-zone-name "+0300"))

;;; Appt (appointment reminders which also integrate with Org agenda)
(package! appt :builtin
  :config
  (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6)

  (run-at-time 10 nil #'appt-activate 1))

;; Let’s setup some org-capture templates, and make them visually nice to access.
(package! doct (:host github :repo "progfolio/doct")
  :commands doct)

(package! org-capture :builtin
  :config
  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))
                  ("Note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Bookmark" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Bookmark"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web")
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch")
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info")
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea")))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra "")
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t")
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t")))
                  ))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook)))))
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys
                                                    prompt
                                                    (not (pos-visible-in-window-p (1- (point-max)))))))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty))

(package! evil-org (:host github :repo "hlissner/evil-org-mode")
  ;; :disabled t
  :hook ((org-mode . evil-org-mode)
         (org-capture-mode . evil-insert-state))
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme)
  (map! :map evil-org-mode-map
        :ni [C-return]   #'+org/insert-item-below
        :ni [C-S-return] #'+org/insert-item-above
        ;; more intuitive RET keybinds
        :n [return]   #'+org/dwim-at-point
        :n "RET"      #'+org/dwim-at-point
        :i [return]   #'+org/return
        :i "RET"      #'+org/return
        :i [S-return] #'+org/shift-return
        :i "S-RET"    #'+org/shift-return
        ;; more vim-esque org motion keys (not covered by evil-org-mode)
        :m "]h"  #'org-forward-heading-same-level
        :m "[h"  #'org-backward-heading-same-level
        :m "]l"  #'org-next-link
        :m "[l"  #'org-previous-link
        :m "]c"  #'org-babel-next-src-block
        :m "[c"  #'org-babel-previous-src-block
        :n "gQ"  #'org-fill-paragraph
        ;; sensible vim-esque folding keybinds
        :n "za"  #'+org/toggle-fold
        :n "zA"  #'org-shifttab
        :n "zc"  #'+org/close-fold
        :n "zC"  #'outline-hide-subtree
        :n "zm"  #'+org/hide-next-fold-level
        :n "zM"  #'+org/close-all-folds
        :n "zn"  #'org-tree-to-indirect-buffer
        :n "zo"  #'+org/open-fold
        :n "zO"  #'outline-show-subtree
        :n "zr"  #'+org/show-next-fold-level
        :n "zR"  #'+org/open-all-folds
        :n "zi"  #'org-toggle-inline-images)
  )


(package! evil-org-agenda :builtin
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  ;; FIXME `evil-org-agenda-mode-map' void function
  ;; `doom-after-module-config-hook' seems not right, change `doom-after-init-hook' fixed
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd doom-leader-key) nil)
  )


;; It’s quite nice to compare Org files
(package! orgdiff (:host github :repo "tecosaur/orgdiff"))

;; Doom needs these dependencies
(package! ob-async :auto)
(package! htmlize :auto)
(package! org-download :auto)
(package! org-pomodoro :auto)
(package! org-yt (:host github :repo "TobiasZawada/org-yt"))
(package! org-cliplink :auto)
(package! ox-clip :auto)


;; Let’s also make creating an org buffer just that little bit easier
(after! evil
  (evil-define-command evil-buffer-org-new (count file)
    "Creates a new ORG buffer replacing the current window, optionally
editing a certain FILE"
    :repeat nil
    (interactive "P<f>")
    (if file
        (evil-edit file)
      (let ((buffer (generate-new-buffer "*new org*")))
        (set-window-buffer nil buffer)
        (with-current-buffer buffer
          (org-mode)))))

  (map! :leader "bo" #'evil-buffer-org-new))


(provide 'chidori-org)
;;; chidori-org.el ends here
