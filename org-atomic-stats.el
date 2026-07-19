;;; org-atomic-stats.el --- Habit tracking statistics and dashboard for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.3.1
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; URL: https://github.com/tmythicator/org-atomic
;; License: GPL-3.0-or-later

;;; Commentary:
;; Provides statistics calculation (streaks, success rates)
;; and an interactive dashboard buffer (`*org-atomic-stats*`) for atomic habits.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'org)
(require 'org-agenda)
(require 'org-atomic-core)
(require 'org-atomic-util)
(require 'org-atomic-graph)
(require 'org-atomic-agenda)

(defgroup org-atomic-stats nil
  "Customization options for org-atomic statistics."
  :group 'org-atomic)

(defcustom org-atomic-stats-default-range 30
  "Default number of days to look back for calculating completion statistics."
  :type 'integer
  :group 'org-atomic-stats)

(defface org-atomic-stats-header-face
  '((((background light))
     (:foreground "#0f766e" :weight bold :height 1.2))
    (((background dark))
     (:foreground "#2dd4bf" :weight bold :height 1.2)))
  "Face for the dashboard main header."
  :group 'org-atomic-stats)

(defface org-atomic-stats-subheader-face
  '((((background light))
     (:foreground "#1e293b" :weight bold :height 1.1))
    (((background dark))
     (:foreground "#f1f5f9" :weight bold :height 1.1)))
  "Face for section headers in the dashboard."
  :group 'org-atomic-stats)

(defface org-atomic-stats-headline-face
  '((((background light)) (:foreground "#0f172a" :weight bold))
    (((background dark)) (:foreground "#f8fafc" :weight bold)))
  "Face for habit headlines in the dashboard."
  :group 'org-atomic-stats)

(defface org-atomic-stats-label-face
  '((((background light)) (:foreground "#64748b" :weight bold))
    (((background dark)) (:foreground "#94a3b8" :weight bold)))
  "Face for labels in the dashboard cards."
  :group 'org-atomic-stats)

(defun org-atomic-stats--step-streak (acc day habit-struct done-dates)
  "Pure function: computes the next streak state for a given DAY.
ACC is a tuple of (current longest temp broken)."
  (let ((success-p
         (org-atomic-core-habit-success-p
          habit-struct (member day done-dates))))
    (pcase-let* ((`(,curr ,long ,temp ,broken) acc))
      (if success-p
          (list
           (if broken
               curr
             (1+ curr))
           (max long (1+ temp)) (1+ temp) broken)
        (list curr long 0 t)))))

(defun org-atomic-stats--calculate-streaks (habit-struct done-dates)
  "Calculate current and longest streaks for HABIT-STRUCT using DONE-DATES."
  (let*
      ((active-days (org-atomic-core-habit-days habit-struct))
       (today (org-today))
       (today-active-p
        (or (null active-days)
            (member (org-atomic-util--day-to-dow today) active-days)))
       (today-success-p
        (org-atomic-core-habit-success-p
         habit-struct (member today done-dates)))

       ;; If today is an active day but not yet successful, start evaluating from yesterday
       (start-eval-day
        (if (and today-active-p (not today-success-p))
            (1- today)
          today))
       (min-day
        (if done-dates
            (apply #'min done-dates)
          (- today 365)))
       (days-seq
        (seq-filter
         (lambda (d)
           (let ((weekday (org-atomic-util--day-to-dow d)))
             (or (null active-days) (member weekday active-days))))
         (number-sequence start-eval-day min-day -1)))
       (result
        (seq-reduce
         (lambda (acc d)
           (org-atomic-stats--step-streak
            acc d habit-struct done-dates))
         days-seq
         (list 0 0 0 nil)))) ;; Initial state: (current longest temp broken)
    (list
     :current-streak (nth 0 result)
     :longest-streak (nth 1 result))))

(defun org-atomic-stats--calculate-rates
    (habit-struct done-dates range-days)
  "Calculate success rate for HABIT-STRUCT using DONE-DATES over RANGE-DAYS."
  (let* ((active-days (org-atomic-core-habit-days habit-struct))
         (today (org-today))
         (start-day (- today (1- range-days)))
         (days-seq (number-sequence start-day today))
         (active-days-seq
          (seq-filter
           (lambda (d)
             (let ((weekday (org-atomic-util--day-to-dow d)))
               (or (null active-days) (member weekday active-days))))
           days-seq))
         (active-count (length active-days-seq))
         (success-count
          (seq-count
           (lambda (d)
             (org-atomic-core-habit-success-p
              habit-struct (member d done-dates)))
           active-days-seq))
         (pct
          (org-atomic-util-calculate-percentage
           success-count active-count)))
    (list
     :success-count success-count
     :active-count active-count
     :percentage pct)))

(defun org-atomic-stats--draw-bar (percentage width)
  "Draw a Unicode progress bar of WIDTH chars representing PERCENTAGE."
  (let* ((filled-width (round (* width (/ percentage 100.0))))
         (empty-width (- width filled-width))
         (filled-str (make-string filled-width ?█))
         (empty-str (make-string empty-width ?░)))
    (concat
     (propertize filled-str 'face 'org-atomic-graph-done-face)
     (propertize empty-str 'face 'org-atomic-graph-skipped-face))))

(defun org-atomic-stats--collect-habits ()
  "Scan all `org-agenda-files' and collect parsed habits with their markers."
  (delq
   nil
   (org-map-entries
    (lambda ()
      (let ((habit (org-atomic-core-parse-habit)))
        (when habit
          (list
           habit (point-marker)
           (org-atomic-util--clean-headline-text
            (org-get-heading t t t t))))))
    nil 'agenda)))

(defun org-atomic-stats--format-habit-card (habit marker range-days)
  "Format a single HABIT card at MARKER over RANGE-DAYS."
  (let* ((headline
          (org-atomic-util-with-heading-at-marker
           marker
           (org-atomic-util--clean-headline-text
            (org-get-heading t t t t))))
         (id (org-atomic-core-habit-id habit))
         (is-bad (org-atomic-core-habit-bad-p habit))
         (why (org-atomic-core-habit-why habit))

         ;; Stats
         (done-dates
          (org-atomic-graph--get-non-canceled-done-dates marker))
         (today (org-today))
         (rates
          (org-atomic-stats--calculate-rates
           habit done-dates range-days))
         (pct (plist-get rates :percentage))

         ;; Streaks
         (streaks
          (org-atomic-stats--calculate-streaks habit done-dates))
         (current-streak (plist-get streaks :current-streak))
         (longest-streak (plist-get streaks :longest-streak))

         ;; Sparkline (last 14 days)
         (sparkline
          (org-atomic-util-with-heading-at-marker
           marker
           (org-atomic-graph-build
            nil (- today 14) today today habit)))

         ;; Card parts
         (title-str
          (concat
           (if id
               (propertize (format "[%s] " id)
                           'face
                           'org-atomic-agenda-id-face)
             "")
           (propertize headline
                       'face
                       'org-atomic-stats-headline-face)))
         (bar-str (org-atomic-stats--draw-bar pct 10)))

    (concat
     "  " title-str "\n"
     (format "  %-18s │ %-25s │ Rate: %3d%%  %s\n"
             (propertize (if is-bad
                             "Type: Bad Habit"
                           "Type: Good Habit")
                         'face 'org-atomic-stats-label-face)
             (format "Streak: %d days (Max: %d)"
                     current-streak
                     longest-streak)
             pct bar-str)
     (format "  %-18s %s\n"
             (propertize "History (14d):"
                         'face
                         'org-atomic-stats-label-face)
             sparkline)
     (when why
       (format "  %-18s %s\n"
               (propertize "Why:" 'face 'org-atomic-stats-label-face)
               why))
     ;; Rules
     (let ((rules (org-atomic-core-habit-strategies habit)))
       (when rules
         (concat
          (mapconcat (lambda (rule)
                       (format "  %-18s %s\n"
                               (propertize
                                (concat (car rule) ":")
                                'face 'org-atomic-stats-label-face)
                               (cdr rule)))
                     rules
                     "")
          "\n")))
     "\n")))

(defun org-atomic-stats--render ()
  "Render the Org-Atomic stats page."
  (let* ((habits (org-atomic-stats--collect-habits))
         (range org-atomic-stats-default-range)
         (grouped
          (seq-group-by
           (lambda (item)
             (org-atomic-core-habit-bad-p (car item)))
           habits))
         (bad-habits (alist-get t grouped))
         (good-habits (alist-get nil grouped))
         (total-habits (length habits))
         (total-good (length good-habits))
         (total-bad (length bad-habits))
         (banner
          (concat
           "\n"
           (propertize "  ORG-ATOMIC HABITS STATS\n\n"
                       'face
                       'org-atomic-stats-header-face)))
         (summary
          (concat
           (propertize "  Summary:\n"
                       'face 'org-atomic-stats-subheader-face)
           (format "    Total Habits: %d  (Good: %d, Bad: %d)\n"
                   total-habits total-good total-bad)
           (format "    Tracking Range: last %d days\n\n" range)))
         (good-section
          (when good-habits
            (concat
             (propertize "  GOOD HABITS\n\n"
                         'face
                         'org-atomic-stats-subheader-face)
             (string-join (seq-map
                           (lambda (item)
                             (org-atomic-stats--format-habit-card
                              (nth 0 item) (nth 1 item) range))
                           good-habits)
                          ""))))
         (bad-section
          (when bad-habits
            (concat
             (propertize "  BAD HABITS\n\n"
                         'face
                         'org-atomic-stats-subheader-face)
             (string-join (seq-map
                           (lambda (item)
                             (org-atomic-stats--format-habit-card
                              (nth 0 item) (nth 1 item) range))
                           bad-habits)
                          "")))))
    (insert (concat banner summary good-section bad-section))
    (goto-char (point-min))))

(defun org-atomic-stats-refresh ()
  "Refresh the Org-Atomic stats buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-atomic-stats--render)))

(defvar org-atomic-stats-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-atomic-stats-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-atomic-stats-mode'.")

(define-derived-mode
 org-atomic-stats-mode
 special-mode
 "Org-Atomic Stats"
 "Major mode for displaying Org-Atomic habit tracking statistics."
 (setq-local truncate-lines t))

;;;###autoload
(defun org-atomic-stats ()
  "Open the Org-Atomic statistics dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*org-atomic-stats*")))
    (with-current-buffer buf
      (org-atomic-stats-mode)
      (org-atomic-stats-refresh))
    (select-window (display-buffer buf))))

(provide 'org-atomic-stats)
;;; org-atomic-stats.el ends here
