;;; org-atomic-stats.el --- Habit tracking statistics and dashboard for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.3.0
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

(defun org-atomic-stats--calculate-streaks (habit-struct done-dates)
  "Calculate current and longest streaks for HABIT-STRUCT using DONE-DATES."
  (let*
      ((active-days (org-atomic-core-habit-days habit-struct))
       (type (org-atomic-core-habit-type habit-struct))
       (is-bad-habit
        (and type (string= (downcase (string-trim type)) "bad")))
       (today (org-today))
       ;; Start checking from today, or yesterday if today is not completed yet
       (start-eval-day
        (if (and
             (or (null active-days)
                 (member
                  (org-atomic-util--day-to-dow today) active-days))
             (if is-bad-habit
                 (member today done-dates) ; today is broken/failure for bad habit
               (not (member today done-dates)))) ; today is not completed yet for good habit
            (1- today)
          today))
       (current-streak 0)
       (longest-streak 0)
       (temp-streak 0)
       ;; Scan back up to 365 days or the oldest done date
       (min-day
        (if done-dates
            (apply #'min done-dates)
          (- today 365)))
       (d start-eval-day)
       (streak-broken nil))
    (while (>= d min-day)
      (let* ((weekday (org-atomic-util--day-to-dow d))
             (is-active
              (or (null active-days) (member weekday active-days))))
        (when is-active
          (let* ((done-p (member d done-dates))
                 (success
                  (if is-bad-habit
                      (not done-p)
                    done-p)))
            (if success
                (progn
                  (setq temp-streak (1+ temp-streak))
                  (setq longest-streak
                        (max longest-streak temp-streak))
                  (unless streak-broken
                    (setq current-streak (1+ current-streak))))
              ;; Failure!
              (setq temp-streak 0)
              (setq streak-broken t)))))
      (setq d (1- d)))
    (list
     :current-streak current-streak
     :longest-streak longest-streak)))

(defun org-atomic-stats--calculate-rates
    (habit-struct done-dates range-days)
  "Calculate success rate for HABIT-STRUCT using DONE-DATES over RANGE-DAYS."
  (let* ((active-days (org-atomic-core-habit-days habit-struct))
         (type (org-atomic-core-habit-type habit-struct))
         (is-bad-habit
          (and type (string= (downcase (string-trim type)) "bad")))
         (today (org-today))
         (start-day (- today (1- range-days)))
         (success-count 0)
         (active-count 0)
         (d start-day))
    (while (<= d today)
      (let* ((weekday (org-atomic-util--day-to-dow d))
             (is-active
              (or (null active-days) (member weekday active-days))))
        (when is-active
          (setq active-count (1+ active-count))
          (let* ((done-p (member d done-dates))
                 (success
                  (if is-bad-habit
                      (not done-p)
                    done-p)))
            (when success
              (setq success-count (1+ success-count))))))
      (setq d (1+ d)))
    (let ((pct
           (org-atomic-util-calculate-percentage
            success-count active-count)))
      (list
       :success-count success-count
       :active-count active-count
       :percentage pct))))

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
  (let ((habits nil))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (re-search-forward
                      org-atomic-util-headline-regexp
                      nil t)
                (let ((habit (org-atomic-core-parse-habit)))
                  (when habit
                    (push (list
                           habit (point-marker)
                           (org-atomic-util--clean-headline-text
                            (org-get-heading t t t t)))
                          habits)))))))))
    (nreverse habits)))

(defun org-atomic-stats--format-habit-card (habit marker range-days)
  "Format a single HABIT card at MARKER over RANGE-DAYS."
  (let* ((headline
          (org-atomic-util-with-heading-at-marker
           marker
           (org-atomic-util--clean-headline-text
            (org-get-heading t t t t))))
         (id (org-atomic-core-habit-id habit))
         (type (org-atomic-core-habit-type habit))
         (is-bad
          (and type (string= (downcase (string-trim type)) "bad")))
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
     (let ((rules nil))
       (if is-bad
           (let ((invisible (org-atomic-core-habit-invisible habit))
                 (unattractive
                  (org-atomic-core-habit-unattractive habit))
                 (hard (org-atomic-core-habit-hard habit))
                 (unsatisfying
                  (org-atomic-core-habit-unsatisfying habit)))
             (when invisible
               (push (cons "Invisible" invisible) rules))
             (when unattractive
               (push (cons "Unattractive" unattractive) rules))
             (when hard
               (push (cons "Hard" hard) rules))
             (when unsatisfying
               (push (cons "Unsatisfying" unsatisfying) rules)))
         (let ((obvious (org-atomic-core-habit-obvious habit))
               (attractive (org-atomic-core-habit-attractive habit))
               (easy (org-atomic-core-habit-easy habit))
               (satisfying (org-atomic-core-habit-satisfying habit)))
           (when obvious
             (push (cons "Obvious" obvious) rules))
           (when attractive
             (push (cons "Attractive" attractive) rules))
           (when easy
             (push (cons "Easy" easy) rules))
           (when satisfying
             (push (cons "Satisfying" satisfying) rules))))
       (setq rules (nreverse rules))
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
         (good-habits nil)
         (bad-habits nil))
    ;; Separate good and bad habits
    (dolist (item habits)
      (let* ((habit (nth 0 item))
             (type (org-atomic-core-habit-type habit)))
        (if (and type (string= (downcase (string-trim type)) "bad"))
            (push item bad-habits)
          (push item good-habits))))
    (setq good-habits (nreverse good-habits))
    (setq bad-habits (nreverse bad-habits))

    ;; Print Title Banner
    (insert "\n")
    (insert
     (propertize "  ORG-ATOMIC HABITS STATS\n"
                 'face
                 'org-atomic-stats-header-face))

    (insert "\n")
    ;; Display overall stats summary
    (let* ((total-habits (length habits))
           (total-good (length good-habits))
           (total-bad (length bad-habits)))
      (insert
       (propertize "  Summary:\n"
                   'face
                   'org-atomic-stats-subheader-face))
      (insert
       (format "    Total Habits: %d  (Good: %d, Bad: %d)\n"
               total-habits
               total-good
               total-bad))
      (insert (format "    Tracking Range: last %d days\n\n" range)))

    ;; Print Good Habits Section
    (when good-habits
      (insert
       (propertize "  GOOD HABITS\n"
                   'face
                   'org-atomic-stats-subheader-face))
      (insert "\n")
      (dolist (item good-habits)
        (insert
         (org-atomic-stats--format-habit-card
          (nth 0 item) (nth 1 item) range))))

    ;; Print Bad Habits Section
    (when bad-habits
      (insert
       (propertize "  BAD HABITS\n"
                   'face
                   'org-atomic-stats-subheader-face))
      (insert "\n")
      (dolist (item bad-habits)
        (insert
         (org-atomic-stats--format-habit-card
          (nth 0 item) (nth 1 item) range))))

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
