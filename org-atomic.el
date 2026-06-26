;;; org-atomic.el --- Modern Atomic Habits tracking for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Maintainer: Alexandr Timchenko <atimchenko92@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; Keywords: outlines, hypermedia, calendar, tasks
;; URL: https://github.com/tmythicator/org-atomic
;; License: GPL-3.0-or-later

;;; Commentary:
;; Brings the philosophy of "Atomic Habits" to Emacs org-mode.

;;; Code:

(require 'cl-lib)
(require 'org-atomic-core)
(require 'org-atomic-util)
(require 'subr-x)
(require 'calendar)
(require 'org)
(require 'org-habit)
(require 'org-agenda)
(require 'org-atomic-sparkline)
(require 'org-atomic-agenda)

(defun org-atomic-is-active-today-p (&optional day-of-week)
  "Check if the habit at point is scheduled to be active today.
Active status is defined by the `ATOMIC_DAYS' property.  If the property
is missing, it defaults to active (returns non-nil).
If DAY-OF-WEEK is non-nil, use that instead of today's day of week."
  (let* ((habit (org-atomic--parse-habit))
         (active-days
          (when habit
            (org-atomic-habit-days habit))))
    (or (null active-days)
        (let ((dow
               (or day-of-week
                   (let ((d
                          (calendar-day-of-week
                           (calendar-current-date))))
                     (if (= d 0)
                         7
                       d)))))
          (member dow active-days)))))

(defun org-atomic--org-habit-build-graph-advice
    (orig-fun habit starting current ending)
  "Advice to intercept `org-habit-build-graph' and draw an atomic sparkline.
ORIG-FUN is the original function.  HABIT, STARTING, CURRENT, and ENDING
are the standard arguments."
  (let ((parsed (org-atomic--parse-habit)))
    (if parsed
        (org-atomic-build-sparkline
         habit starting current ending parsed)
      (funcall orig-fun habit starting current ending))))

(defun org-atomic--find-next-active-day (day active-days)
  "Find the next day starting from DAY (inclusive) that is member of ACTIVE-DAYS.
DAY is an integer day number.  ACTIVE-DAYS is a list of active weekdays."
  (if (null active-days)
      day
    (while (not
            (member (org-atomic-util--day-to-dow day) active-days))
      (setq day (1+ day)))
    day))

(defun org-atomic--update-scheduled-date (day &optional repeater)
  "Set the SCHEDULED property of the entry at point to DAY.
DAY is an integer representing day number.  REPEATER is an optional repeater string."
  (let* ((epoch-offset (time-to-days (encode-time 0 0 0 1 1 1970)))
         (new-time (days-to-time (- day epoch-offset)))
         (base-date-str (format-time-string "%Y-%m-%d %a" new-time))
         (new-ts-str
          (if repeater
              (format "<%s %s>" base-date-str repeater)
            (format "<%s>" base-date-str))))
    (org-entry-put nil "SCHEDULED" new-ts-str)))

(defun org-atomic--org-auto-repeat-maybe-advice (orig-fun &rest args)
  "Around advice for `org-auto-repeat-maybe' to adjust new SCHEDULED date.
ORIG-FUN is the original function, and ARGS are its arguments."
  (let ((repeated (apply orig-fun args)))
    (when (and repeated (org-is-habit-p))
      (let* ((resolved-marker
              (save-excursion
                (org-back-to-heading t)
                (point-marker)))
             (habit-struct (org-atomic--parse-habit resolved-marker)))
        (when (and habit-struct (org-atomic-habit-days habit-struct))
          (let ((scheduled (org-entry-get nil "SCHEDULED")))
            (when (and scheduled
                       (string-match org-ts-regexp3 scheduled))
              (let* ((ts-str (match-string 0 scheduled))
                     (time (org-time-string-to-time ts-str))
                     (day (time-to-days time))
                     (active-days
                      (org-atomic-habit-days habit-struct))
                     (next-day
                      (org-atomic--find-next-active-day
                       day active-days)))
                (when (/= day next-day)
                  (let ((repeater
                         (and (string-match
                               org-atomic-repeater-regexp ts-str)
                              (match-string 1 ts-str))))
                    (org-atomic--update-scheduled-date
                     next-day
                     repeater)))))))))
    repeated))

(defun org-atomic--org-agenda-get-day-entries-advice
    (orig-fun file date &rest args)
  "Around advice to filter out inactive atomic habits.
ORIG-FUN is the original function.  FILE is the file to search, DATE is the
date to scan, and ARGS are additional arguments."
  (let ((rtn (apply orig-fun file date args))
        (filtered nil))
    (dolist (item rtn)
      (let* ((marker (org-atomic-util--find-marker item))
             (habit
              (when marker
                (org-atomic--parse-habit marker)))
             (keep t))
        (when (and habit (org-atomic-habit-days habit))
          (let* ((dow
                  (org-atomic-util--day-to-dow
                   (calendar-day-of-week date)))
                 (active-days (org-atomic-habit-days habit)))
            (unless (member dow active-days)
              (setq keep nil))))
        (when keep
          (push item filtered))))
    (nreverse filtered)))

(defvar org-atomic--in-rollover nil
  "Dynamic variable bound to t to prevent infinite recursion in rollover.")

(defun org-atomic-roll-over-habits (&rest _args)
  "Roll over overdue habits in all `org-agenda-files' to today.
This shifts the SCHEDULED property of any habit whose scheduled date is in
the past to the current day (or the next active day)."
  (interactive)
  (when (and org-atomic-mode (not org-atomic--in-rollover))
    (let ((org-atomic--in-rollover t))
      (dolist (file (org-agenda-files))
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (let ((was-modified (buffer-modified-p)))
              (org-map-entries
               (lambda ()
                 (let* ((habit (org-atomic--parse-habit)))
                   (when habit
                     (let ((scheduled
                            (org-entry-get nil "SCHEDULED")))
                       (when (and scheduled
                                  (string-match
                                   org-ts-regexp3 scheduled))
                         (let* ((ts-str (match-string 0 scheduled))
                                (time
                                 (org-time-string-to-time ts-str))
                                (scheduled-day (time-to-days time))
                                (today (org-today)))
                           (when (< scheduled-day today)
                             (let* ((active-days
                                     (org-atomic-habit-days habit))
                                    (next-day
                                     (org-atomic--find-next-active-day
                                      today active-days))
                                    (repeater
                                     (and (string-match
                                           org-atomic-repeater-regexp
                                           ts-str)
                                          (match-string 1 ts-str))))
                               (org-atomic--update-scheduled-date
                                next-day
                                repeater)))))))))
               "+STYLE=\"habit\"")
              (when (and (not was-modified) (buffer-modified-p))
                (save-buffer)))))))))

(defun org-atomic--refresh-agenda ()
  "Refresh the Org Agenda buffer if it exists."
  (let ((buf (get-buffer org-agenda-buffer-name)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (org-agenda-redo)))))

(defun org-atomic--enable ()
  "Enable org-atomic advices."
  (advice-add
   'org-habit-build-graph
   :around #'org-atomic--org-habit-build-graph-advice)
  (advice-add
   'org-agenda-format-item
   :around #'org-atomic--org-agenda-format-item-advice)
  (advice-add
   'org-auto-repeat-maybe
   :around #'org-atomic--org-auto-repeat-maybe-advice)
  (advice-add
   'org-agenda-get-day-entries
   :around #'org-atomic--org-agenda-get-day-entries-advice)
  (advice-add
   'org-agenda-prepare-buffers
   :before #'org-atomic-roll-over-habits)
  (add-hook
   'org-agenda-finalize-hook #'org-atomic-agenda-finalize-faces)
  (org-atomic--enable-sorting)
  (org-atomic--refresh-agenda))

(defun org-atomic--disable ()
  "Disable org-atomic advices."
  (advice-remove
   'org-habit-build-graph #'org-atomic--org-habit-build-graph-advice)
  (advice-remove
   'org-agenda-format-item
   #'org-atomic--org-agenda-format-item-advice)
  (advice-remove
   'org-auto-repeat-maybe #'org-atomic--org-auto-repeat-maybe-advice)
  (advice-remove
   'org-agenda-get-day-entries
   #'org-atomic--org-agenda-get-day-entries-advice)
  (advice-remove
   'org-agenda-prepare-buffers #'org-atomic-roll-over-habits)
  (remove-hook
   'org-agenda-finalize-hook #'org-atomic-agenda-finalize-faces)
  (org-atomic--disable-sorting)
  (org-atomic--refresh-agenda))

;;;###autoload
(define-minor-mode org-atomic-mode
  "Global minor mode to enable modern atomic habit tracking in Org-mode."
  :global t
  :group
  'org-atomic
  (if org-atomic-mode
      (org-atomic--enable)
    (org-atomic--disable)))

(provide 'org-atomic)
;;; org-atomic.el ends here
