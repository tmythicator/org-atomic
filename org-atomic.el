;;; org-atomic.el --- Modern Atomic Habits tracking for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Maintainer: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.4.0
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; Keywords: outlines, hypermedia, calendar, tasks
;; URL: https://github.com/tmythicator/org-atomic
;; License: GPL-3.0-or-later

;;; Commentary:
;; Brings the philosophy of "Atomic Habits" to Emacs org-mode.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org-atomic-core)
(require 'org-atomic-util)
(require 'subr-x)
(require 'calendar)
(require 'org)
(require 'org-habit)
(require 'org-agenda)
(require 'org-atomic-graph)
(require 'org-atomic-agenda)
(require 'org-atomic-stats)

(defgroup org-atomic nil
  "Options concerning atomic habit tracking in Org-mode."
  :tag "Org Atomic"
  :group 'org-progress)

(defvar org-atomic-mode)

(defun org-atomic-is-active-today-p (&optional day-of-week)
  "Check if the habit at point is scheduled to be active today.
Active status is defined by the `ATOMIC_DAYS' property.  If the property
is missing, it defaults to active (returns non-nil).
If DAY-OF-WEEK is non-nil, use that instead of today's day of week."
  (org-atomic-core-active-on-date-p nil day-of-week))

(defun org-atomic--update-scheduled-date (day &optional repeater)
  "Set the SCHEDULED property of the entry at point to DAY.
DAY is an integer representing day number.
REPEATER is an optional repeater string."
  (let* ((epoch-offset (time-to-days (encode-time 0 0 0 1 1 1970)))
         (new-time (days-to-time (- day epoch-offset)))
         (base-date-str (format-time-string "%Y-%m-%d %a" new-time))
         (new-ts-str
          (if repeater
              (format "<%s %s>" base-date-str repeater)
            (format "<%s>" base-date-str))))
    (org-entry-put nil "SCHEDULED" new-ts-str)))

(defun org-atomic--reschedule-to-active
    (habit-struct &optional base-day)
  "Reschedule the habit at point to the next active day.
HABIT-STRUCT is the parsed habit object.
BASE-DAY is the absolute day number to start looking from (inclusive).
If BASE-DAY is nil, it defaults to the entry's currently scheduled day."
  (let ((scheduled (org-entry-get nil "SCHEDULED")))
    (when (and scheduled (string-match org-ts-regexp3 scheduled))
      (let* ((ts-str (match-string 0 scheduled))
             (time (org-time-string-to-time ts-str))
             (orig-day (time-to-days time))
             (start-day (or base-day orig-day))
             (active-days (org-atomic-core-habit-days habit-struct))
             (next-day
              (org-atomic-util-find-next-active-day
               start-day active-days)))
        (when (/= orig-day next-day)
          (let ((repeater
                 (and (string-match
                       org-atomic-util-repeater-regexp ts-str)
                      (match-string 1 ts-str))))
            (org-atomic--update-scheduled-date next-day
                                               repeater)))))))

(defun org-atomic--auto-repeat-maybe-advice (orig-fun &rest args)
  "Around advice for `org-auto-repeat-maybe' to adjust new SCHEDULED date.
ORIG-FUN is the original function, and ARGS are its arguments."
  (let ((repeated (apply orig-fun args)))
    (when (and repeated (org-is-habit-p))
      (let* ((resolved-marker
              (save-excursion
                (org-atomic-util--goto-heading-at-point)
                (point-marker)))
             (habit-struct
              (org-atomic-core-parse-habit resolved-marker)))
        (when (and habit-struct
                   (org-atomic-core-habit-days habit-struct))
          (org-atomic--reschedule-to-active habit-struct))))
    repeated))

(defvar org-atomic--in-rollover nil
  "Dynamic variable bound to t to prevent infinite recursion in rollover.")

(defun org-atomic-roll-over-habits (&rest _args)
  "Roll over overdue habits in all `org-agenda-files' to today.
This shifts the SCHEDULED property of any habit whose scheduled date is in
the past to the current day (or the next active day)."
  (interactive)
  (when (and org-atomic-mode (not org-atomic--in-rollover))
    (let ((org-atomic--in-rollover t))
      (seq-do
       (lambda (buf)
         (with-current-buffer buf
           (let ((was-modified (buffer-modified-p)))
             (org-map-entries
              #'org-atomic--roll-over-single-habit "+STYLE=\"habit\"")
             (when (and (not was-modified) (buffer-modified-p))
               (save-buffer)))))
       (org-atomic-core--agenda-buffers t)))))

(defun org-atomic--roll-over-single-habit ()
  "Roll over the habit at point to today if it is overdue."
  (let* ((habit (org-atomic-core-parse-habit))
         (scheduled (and habit (org-entry-get nil "SCHEDULED")))
         (scheduled-day
          (when (and scheduled
                     (string-match org-ts-regexp3 scheduled))
            (time-to-days
             (org-time-string-to-time (match-string 0 scheduled))))))
    (when (and scheduled-day (< scheduled-day (org-today)))
      (org-atomic--reschedule-to-active habit (org-today)))))

(defun org-atomic--refresh-agenda ()
  "Refresh the Org Agenda buffer if it exists."
  (let ((buf (get-buffer org-agenda-buffer-name)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (org-agenda-redo)))))

(defun org-atomic--enable ()
  "Enable org-atomic mode and all sub-modules."
  (org-atomic-graph--enable)
  (org-atomic-agenda--enable)
  (advice-add
   'org-auto-repeat-maybe
   :around #'org-atomic--auto-repeat-maybe-advice)
  (advice-add
   'org-agenda-prepare-buffers
   :before #'org-atomic--roll-over-habits)
  (org-atomic--refresh-agenda))

(defun org-atomic--disable ()
  "Disable org-atomic mode and all sub-modules."
  (org-atomic-graph--disable)
  (org-atomic-agenda--disable)
  (advice-remove
   'org-auto-repeat-maybe #'org-atomic--auto-repeat-maybe-advice)
  (advice-remove
   'org-agenda-prepare-buffers #'org-atomic--roll-over-habits)
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
