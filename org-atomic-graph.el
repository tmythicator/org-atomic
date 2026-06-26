;;; org-atomic-graph.el --- Consistency graph rendering for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.1.0
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; URL: https://github.com/tmythicator/org-atomic
;; License: GPL-3.0-or-later

;;; Commentary:
;; Consistency graph rendering logic for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'org-atomic-core)
(require 'org-atomic-util)
(require 'org-habit)

(defgroup org-atomic-graph nil
  "Customization group for org-atomic consistency graphs."
  :group 'org-atomic)

(defcustom org-atomic-graph-done-char ?■
  "Unicode character to represent completed days."
  :type 'character
  :group 'org-atomic-graph)

(defcustom org-atomic-graph-missed-char ?□
  "Unicode character to represent missed days."
  :type 'character
  :group 'org-atomic-graph)

(defcustom org-atomic-graph-skipped-char ?·
  "Unicode character to represent skipped days."
  :type 'character
  :group 'org-atomic-graph)

(defcustom org-atomic-graph-start-char ?\[
  "Unicode character to mark the beginning of a consistency graph."
  :type 'character
  :group 'org-atomic-graph)

(defcustom org-atomic-graph-end-char ?\]
  "Unicode character to mark the end of a consistency graph."
  :type 'character
  :group 'org-atomic-graph)

(defface org-atomic-graph-done-face
  '((((background light))
     (:foreground
      "#4caf50"
      :background "#eceff1"
      :inherit fixed-pitch))
    (((background dark))
     (:foreground
      "#81c784"
      :background "#2d3748"
      :inherit fixed-pitch)))
  "Face for completed habit days."
  :group 'org-atomic-graph)

(defface org-atomic-graph-missed-face
  '((((background light))
     (:foreground
      "#c62828"
      :background "#eceff1"
      :inherit fixed-pitch))
    (((background dark))
     (:foreground
      "#e57373"
      :background "#2d3748"
      :inherit fixed-pitch)))
  "Face for missed habit days."
  :group 'org-atomic-graph)

(defface org-atomic-graph-skipped-face
  '((((background light))
     (:foreground
      "#b0bec5"
      :background "#eceff1"
      :inherit fixed-pitch))
    (((background dark))
     (:foreground
      "#718096"
      :background "#2d3748"
      :inherit fixed-pitch)))
  "Face for skipped habit days."
  :group 'org-atomic-graph)

(defface org-atomic-graph-border-face
  '((((background light))
     (:foreground
      "#b0bec5"
      :background "#eceff1"
      :inherit fixed-pitch))
    (((background dark))
     (:foreground
      "#4a5568"
      :background "#2d3748"
      :inherit fixed-pitch)))
  "Face for consistency graph boundary symbols."
  :group 'org-atomic-graph)

(defun org-atomic-graph-draw (history)
  "Draw a consistency graph from a HISTORY list.
Each element in HISTORY should be one of `good-done', `good-missed',
`bad-done', `bad-avoided', `skipped', or `future'."
  (let* ((start-str
          (propertize (string org-atomic-graph-start-char)
                      'face
                      'org-atomic-graph-border-face))
         (end-str
          (propertize (string org-atomic-graph-end-char)
                      'face
                      'org-atomic-graph-border-face))
         (body-strs
          (mapcar
           (lambda (status)
             (cl-case
              status
              (good-done
               (propertize (string org-atomic-graph-done-char)
                           'face 'org-atomic-graph-done-face))
              (good-missed
               (propertize (string org-atomic-graph-missed-char)
                           'face 'org-atomic-graph-missed-face))
              (bad-done
               (propertize (string org-atomic-graph-done-char)
                           'face 'org-atomic-graph-missed-face))
              (bad-avoided
               (propertize (string org-atomic-graph-missed-char)
                           'face 'org-atomic-graph-done-face))
              (skipped
               (propertize (string org-atomic-graph-skipped-char)
                           'face 'org-atomic-graph-skipped-face))
              (t
               (propertize " "
                           'face 'org-atomic-graph-skipped-face))))
           history)))
    (concat start-str (apply #'concat body-strs) end-str)))

(defun org-atomic-graph--get-non-canceled-done-dates
    (&optional marker)
  "Parse LOGBOOK of entry at MARKER or point.
Return active done dates as day numbers, excluding transitions to CANCELED,
CANCELLED or other non-DONE states."
  (let ((resolved-marker (org-atomic-util--find-marker marker)))
    (org-atomic-util-with-heading-at-marker
     resolved-marker (org-narrow-to-subtree) (goto-char (point-min))
     (let ((dates nil)
           (excluded
            (mapcar
             #'upcase org-atomic-core-excluded-logbook-states)))
       (while (re-search-forward org-atomic-util-logbook-state-regexp
                                 nil
                                 t)
         (let ((state (match-string 1))
               (ts-str (match-string 3)))
           (when (and state
                      ts-str
                      (member state org-done-keywords)
                      (not (member (upcase state) excluded)))
             (let* ((time (org-time-string-to-time ts-str))
                    (day (time-to-days time)))
               (push day dates)))))
       (nreverse dates)))))

(defun org-atomic-graph-build
    (habit starting current ending &optional parsed)
  "Build an atomic consistency graph for HABIT from STARTING to ENDING.
CURRENT is the effective today time.
If PARSED is a non-nil habit plist, use it; otherwise parse the habit."
  (let* ((resolved-marker (org-atomic-util--find-marker))
         (habit-struct
          (or parsed (org-atomic-core-parse-habit resolved-marker)))
         (done-dates
          (or (org-atomic-graph--get-non-canceled-done-dates
               resolved-marker)
              (org-habit-done-dates habit)))
         (start-day (org-atomic-util--time-to-day-number starting))
         (now-day (org-atomic-util--time-to-day-number current))
         (end-day (org-atomic-util--time-to-day-number ending))
         (target-len (1+ (- end-day start-day)))
         (body-len (- target-len 2))
         (active-days
          (when habit-struct
            (org-atomic-core-habit-days habit-struct)))
         (habit-type
          (if habit-struct
              (org-atomic-core-habit-type habit-struct)
            "good"))
         (is-bad-habit
          (string= (downcase (string-trim habit-type)) "bad"))
         (history nil))
    (let* ((future-days (- end-day now-day))
           (truncate-future (min 2 (max 0 future-days)))
           (truncate-past (- 2 truncate-future))
           (loop-start (+ start-day truncate-past))
           (d loop-start))
      (while (< d (+ loop-start body-len))
        (let* ((done-p (member d done-dates))
               (weekday (org-atomic-util--day-to-dow d))
               (is-active-day
                (or (null active-days) (member weekday active-days))))
          (cond
           ((> d now-day)
            (push 'future history))
           ((not is-active-day)
            (push 'skipped history))
           ((not is-bad-habit)
            (if done-p
                (push 'good-done history)
              (push 'good-missed history)))
           (is-bad-habit
            (if done-p
                (push 'bad-done history)
              (push 'bad-avoided history)))))
        (setq d (1+ d))))
    (org-atomic-graph-draw (nreverse history))))

(provide 'org-atomic-graph)
;;; org-atomic-graph.el ends here
