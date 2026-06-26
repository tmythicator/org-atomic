;;; org-atomic-sparkline.el --- Sparkline rendering for org-atomic  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.1.0
;; License: GPL-3.0-or-later

;;; Commentary:
;; Sparkline rendering and consistency history plotting for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'org-atomic-core)
(require 'org-atomic-util)
(require 'org-habit)

(defgroup org-atomic-sparkline nil
  "Customization group for org-atomic sparklines."
  :group 'org-atomic)

(defcustom org-atomic-done-char ?■
  "Unicode character to represent completed days."
  :type 'character
  :group 'org-atomic-sparkline)

(defcustom org-atomic-missed-char ?□
  "Unicode character to represent missed days."
  :type 'character
  :group 'org-atomic-sparkline)

(defcustom org-atomic-skipped-char ?·
  "Unicode character to represent skipped days."
  :type 'character
  :group 'org-atomic-sparkline)

(defcustom org-atomic-sparkline-start-char ?\[
  "Unicode character to mark the beginning of a sparkline."
  :type 'character
  :group 'org-atomic-sparkline)

(defcustom org-atomic-sparkline-end-char ?\]
  "Unicode character to mark the end of a sparkline."
  :type 'character
  :group 'org-atomic-sparkline)

(defface org-atomic-done-face
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
  :group 'org-atomic-sparkline)

(defface org-atomic-missed-face
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
  :group 'org-atomic-sparkline)

(defface org-atomic-skipped-face
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
  :group 'org-atomic-sparkline)

(defface org-atomic-border-face
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
  "Face for sparkline boundary symbols."
  :group 'org-atomic-sparkline)

(defun org-atomic-draw-sparkline (history)
  "Draw a consistency graph from a HISTORY list.
Each element in HISTORY should be one of `good-done', `good-missed',
`bad-done', `bad-avoided', `skipped', or `future'."
  (let* ((start-str
          (propertize (string org-atomic-sparkline-start-char)
                      'face 'org-atomic-border-face))
         (end-str
          (propertize (string org-atomic-sparkline-end-char)
                      'face
                      'org-atomic-border-face))
         (body-strs
          (mapcar
           (lambda (status)
             (cl-case
              status
              (good-done
               (propertize (string org-atomic-done-char)
                           'face 'org-atomic-done-face))
              (good-missed
               (propertize (string org-atomic-missed-char)
                           'face 'org-atomic-missed-face))
              (bad-done
               (propertize (string org-atomic-done-char)
                           'face 'org-atomic-missed-face))
              (bad-avoided
               (propertize (string org-atomic-missed-char)
                           'face 'org-atomic-done-face))
              (skipped
               (propertize (string org-atomic-skipped-char)
                           'face 'org-atomic-skipped-face))
              (t (propertize " " 'face 'org-atomic-skipped-face))))
           history)))
    (concat start-str (apply #'concat body-strs) end-str)))

(defun org-atomic--get-non-canceled-done-dates (&optional marker)
  "Parse LOGBOOK of entry at MARKER or point.
Return active done dates as day numbers, excluding transitions to CANCELED,
CANCELLED or other non-DONE states."
  (let ((resolved-marker (org-atomic-util--find-marker marker)))
    (when (and resolved-marker (marker-buffer resolved-marker))
      (with-current-buffer (marker-buffer resolved-marker)
        (save-excursion
          (save-restriction
            (goto-char resolved-marker)
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (let ((dates nil)
                  (excluded
                   (mapcar
                    #'upcase org-atomic-excluded-logbook-states)))
              (while
                  (re-search-forward
                   "- State \"\\([^\"]+\\)\"[ \t]+from[ \t]+\"\\([^\"]+\\)\"[ \t]+\\(\\[[^]]+\\]\\|\\(?:<[^>]+>\\)\\)"
                   nil t)
                (let ((state (match-string 1))
                      (ts-str (match-string 3)))
                  (when (and state
                             ts-str
                             (member state org-done-keywords)
                             (not (member (upcase state) excluded)))
                    (let* ((time (org-time-string-to-time ts-str))
                           (day (time-to-days time)))
                      (push day dates)))))
              (nreverse dates))))))))

(defun org-atomic-build-sparkline
    (habit starting current ending &optional parsed)
  "Build an atomic sparkline for HABIT from STARTING to ENDING.
CURRENT is the effective today time.
If PARSED is a non-nil habit plist, use it; otherwise parse the habit."
  (let* ((resolved-marker (org-atomic-util--find-marker))
         (habit-struct
          (or parsed (org-atomic--parse-habit resolved-marker)))
         (done-dates
          (or (org-atomic--get-non-canceled-done-dates
               resolved-marker)
              (org-habit-done-dates habit)))
         (start-day (org-atomic-util--time-to-day-number starting))
         (now-day (org-atomic-util--time-to-day-number current))
         (end-day (org-atomic-util--time-to-day-number ending))
         (target-len (1+ (- end-day start-day)))
         (body-len (- target-len 2))
         (active-days
          (when habit-struct
            (org-atomic-habit-days habit-struct)))
         (habit-type
          (if habit-struct
              (org-atomic-habit-type habit-struct)
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
    (org-atomic-draw-sparkline (nreverse history))))

(provide 'org-atomic-sparkline)
;;; org-atomic-sparkline.el ends here
