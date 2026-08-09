;;; org-atomic-graph.el --- Consistency graph rendering for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.4.1
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; URL: https://github.com/tmythicator/org-atomic
;; License: GPL-3.0-or-later

;;; Commentary:
;; Consistency graph rendering logic for org-atomic.

;;; Code:

(require 'seq)
(require 'pcase)
(require 'subr-x)
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

(defcustom org-atomic-graph-show-percentage t
  "If non-nil, append the completion percentage to the consistency graph."
  :type 'boolean
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

(defun org-atomic-graph--status-kind (status)
  "Classify habit history STATUS into :success, :missed, or :ignored."
  (pcase status
    ((or 'good-done 'bad-avoided) :success)
    ((or 'good-missed 'bad-done) :missed)
    (_ :ignored)))

(defun org-atomic-graph--calculate-history-percentage (history)
  "Calculate completion percentage from HISTORY in a single pass."
  (pcase-let* ((`(:success ,succ :active ,active)
                (seq-reduce
                 (pcase-lambda (`(:success ,s :active ,a) status)
                   (pcase (org-atomic-graph--status-kind status)
                     (:success (list :success (1+ s) :active (1+ a)))
                     (:missed (list :success s :active (1+ a)))
                     (:ignored (list :success s :active a))))
                 history '(:success 0 :active 0))))
    (org-atomic-util-calculate-percentage succ active)))

(defun org-atomic-graph--format-day-char (status)
  "Render propertized character representing habit day STATUS."
  (pcase status
    ('good-done
     (propertize (string org-atomic-graph-done-char)
                 'face
                 'org-atomic-graph-done-face))
    ('good-missed
     (propertize (string org-atomic-graph-missed-char)
                 'face
                 'org-atomic-graph-missed-face))
    ('bad-done
     (propertize (string org-atomic-graph-done-char)
                 'face
                 'org-atomic-graph-missed-face))
    ('bad-avoided
     (propertize (string org-atomic-graph-missed-char)
                 'face
                 'org-atomic-graph-done-face))
    ('skipped
     (propertize (string org-atomic-graph-skipped-char)
                 'face
                 'org-atomic-graph-skipped-face))
    (_ (propertize " " 'face 'org-atomic-graph-skipped-face))))

(defun org-atomic-graph-draw (history &optional show-percentage)
  "Draw a consistency graph from a HISTORY list.
Each element in HISTORY should be one of `good-done', `good-missed',
`bad-done', `bad-avoided', `skipped', or `future'.
If SHOW-PERCENTAGE is non-nil, append the completion percentage."
  (let* ((start-str
          (propertize (string org-atomic-graph-start-char)
                      'face
                      'org-atomic-graph-border-face))
         (end-str
          (propertize (string org-atomic-graph-end-char)
                      'face
                      'org-atomic-graph-border-face))
         (body-str
          (mapconcat #'org-atomic-graph--format-day-char history ""))
         (pct-str
          (if show-percentage
              (propertize
               (format " %d%%"
                       (org-atomic-graph--calculate-history-percentage
                        history))
               'face 'org-atomic-graph-skipped-face)
            "")))
    (concat start-str body-str end-str pct-str)))

(defun org-atomic-graph--determine-day-status
    (d now-day done-p active-days is-bad-habit)
  "Pure function: determine the status symbol for day D.
Compares D against NOW-DAY.  Uses DONE-P to check completion,
ACTIVE-DAYS to check if it's a scheduled day, and IS-BAD-HABIT
to determine the habit type."
  (let ((is-active-day (org-atomic-util-day-active-p d active-days)))
    (cond
     ((> d now-day)
      'future)
     ((not is-active-day)
      'skipped)
     ((not is-bad-habit)
      (if done-p
          'good-done
        'good-missed))
     (is-bad-habit
      (if done-p
          'bad-done
        'bad-avoided)))))

(defun org-atomic-graph-build
    (habit starting current ending &optional parsed)
  "Build an atomic consistency graph for HABIT from STARTING to ENDING.
CURRENT is the effective today time.
If PARSED is a non-nil habit plist, use it; otherwise parse the habit."
  (let* ((resolved-marker (org-atomic-util--find-marker))
         (habit-struct
          (or parsed (org-atomic-core-parse-habit resolved-marker)))
         (done-dates
          (or (org-atomic-core-get-done-dates resolved-marker)
              (org-habit-done-dates habit)))
         (start-day (org-atomic-util--time-to-day-number starting))
         (now-day (org-atomic-util--time-to-day-number current))
         (end-day (org-atomic-util--time-to-day-number ending))
         (target-len (1+ (- end-day start-day)))
         (body-len (- target-len 2))
         (active-days
          (when habit-struct
            (org-atomic-core-habit-days habit-struct)))
         (is-bad-habit
          (and habit-struct
               (org-atomic-core-habit-bad-p habit-struct))))
    (let* ((future-days (- end-day now-day))
           (truncate-future (min 2 (max 0 future-days)))
           (truncate-past (- 2 truncate-future))
           (loop-start (+ start-day truncate-past))
           (loop-end (1- (+ loop-start body-len)))
           (history
            (seq-map
             (lambda (d)
               (org-atomic-graph--determine-day-status
                d
                now-day
                (member d done-dates)
                active-days
                is-bad-habit))
             (number-sequence loop-start loop-end))))
      (org-atomic-graph-draw history
                             org-atomic-graph-show-percentage))))

(defun org-atomic-graph--build-graph-advice (orig-fun &rest args)
  "Advice to intercept `org-habit-build-graph' and draw an atomic graph.
ORIG-FUN is the shadowed upstream function, and ARGS contains the standard
arguments: (HABIT STARTING CURRENT ENDING)."
  (if-let* ((parsed (org-atomic-core-parse-habit)))
      (apply #'org-atomic-graph-build (append args (list parsed)))
    (apply orig-fun args)))

(defun org-atomic-graph--enable ()
  "Enable graph advice for org-habit."
  (advice-add
   'org-habit-build-graph
   :around #'org-atomic-graph--build-graph-advice))

(defun org-atomic-graph--disable ()
  "Disable graph advice for org-habit."
  (advice-remove
   'org-habit-build-graph #'org-atomic-graph--build-graph-advice))

(provide 'org-atomic-graph)
;;; org-atomic-graph.el ends here
