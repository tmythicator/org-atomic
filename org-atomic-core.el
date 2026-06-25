;;; org-atomic-core.el --- Core domain structures and utilities for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko

;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Keywords: outlines, hypermedia, calendar

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Core structures, plist definition, and properties parsing for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'org)

(defgroup org-atomic nil
  "Options concerning atomic habit tracking in Org-mode."
  :tag "Org Atomic"
  :group 'org-progress)

(defcustom org-atomic-day-groups
  '(("workdays" 1 2 3 4 5) ("weekends" 6 7) ("daily" 1 2 3 4 5 6 7))
  "Alist of day group names and their active weekdays (1=Monday, 7=Sunday)."
  :type '(alist :key-type string :value-type (repeat integer))
  :group 'org-atomic)

(defcustom org-atomic-excluded-logbook-states
  '("CANCELED" "CANCELLED" "SKIPPED" "FAILED")
  "List of Org TODO keywords that should NOT be counted as completions.
These states represent canceled, skipped, or failed attempts rather
than successful habit executions."
  :type '(repeat string)
  :group 'org-atomic)

(defun org-atomic-habit-create (&rest args)
  "Create a new atomic habit plist with default values, overridden by ARGS."
  (let ((defaults
         (list
          :id nil
          :obvious nil
          :attractive nil
          :easy nil
          :satisfying nil
          :why nil
          :invisible nil
          :unattractive nil
          :hard nil
          :unsatisfying nil
          :type "good"
          :days nil
          :anchor nil)))
    (while args
      (let ((key (pop args))
            (val (pop args)))
        (plist-put defaults key val)))
    defaults))

(defun org-atomic-habit-p (habit)
  "Return non-nil if HABIT is a valid atomic habit plist."
  (and (listp habit) (plist-member habit :id) (plist-get habit :id)))

;; Inline accessors to avoid cl-defstruct requirement warnings
(defsubst org-atomic-habit-id (habit)
  "Get :id from HABIT plist."
  (plist-get habit :id))

(defsubst org-atomic-habit-obvious (habit)
  "Get :obvious from HABIT plist."
  (plist-get habit :obvious))

(defsubst org-atomic-habit-attractive (habit)
  "Get :attractive from HABIT plist."
  (plist-get habit :attractive))

(defsubst org-atomic-habit-easy (habit)
  "Get :easy from HABIT plist."
  (plist-get habit :easy))

(defsubst org-atomic-habit-satisfying (habit)
  "Get :satisfying from HABIT plist."
  (plist-get habit :satisfying))

(defsubst org-atomic-habit-why (habit)
  "Get :why from HABIT plist."
  (plist-get habit :why))

(defsubst org-atomic-habit-invisible (habit)
  "Get :invisible from HABIT plist."
  (plist-get habit :invisible))

(defsubst org-atomic-habit-unattractive (habit)
  "Get :unattractive from HABIT plist."
  (plist-get habit :unattractive))

(defsubst org-atomic-habit-hard (habit)
  "Get :hard from HABIT plist."
  (plist-get habit :hard))

(defsubst org-atomic-habit-unsatisfying (habit)
  "Get :unsatisfying from HABIT plist."
  (plist-get habit :unsatisfying))

(defsubst org-atomic-habit-type (habit)
  "Get :type from HABIT plist."
  (plist-get habit :type))

(defsubst org-atomic-habit-days (habit)
  "Get :days from HABIT plist."
  (plist-get habit :days))

(defsubst org-atomic-habit-anchor (habit)
  "Get :anchor from HABIT plist."
  (plist-get habit :anchor))

(defun org-atomic--get-marker-from-string (str)
  "Extract an Org marker from the text properties of STR."
  (when (stringp str)
    (let ((pos
           (or (text-property-not-all 0 (length str) 'org-marker nil
                                      str)
               (text-property-not-all
                0 (length str) 'org-hd-marker nil
                str))))
      (when pos
        (or (get-text-property pos 'org-marker str)
            (get-text-property pos 'org-hd-marker str))))))

(defun org-atomic--get-marker-from-context ()
  "Extract an Org marker from the current buffer context.
Checks text properties at point, then on the current line, and finally
falls back to `point-marker' if at an Org heading."
  (or (get-text-property (point) 'org-marker)
      (get-text-property (point) 'org-hd-marker)
      (let ((pos
             (or (text-property-not-all
                  (line-beginning-position)
                  (line-end-position)
                  'org-marker
                  nil)
                 (text-property-not-all
                  (line-beginning-position)
                  (line-end-position)
                  'org-hd-marker
                  nil))))
        (if pos
            (or (get-text-property pos 'org-marker)
                (get-text-property pos 'org-hd-marker))
          (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
            (point-marker))))))

(defun org-atomic--find-marker (&optional obj)
  "Find an Org marker from OBJ (string, marker, or nil).
If OBJ is nil, or if it is a string without marker properties, search the
current point, current line, or fallback to `point-marker' if at an Org
heading."
  (cond
   ((markerp obj)
    obj)
   ((stringp obj)
    (or (org-atomic--get-marker-from-string obj)
        (org-atomic--get-marker-from-context)))
   (t
    (org-atomic--get-marker-from-context))))

(defun org-atomic--parse-days (days-str)
  "Parse DAYS-STR into a list of day integers (1=Monday, 7=Sunday).
Accepts custom day group names from `org-atomic-day-groups',
comma/space separated day numbers, or names (e.g. \"mon,tue\" or \"Monday\")."
  (when (and days-str (not (string-empty-p (string-trim days-str))))
    (let* ((clean-str (downcase (string-trim days-str)))
           (group-match
            (cdr (assoc clean-str org-atomic-day-groups))))
      (if group-match
          group-match
        (let ((tokens (split-string clean-str "[, ]+" t)))
          (thread-last
           (mapcar
            (lambda (tok)
              (cond
               ;; Direct integer
               ((string-match-p "^[1-7]$" tok)
                (string-to-number tok))
               ;; Day name abbreviations / full names (Mon -> 1, Sun -> 7)
               (t
                (let ((day-idx
                       (cl-position
                        (substring tok 0 (min 3 (length tok)))
                        '("mon" "tue" "wed" "thu" "fri" "sat" "sun")
                        :test #'string=)))
                  (when day-idx
                    (1+ day-idx))))))
            tokens)
           (delq nil)))))))

(defun org-atomic--parse-habit (&optional marker txt)
  "Parse all ATOMIC_* properties at MARKER or in TXT.
Returns an `org-atomic-habit' plist if the entry is an atomic habit."
  (let ((resolved-marker (org-atomic--find-marker (or marker txt))))
    (when (and resolved-marker (marker-buffer resolved-marker))
      (with-current-buffer (marker-buffer resolved-marker)
        (save-excursion
          (goto-char resolved-marker)
          (let* ((props (org-entry-properties (point)))
                 (id (cdr (assoc "ATOMIC_ID" props)))
                 (days-str (cdr (assoc "ATOMIC_DAYS" props)))
                 (anchor (cdr (assoc "ATOMIC_ANCHOR" props)))
                 (type (cdr (assoc "ATOMIC_TYPE" props)))
                 (why (cdr (assoc "ATOMIC_WHY" props)))
                 (obvious (cdr (assoc "ATOMIC_OBVIOUS" props)))
                 (invisible (cdr (assoc "ATOMIC_INVISIBLE" props)))
                 (attractive (cdr (assoc "ATOMIC_ATTRACTIVE" props)))
                 (unattractive
                  (cdr (assoc "ATOMIC_UNATTRACTIVE" props)))
                 (easy (cdr (assoc "ATOMIC_EASY" props)))
                 (hard (cdr (assoc "ATOMIC_HARD" props)))
                 (satisfying (cdr (assoc "ATOMIC_SATISFYING" props)))
                 (unsatisfying
                  (cdr (assoc "ATOMIC_UNSATISFYING" props))))
            (when (or id
                      days-str
                      anchor
                      type
                      why
                      obvious
                      invisible
                      attractive
                      unattractive
                      easy
                      hard
                      satisfying
                      unsatisfying)
              (org-atomic-habit-create
               :id id
               :anchor anchor
               :days
               (when days-str
                 (org-atomic--parse-days days-str))
               :type (or type "good")
               :why why
               :obvious obvious
               :invisible invisible
               :attractive attractive
               :unattractive unattractive
               :easy easy
               :hard hard
               :satisfying satisfying
               :unsatisfying unsatisfying))))))))

(defun org-atomic--time-to-day-number (time)
  "Convert TIME to an absolute day number.
TIME can be an absolute day number (integer) or a Lisp time value."
  (if (and (integerp time) (< time 10000000))
      time
    (time-to-days time)))

(defun org-atomic--day-to-dow (day-or-cal-dow)
  "Convert DAY-OR-CAL-DOW to standard weekday index (1=Monday, 7=Sunday)."
  (let ((d
         (if (>= day-or-cal-dow 7)
             (mod day-or-cal-dow 7)
           day-or-cal-dow)))
    (if (= d 0)
        7
      d)))

(provide 'org-atomic-core)
;;; org-atomic-core.el ends here
