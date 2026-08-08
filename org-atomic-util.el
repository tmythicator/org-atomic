;;; org-atomic-util.el --- Utility helper functions for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.3.1
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; URL: https://github.com/tmythicator/org-atomic
;; License: GPL-3.0-or-later

;;; Commentary:
;; Shared pure utility functions and parsing helpers for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'pcase)
(require 'calendar)
(require 'org)

;;; ============================================================================
;;; Constants & Regular Expressions
;;; ============================================================================

(defconst org-atomic-util-repeater-regexp "\\([.+]?\\+[0-9]+[dwmy]\\)"
  "Regexp matching Org repeater specifications.
Examples: +1d, ++1d, .+1d.")

(defconst org-atomic-util--time-range-bracket-regexp
  "\\[[0-9]\\{1,2\\}:[0-9]\\{2\\}\\(?: *-[ *][0-9]\\{1,2\\}:[0-9]\\{2\\}\\)?\\]"
  "Regexp matching bracketed time ranges.
Examples: [07:30 - 08:30] or [7:30].")

(defconst org-atomic-util--time-range-angle-regexp
  "<[0-9]\\{1,2\\}:[0-9]\\{2\\}\\(?: *-[ *][0-9]\\{1,2\\}:[0-9]\\{2\\}\\)?>"
  "Regexp matching angled time ranges.
Examples: <07:30 - 08:30> or <7:30>.")

(defconst org-atomic-util--time-duration-bracket-regexp
  "\\[-[ *][0-9]\\{1,2\\}:[0-9]\\{2\\}\\]"
  "Regexp matching bracketed duration/relative times.
Examples: [-08:30] or [- 8:30].")

(defconst org-atomic-util--time-duration-angle-regexp
  "<-[ *][0-9]\\{1,2\\}:[0-9]\\{2\\}>"
  "Regexp matching angled duration/relative times.
Examples: <-08:30> or <- 8:30>.")

(defconst org-atomic-util--day-digit-regexp "^[1-7]$"
  "Regexp matching a single day number from 1 to 7.")

(defconst org-atomic-util--day-separator-regexp "[, ]+"
  "Regexp matching day string separators (commas and spaces).")

(defconst org-atomic-util--time-regexp
  "\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)"
  "Regexp matching time strings in HH:MM format.")

(defconst org-atomic-util--tags-regexp " +:[a-zA-Z0-9_@:]+:$"
  "Regexp matching Org headline tags at the end of a line.")

(defconst org-atomic-util-logbook-state-regexp
  "- State \"\\([^\"]+\\)\"[ \t]+from[ \t]+\"\\([^\"]+\\)\"[ \t]+\\(\\[[^]]+\\]\\|\\(?:<[^>]+>\\)\\)"
  "Regexp matching Org logbook state changes.")

(defconst org-atomic-util-headline-regexp "^\\*+ "
  "Regexp matching Org headline stars.")

(defconst org-atomic-util-weekday-names
  '("mon" "tue" "wed" "thu" "fri" "sat" "sun")
  "List of expected weekday abbreviations, starting from Monday.")

(defconst org-atomic-util-marker-properties
  '(org-marker org-hd-marker)
  "Text properties used by Org-mode to store markers.")


;;; ============================================================================
;;; Marker Extraction
;;; ============================================================================

(defun org-atomic-util--get-marker-from-string (str)
  "Extract an Org marker from the text properties of STR."
  (when (stringp str)
    (seq-some
     (lambda (prop)
       (let ((pos
              (text-property-not-all 0 (length str) prop nil str)))
         (when pos
           (get-text-property pos prop str))))
     org-atomic-util-marker-properties)))

(defun org-atomic-util--get-marker-from-context ()
  "Extract an Org marker from the current buffer context.
Checks text properties at point, then on the current line, and finally
falls back to `point-marker' if at an Org heading."
  (or (seq-some
       (lambda (prop) (get-text-property (point) prop))
       org-atomic-util-marker-properties)
      (seq-some
       (lambda (prop)
         (let ((pos
                (text-property-not-all
                 (line-beginning-position)
                 (line-end-position)
                 prop
                 nil)))
           (when pos
             (get-text-property pos prop))))
       org-atomic-util-marker-properties)
      (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
        (point-marker))))

(defun org-atomic-util--find-marker (&optional obj)
  "Find an Org marker from OBJ (string, marker, or nil).
If OBJ is nil, or if it is a string without marker properties, search the
current point, current line, or fallback to `point-marker' if at an Org
heading."
  (cond
   ((markerp obj)
    obj)
   ((stringp obj)
    (or (org-atomic-util--get-marker-from-string obj)
        (org-atomic-util--get-marker-from-context)))
   (t
    (org-atomic-util--get-marker-from-context))))

(defun org-atomic-util-walk-property-intervals
    (prop fn &optional start end)
  "Iterate over intervals of text property PROP from START to END, calling FN.
FN is called with (START-POS END-POS PROPERTY-VALUE) for each non-nil interval."
  (let ((pos (or start (point-min)))
        (limit (or end (point-max))))
    (while (< pos limit)
      (let ((next (next-single-property-change pos prop nil limit))
            (val (get-text-property pos prop)))
        (when val
          (funcall fn pos next val))
        (setq pos next)))))


;;; ============================================================================
;;; Heading Resolution & Context
;;; ============================================================================

(defun org-atomic-util--goto-heading-at-point ()
  "Move point to the beginning of the current heading safely.
If point is already at a heading, do nothing.
Otherwise, try to find the heading using `org-back-to-heading',
and fallback to a regex search backward if that fails."
  (unless (and (derived-mode-p 'org-mode) (org-at-heading-p))
    (unless (ignore-errors
              (org-back-to-heading t))
      (let ((re
             (if (boundp 'org-outline-regexp-bol)
                 org-outline-regexp-bol
               org-atomic-util-headline-regexp)))
        (re-search-backward re nil t)))))

(defmacro org-atomic-util-with-heading-at-marker (marker &rest body)
  "Execute BODY with MARKER's buffer current, widened, and point at its heading."
  (declare (indent 1) (debug t))
  (let ((m (make-symbol "marker"))
        (buf (make-symbol "buf")))
    `(let* ((,m ,marker)
            (,buf (and ,m (marker-buffer ,m))))
       (when ,buf
         (with-current-buffer ,buf
           (save-restriction
             (widen)
             (save-excursion
               (goto-char ,m)
               (org-atomic-util--goto-heading-at-point)
               ,@body)))))))

;;; ============================================================================
;;; Day & Time Parsing
;;; ============================================================================

(defun org-atomic-util--token-to-dow (tok)
  "Parse day token TOK into a weekday integer (1=Monday, 7=Sunday).
Return nil if TOK does not match a valid day number or name."
  (cond
   ;; Direct integer
   ((string-match-p org-atomic-util--day-digit-regexp tok)
    (string-to-number tok))
   ;; Day name abbreviations / full names (Mon -> 1, Sun -> 7)
   (t
    (let ((day-idx
           (cl-position
            (substring tok 0 (min 3 (length tok)))
            org-atomic-util-weekday-names
            :test #'string=)))
      (when day-idx
        (1+ day-idx))))))

(defun org-atomic-util--parse-days (days-str)
  "Parse DAYS-STR into a list of day integers (1=Monday, 7=Sunday).
Accepts custom day group names from `org-atomic-day-groups',
comma/space separated day numbers, or names (e.g. \"mon,tue\" or \"Monday\")."
  (when-let* ((trimmed (and days-str (string-trim days-str)))
              ((not (string-empty-p trimmed))))
    (let* ((clean-str (downcase trimmed))
           (group-match
            (cdr
             (assoc
              clean-str
              (and (boundp 'org-atomic-core-day-groups)
                   org-atomic-core-day-groups)))))
      (or group-match
          (thread-last
           (split-string clean-str
                         org-atomic-util--day-separator-regexp t)
           (seq-map #'org-atomic-util--token-to-dow) (delq nil))))))

(defun org-atomic-util--parse-time-str-to-int (str)
  "Parse a time string (HH:MM) from STR into an integer HHMM."
  (when (and str (string-match org-atomic-util--time-regexp str))
    (+ (* (string-to-number (match-string 1 str)) 100)
       (string-to-number (match-string 2 str)))))


;;; ============================================================================
;;; Date & Calendar Math
;;; ============================================================================

(defun org-atomic-util--time-to-day-number (time)
  "Convert TIME to an absolute day number.
TIME can be an absolute day number (integer) or a Lisp time value."
  (if (and (integerp time) (< time 10000000))
      time
    (time-to-days time)))

(defun org-atomic-util--day-to-dow (day-or-cal-dow)
  "Convert DAY-OR-CAL-DOW to standard weekday index (1=Monday, 7=Sunday)."
  (let ((d
         (if (>= day-or-cal-dow 7)
             (mod day-or-cal-dow 7)
           day-or-cal-dow)))
    (if (= d 0)
        7
      d)))


;;; ============================================================================
;;; Text Cleaning & Formatting
;;; ============================================================================

(defun org-atomic-util--clean-headline-text (text)
  "Clean Org metadata, tags, timestamps, and time ranges from TEXT."
  (let ((ts-regex
         (if (boundp 'org-ts-regexp-both)
             org-ts-regexp-both
           "")))
    (thread-last
     text
     (replace-regexp-in-string org-atomic-util--tags-regexp "")
     (replace-regexp-in-string ts-regex "")
     (replace-regexp-in-string
      org-atomic-util--time-range-bracket-regexp "")
     (replace-regexp-in-string
      org-atomic-util--time-range-angle-regexp "")
     (org-trim))))

(defconst org-atomic-util--time-cleanup-patterns
  (list org-atomic-util--time-range-bracket-regexp
        org-atomic-util--time-duration-bracket-regexp
        org-atomic-util--time-range-angle-regexp
        org-atomic-util--time-duration-angle-regexp)
  "List of regexes matching time ranges and duration expressions.")

(defun org-atomic-util--clean-result-time (str)
  "Remove bracketed or angled time ranges and their remnants from STR."
  (seq-reduce
   (lambda (acc pat)
     (replace-regexp-in-string (concat "[ \t]*" pat) "" acc))
   org-atomic-util--time-cleanup-patterns
   str))


;;; ============================================================================
;;; Comparison Helpers
;;; ============================================================================

(defun org-atomic-util--cmp-number-with-nil (a b)
  "Compare numbers A and B, treating nil as larger than any number.
Returns -1 if A < B, 1 if A > B, or nil if they are equal or both nil."
  (pcase (cons a b)
    (`(nil . nil) nil)
    (`(,_ . nil) -1)
    (`(nil . ,_) 1)
    (`(,na . ,nb)
     (cond
      ((< na nb)
       -1)
      ((> na nb)
       1)
      (t
       nil)))))

(defun org-atomic-util-calculate-percentage
    (success-count active-count)
  "Calculate the percentage of SUCCESS-COUNT out of ACTIVE-COUNT.
Return 0 if ACTIVE-COUNT is 0."
  (if (> active-count 0)
      (round (* 100 (/ success-count (float active-count))))
    0))

(provide 'org-atomic-util)
;;; org-atomic-util.el ends here
