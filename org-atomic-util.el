;;; org-atomic-util.el --- Utility helper functions for org-atomic  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.0.1
;; License: GPL-3.0-or-later

;;; Commentary:
;; Shared pure utility functions and parsing helpers for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'org)

;;; Constants & Regular Expressions

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

;;; Core Utilities

(defun org-atomic-util--get-marker-from-string (str)
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

(defun org-atomic-util--get-marker-from-context ()
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

(defun org-atomic-util--parse-days (days-str)
  "Parse DAYS-STR into a list of day integers (1=Monday, 7=Sunday).
Accepts custom day group names from `org-atomic-day-groups',
comma/space separated day numbers, or names (e.g. \"mon,tue\" or \"Monday\")."
  (when (and days-str (not (string-empty-p (string-trim days-str))))
    (let* ((clean-str (downcase (string-trim days-str)))
           (group-match
            (cdr
             (assoc
              clean-str
              (and (boundp 'org-atomic-day-groups)
                   org-atomic-day-groups)))))
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

(defun org-atomic-util--parse-time-str-to-int (str)
  "Parse a time string (HH:MM) from STR into an integer HHMM."
  (when (and str
             (string-match
              "\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)" str))
    (+ (* (string-to-number (match-string 1 str)) 100)
       (string-to-number (match-string 2 str)))))

(defun org-atomic-util--clean-headline-text (text)
  "Clean Org metadata, tags, timestamps, and time ranges from TEXT."
  (let ((ts-regex
         (if (boundp 'org-ts-regexp-both)
             org-ts-regexp-both
           "")))
    (thread-last
     text
     (replace-regexp-in-string " +:[a-zA-Z0-9_@:]+:$" "")
     (replace-regexp-in-string ts-regex "")
     (replace-regexp-in-string org-atomic-util--time-range-bracket-regexp "")
     (replace-regexp-in-string org-atomic-util--time-range-angle-regexp "")
     (org-trim))))

(defun org-atomic-util--clean-result-time (str)
  "Remove bracketed or angled time ranges and their remnants from STR."
  (thread-last
   str
   (replace-regexp-in-string
    (concat "[ \t]*" org-atomic-util--time-range-bracket-regexp) "")
   (replace-regexp-in-string
    (concat "[ \t]*" org-atomic-util--time-duration-bracket-regexp) "")
   (replace-regexp-in-string
    (concat "[ \t]*" org-atomic-util--time-range-angle-regexp) "")
   (replace-regexp-in-string
    (concat "[ \t]*" org-atomic-util--time-duration-angle-regexp) "")))

(defun org-atomic-util--cmp-number-with-nil (a b)
  "Compare numbers A and B, treating nil as larger than any number.
Returns -1 if A < B, 1 if A > B, or nil if they are equal or both nil."
  (cond
   ((and a b)
    (cond
     ((< a b)
      -1)
     ((> a b)
      1)
     (t
      nil)))
   (a
    -1)
   (b
    1)
   (t
    nil)))

(provide 'org-atomic-util)
;;; org-atomic-util.el ends here
