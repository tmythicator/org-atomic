;;; org-atomic-core.el --- Core domain structures and utilities for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.1.0
;; License: GPL-3.0-or-later

;;; Commentary:
;; Core structures, plist definitions, and property parsing for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'org)
(require 'org-atomic-util)

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
          :next nil)))
    (while args
      (let ((key (pop args))
            (val (pop args)))
        (plist-put defaults key val)))
    defaults))

;; Plist accessors
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

(defconst org-atomic--property-mapping
  '(("ATOMIC_ID"           . :id)
    ("ATOMIC_NEXT"         . :next)
    ("ATOMIC_DAYS"         . :days)
    ("ATOMIC_TYPE"         . :type)
    ("ATOMIC_WHY"          . :why)
    ("ATOMIC_OBVIOUS"      . :obvious)
    ("ATOMIC_INVISIBLE"    . :invisible)
    ("ATOMIC_ATTRACTIVE"   . :attractive)
    ("ATOMIC_UNATTRACTIVE" . :unattractive)
    ("ATOMIC_EASY"         . :easy)
    ("ATOMIC_HARD"         . :hard)
    ("ATOMIC_SATISFYING"   . :satisfying)
    ("ATOMIC_UNSATISFYING" . :unsatisfying))
  "Mapping from Org properties to habit plist keys.")

(defun org-atomic--parse-habit (&optional marker txt)
  "Parse all ATOMIC_* properties at MARKER or in TXT.
Returns an `org-atomic-habit' plist if the entry is an atomic habit."
  (let ((resolved-marker
         (org-atomic-util--find-marker (or marker txt))))
    (when (and resolved-marker (marker-buffer resolved-marker))
      (with-current-buffer (marker-buffer resolved-marker)
        (save-excursion
          (goto-char resolved-marker)
          (let* ((props (org-entry-properties (point)))
                 (habit-args nil)
                 (has-any nil))
            (dolist (map org-atomic--property-mapping)
              (let* ((prop-name (car map))
                     (plist-key (cdr map))
                     (val (cdr (assoc prop-name props))))
                (when val
                  (setq has-any t)
                  (cond
                   ((eq plist-key :days)
                    (setq val (org-atomic-util--parse-days val)))
                   ((and (eq plist-key :type)
                         (string-empty-p (string-trim val)))
                    (setq val "good")))
                  (setq habit-args (plist-put habit-args plist-key val)))))
            (when has-any
              (unless (plist-get habit-args :type)
                (setq habit-args (plist-put habit-args :type "good")))
              (apply #'org-atomic-habit-create habit-args))))))))

;;; Caches

(defvar org-atomic--find-id-cache (make-hash-table :test 'equal)
  "Cache for `org-atomic--find-habit-by-id`.")

(defvar org-atomic--find-next-cache (make-hash-table :test 'equal)
  "Cache for `org-atomic--find-predecessor-by-next-id`.")

(defvar org-atomic--time-cache (make-hash-table :test 'equal)
  "Cache for `org-atomic--get-time-at-marker`.")

(defun org-atomic-clear-caches ()
  "Clear all org-atomic caches."
  (interactive)
  (clrhash org-atomic--find-id-cache)
  (clrhash org-atomic--find-next-cache)
  (clrhash org-atomic--time-cache))

;;; Heading Search & Hierarchy Utilities

(defun org-atomic--find-heading-by-property
    (property value &optional cache)
  "Find the marker of the heading where PROPERTY equals VALUE.
Uses CACHE (a hash table) if provided."
  (if (and cache
           (let ((cached (gethash value cache 'not-found)))
             (not (eq cached 'not-found))))
      (gethash value cache)
    (let ((found-pos nil)
          (buffers
           (cons
            (current-buffer)
            (delq
             (current-buffer)
             (delq
              nil
              (mapcar #'find-buffer-visiting org-agenda-files))))))
      (setq found-pos
            (cl-some
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (while (and (not found-pos)
                                   (re-search-forward "^\\*+ " nil t))
                         (let ((val (org-entry-get (point) property)))
                           (if (and val
                                    (string= (string-trim val) value))
                               (setq found-pos (point-marker))
                             (forward-line 1))))
                       found-pos)))))
             buffers))
      (when cache
        (puthash value found-pos cache))
      found-pos)))

(defun org-atomic--find-habit-by-id (id)
  "Find the marker of the habit with ATOMIC_ID equal to ID."
  (org-atomic--find-heading-by-property "ATOMIC_ID" id
                                        org-atomic--find-id-cache))

(defun org-atomic--find-predecessor-by-next-id (id)
  "Find the marker of the habit that has ATOMIC_NEXT equal to ID."
  (org-atomic--find-heading-by-property "ATOMIC_NEXT" id
                                        org-atomic--find-next-cache))

(defun org-atomic--get-time-at-marker (marker)
  "Get the time of day (integer HHMM) for the habit at MARKER."
  (when (and marker (marker-buffer marker))
    (let* ((cache-key
            (format "%s:%d"
                    (buffer-name (marker-buffer marker))
                    (marker-position marker)))
           (cached
            (gethash cache-key org-atomic--time-cache 'not-found)))
      (if (not (eq cached 'not-found))
          cached
        (let ((time-val
               (with-current-buffer (marker-buffer marker)
                 (save-excursion
                   (goto-char marker)
                   (let ((headline (org-get-heading t t t t))
                         (scheduled
                          (org-entry-get (point) "SCHEDULED")))
                     (or (org-atomic-util--parse-time-str-to-int
                          headline)
                         (org-atomic-util--parse-time-str-to-int
                          scheduled)))))))
          (puthash cache-key time-val org-atomic--time-cache)
          time-val)))))

(defun org-atomic--get-effective-time (item key marker)
  "Get the effective time of day for ITEM at MARKER with stack KEY."
  (or (get-text-property 0 'time-of-day item)
      (when marker
        (or (org-atomic--get-time-at-marker marker)
            (when key
              (let* ((parts (split-string key "/"))
                     (root-id (car parts))
                     (root-marker
                      (org-atomic--find-habit-by-id root-id)))
                (when root-marker
                  (org-atomic--get-time-at-marker root-marker))))))))

(defun org-atomic--get-stack-key (marker &optional visited)
  "Get the hierarchical stack key for the habit at MARKER.
VISITED is a list of already visited IDs to prevent infinite loops."
  (when (and marker (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let ((id (org-entry-get (point) "ATOMIC_ID")))
          (when id
            (let* ((id-trimmed (string-trim id))
                   (pred-marker
                    (unless (member id-trimmed visited)
                      (org-atomic--find-predecessor-by-next-id
                       id-trimmed))))
              (cond
               ;; Base case: no predecessor or cycle
               ((or (null pred-marker) (member id-trimmed visited))
                id-trimmed)
               ;; Recursive case: resolve parent stack key
               (t
                (concat
                 (org-atomic--get-stack-key pred-marker
                                            (cons id-trimmed visited))
                 "/" id-trimmed))))))))))

(provide 'org-atomic-core)
;;; org-atomic-core.el ends here
