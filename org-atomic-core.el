;;; org-atomic-core.el --- Core logic and domain structures for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.3.1
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; URL: https://github.com/tmythicator/org-atomic
;; License: GPL-3.0-or-later

;;; Commentary:
;; Core structures, plist definitions, and property parsing for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'org)
(require 'seq)
(require 'org-atomic-util)

(defgroup org-atomic-core nil
  "Core logic and settings for org-atomic."
  :group 'org-atomic)

(defcustom org-atomic-core-day-groups
  '(("workdays" 1 2 3 4 5) ("weekends" 6 7) ("daily" 1 2 3 4 5 6 7))
  "Alist of day group names and their active weekdays (1=Monday, 7=Sunday)."
  :type '(alist :key-type string :value-type (repeat integer))
  :group 'org-atomic-core)

(defcustom org-atomic-core-excluded-logbook-states
  '("CANCELED" "CANCELLED" "SKIPPED" "FAILED")
  "List of Org TODO keywords that should NOT be counted as completions.
These states represent canceled, skipped, or failed attempts rather
than successful habit executions."
  :type '(repeat string)
  :group 'org-atomic-core)


(cl-defstruct
 (org-atomic-habit
  (:constructor org-atomic-core-habit-create)
  (:conc-name org-atomic-core-habit-)
  (:copier nil))
 "Structure representing an atomic habit."
 id
 obvious
 attractive
 easy
 satisfying
 why
 invisible
 unattractive
 hard
 unsatisfying
 (type "good")
 days
 next)

(defconst org-atomic-core--property-mapping
  '(("ATOMIC_ID" . :id)
    ("ATOMIC_NEXT" . :next)
    ("ATOMIC_DAYS" . :days)
    ("ATOMIC_TYPE" . :type)
    ("ATOMIC_WHY" . :why)
    ("ATOMIC_OBVIOUS" . :obvious)
    ("ATOMIC_INVISIBLE" . :invisible)
    ("ATOMIC_ATTRACTIVE" . :attractive)
    ("ATOMIC_UNATTRACTIVE" . :unattractive)
    ("ATOMIC_EASY" . :easy)
    ("ATOMIC_HARD" . :hard)
    ("ATOMIC_SATISFYING" . :satisfying)
    ("ATOMIC_UNSATISFYING" . :unsatisfying))
  "Mapping from Org properties to habit plist keys.")

(defun org-atomic-core--parse-property-mapping (mapping props)
  "Pure function: parse a single MAPPING against PROPS alist."
  (pcase-let* ((`(,prop-name . ,slot) mapping)
               (val (cdr (assoc prop-name props))))
    (when (and val (not (string-empty-p (string-trim val))))
      (list
       slot
       (if (eq slot :days)
           (org-atomic-util--parse-days val)
         (string-trim val))))))

(defun org-atomic-core-parse-habit (&optional marker txt)
  "Parse all ATOMIC_* properties at MARKER or in TXT.
Returns an `org-atomic-habit' plist if the entry is an atomic habit."
  (when-let* ((resolved-marker
               (org-atomic-util--find-marker (or marker txt))))
    (org-atomic-util-with-heading-at-marker
     resolved-marker
     (let* ((props (org-entry-properties (point)))
            (args
             (seq-mapcat
              (lambda (mapping)
                (org-atomic-core--parse-property-mapping
                 mapping props))
              org-atomic-core--property-mapping)))
       (when args
         (apply #'org-atomic-core-habit-create
                (if (plist-member args :type)
                    args
                  (append '(:type "good") args))))))))

;;; Caches

(defvar org-atomic-core--find-id-cache (make-hash-table :test 'equal)
  "Cache for `org-atomic-core--find-habit-by-id`.")

(defvar org-atomic-core--find-next-cache
  (make-hash-table :test 'equal)
  "Cache for `org-atomic-core--find-predecessor-by-next-id`.")

(defvar org-atomic-core--time-cache (make-hash-table :test 'equal)
  "Cache for `org-atomic-core--get-time-at-marker`.")

(defun org-atomic-core-clear-caches ()
  "Clear all org-atomic-core caches."
  (interactive)
  (clrhash org-atomic-core--find-id-cache)
  (clrhash org-atomic-core--find-next-cache)
  (clrhash org-atomic-core--time-cache))

;;; Heading Search & Hierarchy Utilities

(defun org-atomic-core--scan-buffer-for-property (property value)
  "Scan the current widened buffer for a heading where PROPERTY matches VALUE.
Returns the `point-marker' if found, otherwise nil."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward org-atomic-util-headline-regexp
                                  nil
                                  t)
          (let ((val (org-entry-get (point) property)))
            (when (and val (string= (string-trim val) value))
              (throw 'found (point-marker)))))))))

(defun org-atomic-core--find-heading-by-property
    (property value &optional cache)
  "Find the marker of the heading where PROPERTY equals VALUE.
Uses CACHE (a hash table) if provided."
  (if (and cache (gethash value cache))
      (gethash value cache)
    (let* ((value-trimmed (string-trim value))
           (buffers
            (cons
             (current-buffer)
             (delq
              (current-buffer)
              (delq
               nil
               (mapcar #'find-buffer-visiting org-agenda-files)))))
           (found-marker
            (cl-some
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (org-atomic-core--scan-buffer-for-property
                    property value-trimmed))))
             buffers)))
      (when (and cache found-marker)
        (puthash value found-marker cache))
      found-marker)))

(defun org-atomic-core--find-habit-by-id (id)
  "Find the marker of the habit with ATOMIC_ID equal to ID."
  (org-atomic-core--find-heading-by-property
   "ATOMIC_ID" id
   org-atomic-core--find-id-cache))

(defun org-atomic-core--find-predecessor-by-next-id (id)
  "Find the marker of the habit that has ATOMIC_NEXT equal to ID."
  (org-atomic-core--find-heading-by-property
   "ATOMIC_NEXT" id
   org-atomic-core--find-next-cache))

(defun org-atomic-core--get-time-at-marker (marker)
  "Get the time of day (integer HHMM) for the habit at MARKER."
  (when (and marker (marker-buffer marker))
    (let* ((cache-key
            (format "%s:%d"
                    (buffer-name (marker-buffer marker))
                    (marker-position marker)))
           (cached (gethash cache-key org-atomic-core--time-cache)))
      (or cached
          (let ((time-val
                 (org-atomic-util-with-heading-at-marker
                  marker
                  (let ((headline (org-get-heading t t t t))
                        (scheduled
                         (org-entry-get (point) "SCHEDULED")))
                    (or (org-atomic-util--parse-time-str-to-int
                         headline)
                        (org-atomic-util--parse-time-str-to-int
                         scheduled))))))
            (puthash cache-key time-val org-atomic-core--time-cache)
            time-val)))))

(defun org-atomic-core--get-effective-time (item key marker)
  "Get the effective time of day for ITEM at MARKER with stack KEY."
  (or (get-text-property 0 'time-of-day item)
      (when marker
        (or (org-atomic-core--get-time-at-marker marker)
            (when key
              (let* ((parts (split-string key "/"))
                     (root-id (car parts))
                     (root-marker
                      (org-atomic-core--find-habit-by-id root-id)))
                (when root-marker
                  (org-atomic-core--get-time-at-marker
                   root-marker))))))))

(defun org-atomic-core--get-stack-key (marker &optional visited)
  "Get the hierarchical stack key for the habit at MARKER.
VISITED is a list of already visited IDs to prevent infinite loops."
  (org-atomic-util-with-heading-at-marker
   marker
   (let ((id (org-entry-get (point) "ATOMIC_ID")))
     (when id
       (let* ((id-trimmed (string-trim id))
              (pred-marker
               (unless (member id-trimmed visited)
                 (org-atomic-core--find-predecessor-by-next-id
                  id-trimmed))))
         (cond
          ;; Base case: no predecessor or cycle
          ((or (null pred-marker) (member id-trimmed visited))
           id-trimmed)
          ;; Recursive case: resolve parent stack key
          (t
           (concat
            (org-atomic-core--get-stack-key pred-marker
                                            (cons id-trimmed visited))
            "/" id-trimmed))))))))

(defun org-atomic-core-habit-bad-p (habit)
  "Return non-nil if HABIT is configured as a bad habit."
  (let ((type (org-atomic-core-habit-type habit)))
    (and type (string= (downcase (string-trim type)) "bad"))))

(defun org-atomic-core-habit-success-p (habit done-p)
  "Return non-nil if HABIT is successful given its completion status DONE-P."
  (if (org-atomic-core-habit-bad-p habit)
      (not done-p)
    done-p))

(defun org-atomic-core-habit-strategies (habit)
  "Return an alist of active strategy labels and values for HABIT."
  (let ((is-bad (org-atomic-core-habit-bad-p habit)))
    (thread-last
     (if is-bad
         `(("Invisible" . ,(org-atomic-core-habit-invisible habit))
           ("Unattractive"
            .
            ,(org-atomic-core-habit-unattractive habit))
           ("Hard" . ,(org-atomic-core-habit-hard habit))
           ("Unsatisfying"
            .
            ,(org-atomic-core-habit-unsatisfying habit)))
       `(("Obvious" . ,(org-atomic-core-habit-obvious habit))
         ("Attractive" . ,(org-atomic-core-habit-attractive habit))
         ("Easy" . ,(org-atomic-core-habit-easy habit))
         ("Satisfying" . ,(org-atomic-core-habit-satisfying habit))))
     (seq-filter
      (pcase-lambda (`(,_ . ,val))
        (and val (not (string-empty-p (string-trim val)))))))))

(provide 'org-atomic-core)
;;; org-atomic-core.el ends here
