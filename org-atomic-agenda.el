;;; org-atomic-agenda.el --- Org Agenda integration for org-atomic  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;;
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Maintainer: Alexandr Timchenko <atimchenko92@gmail.com>
;; URL: https://github.com/tmythicator/org-atomic
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of org-atomic.
;;
;; org-atomic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; See the LICENSE file or <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains agenda view customization logic for org-atomic.
;; It hooks into `org-agenda-format-item' to display habit ID prefixes
;; and formats hierarchical stack views for anchored habits.

;;; Code:

(require 'cl-lib)
(require 'org-atomic-core)
(require 'subr-x)
(require 'org)
(require 'org-agenda)

(defgroup org-atomic-agenda nil
  "Customization group for org-atomic agenda formatting."
  :group 'org-atomic)

(defcustom org-atomic-id-format "[ %s ] "
  "Format string for the habit ID prefix.  Must contain exactly one '%s'."
  :type 'string
  :group 'org-atomic-agenda)

(defface org-atomic-id-face
  '((((background light)) (:foreground "#0d9488" :weight bold))
    (((background dark)) (:foreground "#2dd4bf" :weight bold)))
  "Face for prepended habit ID tags in the Org Agenda."
  :group 'org-atomic-agenda)

(defface org-atomic-bad-habit-face
  '((((background light)) (:foreground "#e11d48" :weight bold))
    (((background dark)) (:foreground "#fb7185" :weight bold)))
  "Face for prepended bad habit ID tags in the Org Agenda."
  :group 'org-atomic-agenda)

(defface org-atomic-branch-face
  '((((background light)) (:foreground "#b0bec5"))
    (((background dark)) (:foreground "#718096")))
  "Face for tree branch indentation in the Org Agenda."
  :group 'org-atomic-agenda)

(defun org-atomic--build-tooltip (&optional txt parsed-habit)
  "Build a tooltip summary for the habit at point, in TXT, or using PARSED-HABIT."
  (let* ((habit (or parsed-habit (org-atomic--parse-habit nil txt))))
    (when habit
      (let* ((type (org-atomic-habit-type habit))
             (is-bad
              (and type
                   (string= (downcase (string-trim type)) "bad")))
             (why (org-atomic-habit-why habit))
             (obvious (org-atomic-habit-obvious habit))
             (invisible (org-atomic-habit-invisible habit))
             (attractive (org-atomic-habit-attractive habit))
             (unattractive (org-atomic-habit-unattractive habit))
             (easy (org-atomic-habit-easy habit))
             (hard (org-atomic-habit-hard habit))
             (satisfying (org-atomic-habit-satisfying habit))
             (unsatisfying (org-atomic-habit-unsatisfying habit))
             (lines
              (thread-last
               (list
                (format "Type: %s"
                        (if is-bad
                            "Bad Habit"
                          "Good Habit"))
                (when why
                  (format "Why: %s" why))
                (if is-bad
                    (when invisible
                      (format "Make It Invisible: %s" invisible))
                  (when obvious
                    (format "Make It Obvious: %s" obvious)))
                (if is-bad
                    (when unattractive
                      (format "Make It Unattractive: %s"
                              unattractive))
                  (when attractive
                    (format "Make It Attractive: %s" attractive)))
                (if is-bad
                    (when hard
                      (format "Make It Hard: %s" hard))
                  (when easy
                    (format "Make It Easy: %s" easy)))
                (if is-bad
                    (when unsatisfying
                      (format "Make It Unsatisfying: %s"
                              unsatisfying))
                  (when satisfying
                    (format "Make It Satisfying: %s" satisfying))))
               (delq nil))))
        (when (> (length lines) 1)
          (string-join lines "\n"))))))

(defun org-atomic--find-habit-by-id (id)
  "Find the marker or position of the habit with ATOMIC_ID equal to ID."
  (let ((found-pos nil)
        (buffers
         (cons
          (current-buffer)
          (delq
           (current-buffer)
           (delq
            nil (mapcar #'find-buffer-visiting org-agenda-files))))))
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
                 (let ((val (org-entry-get (point) "ATOMIC_ID")))
                   (if (and val (string= (string-trim val) id))
                       (setq found-pos (point-marker))
                     (forward-line 1))))
               found-pos)))))
     buffers)))

(defun org-atomic--get-stack-key (marker &optional visited)
  "Get the hierarchical stack key for the habit at MARKER.
VISITED is a list of already visited IDs to prevent infinite loops."
  (when (and marker (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let ((id (org-entry-get (point) "ATOMIC_ID"))
              (anchor (org-entry-get (point) "ATOMIC_ANCHOR")))
          (when id
            (let ((id-trimmed (string-trim id)))
              (cond
               ;; Base case: no anchor or cycle
               ((or (null anchor) (member id-trimmed visited))
                id-trimmed)
               ;; Recursive case: resolve parent stack key
               (t
                (let ((parent-marker
                       (org-atomic--find-habit-by-id
                        (string-trim anchor))))
                  (if parent-marker
                      (concat
                       (org-atomic--get-stack-key parent-marker
                                                  (cons
                                                   id-trimmed
                                                   visited))
                       "/" id-trimmed)
                    ;; Parent not found, fallback to anchor name / ID
                    (concat
                     (string-trim anchor) "/" id-trimmed))))))))))))


(defun org-atomic-agenda-cmp (a b)
  "Custom comparator for sorting atomic habits in the agenda.
Compares agenda entries A and B by their hierarchical stack keys."
  (let* ((marker-a (org-atomic--find-marker a))
         (marker-b (org-atomic--find-marker b))
         (key-a
          (when marker-a
            (org-atomic--get-stack-key marker-a)))
         (key-b
          (when marker-b
            (org-atomic--get-stack-key marker-b))))
    (cond
     ((and key-a key-b)
      (cond
       ((string-lessp key-a key-b)
        -1)
       ((string-lessp key-b key-a)
        1)
       (t
        nil)))
     (key-a
      -1)
     (key-b
      1)
     (t
      nil))))

(defvar org-atomic--saved-sorting-strategy nil
  "Saved value of `org-agenda-sorting-strategy`.")

(defvar org-atomic--saved-cmp-user-defined nil
  "Saved value of `org-agenda-cmp-user-defined`.")

(defun org-atomic--enable-sorting ()
  "Enable custom agenda sorting for org-atomic."
  (setq org-atomic--saved-cmp-user-defined
        org-agenda-cmp-user-defined)
  (setq org-agenda-cmp-user-defined #'org-atomic-agenda-cmp)
  ;; Modify org-agenda-sorting-strategy
  (setq org-atomic--saved-sorting-strategy
        (copy-tree org-agenda-sorting-strategy))
  (let ((strategy (copy-tree org-agenda-sorting-strategy)))
    (dolist (item strategy)
      (let ((rules (cdr item)))
        (when (and (listp rules)
                   (not (member 'user-defined-up rules))
                   (not (member 'user-defined-down rules)))
          ;; Prepend user-defined-up to rules
          (setcdr item (cons 'user-defined-up rules)))))
    (setq org-agenda-sorting-strategy strategy)))

(defun org-atomic--disable-sorting ()
  "Disable custom agenda sorting for org-atomic."
  (when org-atomic--saved-cmp-user-defined
    (setq org-agenda-cmp-user-defined
          org-atomic--saved-cmp-user-defined)
    (setq org-atomic--saved-cmp-user-defined nil))
  (when org-atomic--saved-sorting-strategy
    (setq org-agenda-sorting-strategy
          org-atomic--saved-sorting-strategy)
    (setq org-atomic--saved-sorting-strategy nil)))


(defun org-atomic--build-prefix-str (stack-key type)
  "Build the propertized ID prefix and hierarchy branch for STACK-KEY and TYPE."
  (let* ((parts (split-string stack-key "/"))
         (len (length parts))
         (label (car (last parts)))
         (face
          (if (string= type "bad")
              'org-atomic-bad-habit-face
            'org-atomic-id-face))
         (label-str
          (propertize (format org-atomic-id-format label)
                      'face
                      face
                      'font-lock-face
                      face)))
    (if (> len 1)
        (let ((indent
               (concat (make-string (* 4 (- len 2)) ?\s) " └── ")))
          (concat
           (propertize indent
                       'face
                       'org-atomic-branch-face
                       'font-lock-face
                       'org-atomic-branch-face)
           label-str))
      label-str)))

(defun org-atomic--splice-prefix (result txt prefix-str)
  "Splice PREFIX-STR into the formatted agenda RESULT string based on TXT."
  (let* ((keywords
          (if (boundp 'org-todo-keywords-1)
              org-todo-keywords-1
            '("TODO" "DONE")))
         (todo-regexp
          (concat "^\\(" (regexp-opt keywords) "\\)\\( +\\)"))
         (has-todo (string-match todo-regexp txt))
         (body-part
          (if has-todo
              (substring txt (match-end 0))
            txt))
         (body-clean
          (org-trim
           (replace-regexp-in-string
            " +:[a-zA-Z0-9_@:]+:$" "" body-part))))
    (if (and (not (string-empty-p body-clean))
             (string-match (regexp-quote body-clean) result))
        (concat
         (substring result 0 (match-beginning 0))
         prefix-str
         (substring result (match-beginning 0)))
      (if (and has-todo (string-match todo-regexp result))
          (concat
           (substring result 0 (match-end 0))
           prefix-str
           (substring result (match-end 0)))
        result))))

(defun org-atomic--org-agenda-format-item-advice
    (orig-fun extra txt &rest args)
  "Advice to intercept `org-agenda-format-item' and prepend ID.
ORIG-FUN is the original function.  EXTRA, TXT, and ARGS are the standard
arguments."
  (let* ((marker (org-atomic--find-marker txt))
         (habit
          (when marker
            (org-atomic--parse-habit marker)))
         (stack-key
          (when marker
            (org-atomic--get-stack-key marker)))
         (result (apply orig-fun extra txt args)))
    (when result
      (let* ((has-stack
              (and stack-key
                   (not (string-empty-p (string-trim stack-key)))))
             (type
              (if habit
                  (org-atomic-habit-type habit)
                "good"))
             (formatted
              (if has-stack
                  (org-atomic--splice-prefix
                   result
                   txt
                   (org-atomic--build-prefix-str stack-key type))
                result))
             (tooltip
              (when habit
                (org-atomic--build-tooltip nil habit))))
        (if tooltip
            (propertize formatted
                        'org-atomic-tooltip
                        tooltip
                        'help-echo
                        tooltip)
          formatted)))))

;;;###autoload
(defun org-atomic-agenda-finalize-faces ()
  "Restore org-atomic faces in the agenda buffer.
This runs after `org-agenda' has finished styling the entries."
  (save-excursion
    (save-restriction
      (widen)
      ;; 1. Restore face properties
      (goto-char (point-min))
      (let ((pos (point)))
        (while (< pos (point-max))
          (let* ((next
                  (next-single-property-change pos 'font-lock-face
                                               nil (point-max)))
                 (fl-face (get-text-property pos 'font-lock-face)))
            (when (memq
                   fl-face
                   '(org-atomic-id-face
                     org-atomic-bad-habit-face
                     org-atomic-branch-face))
              (let ((current-face (get-text-property pos 'face)))
                (put-text-property
                 pos next 'face
                 (if (listp current-face)
                     (cons fl-face (delq fl-face current-face))
                   (list fl-face current-face)))))
            (setq pos next))))
      ;; 2. Restore/merge help-echo tooltips
      (goto-char (point-min))
      (let ((pos (point)))
        (while (< pos (point-max))
          (let* ((next
                  (next-single-property-change pos 'org-atomic-tooltip
                                               nil (point-max)))
                 (tooltip
                  (get-text-property pos 'org-atomic-tooltip)))
            (when tooltip
              (let ((current-echo (get-text-property pos 'help-echo)))
                (put-text-property
                 pos next 'help-echo
                 (if (and current-echo
                          (not
                           (string-prefix-p tooltip current-echo)))
                     (concat tooltip "\n---\n" current-echo)
                   tooltip))))
            (setq pos next)))))))

(provide 'org-atomic-agenda)
;;; org-atomic-agenda.el ends here
