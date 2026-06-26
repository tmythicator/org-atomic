;;; org-atomic-agenda.el --- Org Agenda integration for org-atomic  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.1.0
;; License: GPL-3.0-or-later

;;; Commentary:
;; Org Agenda integration, styling, and stack formatting for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'org-atomic-core)
(require 'subr-x)
(require 'org)
(require 'org-agenda)
(require 'org-atomic-util)

(defgroup org-atomic-agenda nil
  "Customization group for org-atomic agenda formatting."
  :group 'org-atomic)

(defcustom org-atomic-id-format "[ %s ] "
  "Format string for the habit ID prefix.  Must contain exactly one '%s'."
  :type 'string
  :group 'org-atomic-agenda)

(defcustom org-atomic-branch-prefix " └── "
  "Prefix used for stacked branch indentation in the Org Agenda."
  :type 'string
  :group 'org-atomic-agenda)

(defface org-atomic-id-face
  '((((background light))
     (:foreground "#0d9488" :weight bold :inherit fixed-pitch))
    (((background dark))
     (:foreground "#2dd4bf" :weight bold :inherit fixed-pitch)))
  "Face for prepended habit ID tags in the Org Agenda."
  :group 'org-atomic-agenda)

(defface org-atomic-bad-habit-face
  '((((background light))
     (:foreground "#e11d48" :weight bold :inherit fixed-pitch))
    (((background dark))
     (:foreground "#fb7185" :weight bold :inherit fixed-pitch)))
  "Face for prepended bad habit ID tags in the Org Agenda."
  :group 'org-atomic-agenda)

(defface org-atomic-branch-face
  '((((background light))
     (:foreground "#b0bec5" :inherit fixed-pitch))
    (((background dark))
     (:foreground "#718096" :inherit fixed-pitch)))
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

(defun org-atomic-agenda-cmp (a b)
  "Custom comparator for sorting atomic habits in the agenda.
Compares agenda entries A and B by time, then stacks them together."
  (let* ((marker-a (org-atomic-util--find-marker a))
         (marker-b (org-atomic-util--find-marker b))
         (key-a
          (when marker-a
            (org-atomic--get-stack-key marker-a)))
         (key-b
          (when marker-b
            (org-atomic--get-stack-key marker-b)))
         (time-a (org-atomic--get-effective-time a key-a marker-a))
         (time-b (org-atomic--get-effective-time b key-b marker-b)))
    (or (org-atomic-util--cmp-number-with-nil time-a time-b)
        (when (and key-a key-b)
          (let ((root-a (car (split-string key-a "/")))
                (root-b (car (split-string key-b "/"))))
            (when (string= root-a root-b)
              (cond
               ((string-lessp key-a key-b)
                -1)
               ((string-lessp key-b key-a)
                1))))))))

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
  "Build the propertized ID prefix and hierarchy branch for STACK-KEY and TYPE.
Returns a cons cell (INDENT-STR . LABEL-STR)."
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
                      face))
         (indent-str
          (if (> len 1)
              (propertize (concat
                           (make-string (* 4 (- len 2)) ?\s)
                           org-atomic-branch-prefix)
                          'face
                          'org-atomic-branch-face
                          'font-lock-face
                          'org-atomic-branch-face)
            "")))
    (cons indent-str label-str)))

(defun org-atomic--splice-prefix (result txt indent-str id-str)
  "Splice INDENT-STR and ID-STR into RESULT based on TXT.
RESULT is the formatted agenda string.  INDENT-STR is the stacked
habit indentation, and ID-STR is the prepended habit identifier."
  (let* ((keywords
          (if (boundp 'org-todo-keywords-1)
              org-todo-keywords-1
            '("TODO" "DONE")))
         (todo-regexp
          (concat "^\\(" (regexp-opt keywords) "\\)\\( +\\)"))
         (todo-regexp-no-anchor
          (concat "\\<\\(" (regexp-opt keywords) "\\)\\>\\( +\\)"))
         (has-todo (string-match todo-regexp txt))
         (body-part
          (if has-todo
              (substring txt (match-end 0))
            txt))
         (body-clean
          (org-atomic-util--clean-headline-text body-part)))
    (cond
     ((string-match todo-regexp-no-anchor result)
      (let ((todo-start (match-beginning 1))
            (todo-end (match-end 2)))
        (concat
         (substring result 0 todo-start)
         (or indent-str "")
         (substring result todo-start todo-end)
         (or id-str "")
         (substring result todo-end))))
     ((and (not (string-empty-p body-clean))
           (string-match (regexp-quote body-clean) result))
      (let ((body-start (match-beginning 0)))
        (concat
         (substring result 0 body-start)
         (or indent-str "")
         (or id-str "")
         (substring result body-start))))
     (t
      (concat result (or indent-str "") (or id-str ""))))))

(defun org-atomic--org-agenda-format-item-advice
    (orig-fun extra txt &rest args)
  "Advice to intercept `org-agenda-format-item' and prepend ID.
ORIG-FUN is the original function.  EXTRA, TXT, and ARGS are the standard
arguments."
  (let* ((marker (org-atomic-util--find-marker txt))
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
              (let ((cleaned
                     (if habit
                         (org-atomic-util--clean-result-time result)
                       result)))
                (if has-stack
                    (let ((prefix-pair
                           (org-atomic--build-prefix-str
                            stack-key type)))
                      (org-atomic--splice-prefix
                       cleaned
                       txt
                       (car prefix-pair)
                       (cdr prefix-pair)))
                  cleaned)))
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
  (org-atomic-clear-caches)
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
