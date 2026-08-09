;;; org-atomic-agenda.el --- Org Agenda integration for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko
;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Assisted-by: Gemini:gemini-3.5-flash
;; Version: 1.4.0
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; URL: https://github.com/tmythicator/org-atomic
;; License: GPL-3.0-or-later

;;; Commentary:
;; Org Agenda integration, styling, and stack formatting for org-atomic.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pcase)
(require 'subr-x)
(require 'org)
(require 'org-agenda)
(require 'org-atomic-core)
(require 'org-atomic-util)

(defgroup org-atomic-agenda nil
  "Customization group for org-atomic agenda formatting."
  :group 'org-atomic)

(defcustom org-atomic-agenda-id-format "[ %s ] "
  "Format string for the habit ID prefix.  Must contain exactly one '%s'."
  :type 'string
  :group 'org-atomic-agenda)

(defcustom org-atomic-agenda-branch-prefix " └── "
  "Prefix used for stacked branch indentation in the Org Agenda."
  :type 'string
  :group 'org-atomic-agenda)

(defface org-atomic-agenda-id-face
  '((((background light))
     (:foreground "#0d9488" :weight bold :inherit fixed-pitch))
    (((background dark))
     (:foreground "#2dd4bf" :weight bold :inherit fixed-pitch)))
  "Face for prepended habit ID tags in the Org Agenda."
  :group 'org-atomic-agenda)

(defface org-atomic-agenda-bad-habit-face
  '((((background light))
     (:foreground "#e11d48" :weight bold :inherit fixed-pitch))
    (((background dark))
     (:foreground "#fb7185" :weight bold :inherit fixed-pitch)))
  "Face for prepended bad habit ID tags in the Org Agenda."
  :group 'org-atomic-agenda)

(defface org-atomic-agenda-branch-face
  '((((background light))
     (:foreground "#b0bec5" :inherit fixed-pitch))
    (((background dark))
     (:foreground "#718096" :inherit fixed-pitch)))
  "Face for tree branch indentation in the Org Agenda."
  :group 'org-atomic-agenda)

(defun org-atomic-agenda--build-tooltip (&optional txt parsed-habit)
  "Build a tooltip summary for the habit at point, in TXT, or using PARSED-HABIT."
  (let* ((habit
          (or parsed-habit (org-atomic-core-parse-habit nil txt))))
    (when habit
      (let* ((is-bad (org-atomic-core-habit-bad-p habit))
             (why (org-atomic-core-habit-why habit))
             (strategies (org-atomic-core-habit-strategies habit))
             (strategy-lines
              (seq-map
               (pcase-lambda (`(,label . ,text))
                 (format "Make It %s: %s" label text))
               strategies))
             (lines
              (thread-last
               (append
                (list
                 (format "Type: %s"
                         (if is-bad
                             "Bad Habit"
                           "Good Habit"))
                 (when why
                   (format "Why: %s" why)))
                strategy-lines)
               (delq nil))))
        (when (> (length lines) 1)
          (string-join lines "\n"))))))

(defun org-atomic-agenda--cmp-time (time-a time-b)
  "Compare effective times TIME-A and TIME-B."
  (org-atomic-util--cmp-number-with-nil time-a time-b))

(defun org-atomic-agenda--cmp-stacks (key-a key-b)
  "Compare stack keys KEY-A and KEY-B hierarchically.
Returns -1 if KEY-A comes before KEY-B under the same root,
1 if after, or nil if not comparable."
  (when (and key-a key-b)
    (let ((root-a (car (split-string key-a "/")))
          (root-b (car (split-string key-b "/"))))
      (when (string= root-a root-b)
        (cond
         ((string-lessp key-a key-b)
          -1)
         ((string-lessp key-b key-a)
          1))))))

(defun org-atomic-agenda-cmp (a b)
  "Custom comparator for sorting atomic habits in the agenda.
Compares agenda entries A and B by time, then stacks them together."
  (let* ((marker-a (org-atomic-util--find-marker a))
         (marker-b (org-atomic-util--find-marker b))
         (key-a
          (and marker-a (org-atomic-core--get-stack-key marker-a)))
         (key-b
          (and marker-b (org-atomic-core--get-stack-key marker-b)))
         (time-a
          (org-atomic-core--get-effective-time a key-a marker-a))
         (time-b
          (org-atomic-core--get-effective-time b key-b marker-b)))
    (or (org-atomic-agenda--cmp-time time-a time-b)
        (org-atomic-agenda--cmp-stacks key-a key-b))))

(defvar org-atomic-agenda--saved-sorting-strategy nil
  "Saved value of `org-agenda-sorting-strategy`.")

(defvar org-atomic-agenda--saved-cmp-user-defined nil
  "Saved value of `org-agenda-cmp-user-defined`.")

(defun org-atomic-agenda--inject-sorting-rule (strategy)
  "Pure function: return a new STRATEGY with `user-defined-up' prepended."
  (seq-map
   (pcase-lambda (`(,key . ,rules))
     (if (and (listp rules)
              (not (memq 'user-defined-up rules))
              (not (memq 'user-defined-down rules)))
         (cons key (cons 'user-defined-up rules))
       (cons key rules)))
   strategy))

(defun org-atomic-agenda--enable-sorting ()
  "Enable custom agenda sorting for org-atomic-agenda."
  (setq org-atomic-agenda--saved-cmp-user-defined
        org-agenda-cmp-user-defined)
  (setq org-agenda-cmp-user-defined #'org-atomic-agenda-cmp)
  ;; Modify org-agenda-sorting-strategy
  (setq org-atomic-agenda--saved-sorting-strategy
        (copy-tree org-agenda-sorting-strategy))
  (setq org-agenda-sorting-strategy
        (org-atomic-agenda--inject-sorting-rule
         org-agenda-sorting-strategy)))

(defun org-atomic-agenda--disable-sorting ()
  "Disable custom agenda sorting for org-atomic-agenda."
  (when org-atomic-agenda--saved-cmp-user-defined
    (setq org-agenda-cmp-user-defined
          org-atomic-agenda--saved-cmp-user-defined)
    (setq org-atomic-agenda--saved-cmp-user-defined nil))
  (when org-atomic-agenda--saved-sorting-strategy
    (setq org-agenda-sorting-strategy
          org-atomic-agenda--saved-sorting-strategy)
    (setq org-atomic-agenda--saved-sorting-strategy nil)))


(defun org-atomic-agenda--build-prefix-str (stack-key type)
  "Build the propertized ID prefix and hierarchy branch for STACK-KEY and TYPE.
Returns a cons cell (INDENT-STR . LABEL-STR)."
  (let* ((parts (split-string stack-key "/"))
         (len (length parts))
         (label (car (last parts)))
         (face
          (if (string= type "bad")
              'org-atomic-agenda-bad-habit-face
            'org-atomic-agenda-id-face))
         (label-str
          (propertize (format org-atomic-agenda-id-format label)
                      'face face 'font-lock-face face))
         (indent-str
          (if (> len 1)
              (propertize (concat
                           (make-string (* 4 (- len 2)) ?\s)
                           org-atomic-agenda-branch-prefix)
                          'face
                          'org-atomic-agenda-branch-face
                          'font-lock-face
                          'org-atomic-agenda-branch-face)
            "")))
    (cons indent-str label-str)))

(defun org-atomic-agenda--splice-prefix (result txt indent-str id-str)
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

(defun org-atomic-agenda--org-agenda-format-item-advice
    (orig-fun extra txt &rest args)
  "Advice to intercept `org-agenda-format-item' and prepend ID.
ORIG-FUN is the original function.  EXTRA, TXT, and ARGS are the standard
arguments."
  (let* ((marker (org-atomic-util--find-marker txt))
         (habit
          (when marker
            (org-atomic-core-parse-habit marker)))
         (stack-key
          (when marker
            (org-atomic-core--get-stack-key marker)))
         (result (apply orig-fun extra txt args)))
    (when result
      (let* ((has-stack
              (and stack-key
                   (not (string-empty-p (string-trim stack-key)))))
             (type
              (if habit
                  (org-atomic-core-habit-type habit)
                "good"))
             (formatted
              (let ((cleaned
                     (if habit
                         (org-atomic-util--clean-result-time result)
                       result)))
                (if has-stack
                    (let ((prefix-pair
                           (org-atomic-agenda--build-prefix-str
                            stack-key type)))
                      (org-atomic-agenda--splice-prefix
                       cleaned
                       txt
                       (car prefix-pair)
                       (cdr prefix-pair)))
                  cleaned)))
             (tooltip
              (when habit
                (org-atomic-agenda--build-tooltip nil habit))))
        (if tooltip
            (propertize formatted
                        'org-atomic-tooltip
                        tooltip
                        'help-echo
                        tooltip)
          formatted)))))

;;;###autoload
(defun org-atomic-agenda-finalize-faces ()
  "Restore org-atomic-agenda faces in the agenda buffer.
This runs after `org-agenda' has finished styling the entries."
  (org-atomic-core-clear-caches)
  (save-excursion
    (save-restriction
      (widen)
      ;; 1. Restore face properties
      (org-atomic-util-walk-property-intervals
       'font-lock-face
       (lambda (start end fl-face)
         (when (memq
                fl-face
                '(org-atomic-agenda-id-face
                  org-atomic-agenda-bad-habit-face
                  org-atomic-agenda-branch-face))
           (let ((current-face (get-text-property start 'face)))
             (put-text-property
              start end 'face
              (if (listp current-face)
                  (cons fl-face (delq fl-face current-face))
                (list fl-face current-face)))))))
      ;; 2. Restore/merge help-echo tooltips
      (org-atomic-util-walk-property-intervals
       'org-atomic-tooltip
       (lambda (start end tooltip)
         (let ((current-echo (get-text-property start 'help-echo)))
           (put-text-property
            start end 'help-echo
            (if (and current-echo
                     (not (string-prefix-p tooltip current-echo)))
                (concat tooltip "\n---\n" current-echo)
              tooltip))))))))

(provide 'org-atomic-agenda)
;;; org-atomic-agenda.el ends here
