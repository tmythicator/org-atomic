;;; org-atomic-test.el --- Tests for org-atomic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alexandr Timchenko

;; Author: Alexandr Timchenko <atimchenko92@gmail.com>
;; Maintainer: Alexandr Timchenko <atimchenko92@gmail.com>
;; URL: https://github.com/tmythicator/org-atomic
;; Keywords: outlines, hypermedia, calendar, tasks

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of org-atomic.

;;; Commentary:

;; Test suite for the org-atomic package.

;;; Code:

(require 'ert)
(require 'org-atomic)

(ert-deftest org-atomic-test-parse-days ()
  "Test parsing of ATOMIC_DAYS values."
  (let ((org-atomic-day-groups '(("workdays" 1 2 3 4 5)
                                 ("weekends" 6 7))))
    (should (equal (org-atomic--parse-days "workdays") '(1 2 3 4 5)))
    (should (equal (org-atomic--parse-days "weekends") '(6 7)))
    (should (equal (org-atomic--parse-days "1, 2, 3") '(1 2 3)))
    (should (equal (org-atomic--parse-days "1 2 3") '(1 2 3)))
    (should (equal (org-atomic--parse-days "mon tue wed") '(1 2 3)))
    (should (equal (org-atomic--parse-days "mon,tue") '(1 2)))
    (should (equal (org-atomic--parse-days "Sunday Monday") '(7 1)))
    (should (null (org-atomic--parse-days nil)))
    (should (null (org-atomic--parse-days "")))
    (should (null (org-atomic--parse-days "invalid-group")))))

(ert-deftest org-atomic-test-draw-sparkline ()
  "Test drawing of sparklines."
  (let ((org-atomic-done-char ?A)
        (org-atomic-missed-char ?B)
        (org-atomic-skipped-char ?C)
        (org-atomic-sparkline-start-char ?\[)
        (org-atomic-sparkline-end-char ?\]))
    (let ((sparkline (org-atomic-draw-sparkline '(good-done good-missed skipped future))))
      (should (string= (substring-no-properties sparkline 0 1) "["))
      (should (string= (substring-no-properties sparkline 1 2) "A"))
      (should (string= (substring-no-properties sparkline 2 3) "B"))
      (should (string= (substring-no-properties sparkline 3 4) "C"))
      (should (string= (substring-no-properties sparkline 4 5) " "))
      (should (string= (substring-no-properties sparkline 5 6) "]")))))

(ert-deftest org-atomic-test-is-active-today-p ()
  "Test org-atomic-is-active-today-p using mock-habits.org fixture."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test/fixtures/mock-habits.org" (or (bound-and-true-p default-directory) ".")))
    (org-mode)
    (let ((org-atomic-day-groups '(("workdays" 1 2 3 4 5)
                                   ("weekends" 6 7))))
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (org-next-visible-heading 1))
      (should (org-atomic-is-active-today-p 7))
      (should-not (org-atomic-is-active-today-p 1))

      ;; Heading 2: Org-Atomic Coding (workdays: 1-5)
      (org-next-visible-heading 1)
      (should (org-atomic-is-active-today-p 1))
      (should-not (org-atomic-is-active-today-p 7))

      ;; Heading 3: Scrolling Social Media (workdays: 1-5)
      (org-next-visible-heading 1)
      (should (org-atomic-is-active-today-p 1))
      (should-not (org-atomic-is-active-today-p 7))

      ;; Heading 4: Daily Meditation (fallback to daily behavior)
      (org-next-visible-heading 1)
      (should (org-atomic-is-active-today-p 7))
      (should (org-atomic-is-active-today-p 1)))))

(ert-deftest org-atomic-test-build-tooltip ()
  "Test constructing the help-echo tooltip for atomic habits."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test/fixtures/mock-habits.org" (or (bound-and-true-p default-directory) ".")))
    (org-mode)
    ;; Heading 1: Gym (Good Habit)
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (org-next-visible-heading 1))
    (let ((tooltip (org-atomic--build-tooltip nil)))
      (should (string-match-p "Type: Good Habit" tooltip))
      (should (string-match-p "Why: Aids flexibility" tooltip))
      (should (string-match-p "Make It Obvious: Leave yoga mat" tooltip))
      (should (string-match-p "Make It Easy: 10 minutes" tooltip)))

    ;; Heading 3: Scrolling Social Media (Bad Habit)
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (org-next-visible-heading 1))
    (org-next-visible-heading 2) ; Move to heading 3
    (let ((tooltip (org-atomic--build-tooltip nil)))
      (should (string-match-p "Type: Bad Habit" tooltip))
      (should (string-match-p "Why: Waste of time" tooltip))
      (should (string-match-p "Make It Invisible: Put phone in another room" tooltip))
      (should (string-match-p "Make It Hard: Uninstall Instagram" tooltip)))))

(ert-deftest org-atomic-test-bad-habit-sparkline ()
  "Test sparkline building logic for Bad Habits."
  (let* ((day-fri (time-to-days (encode-time 0 0 0 26 6 2026))) ; Absolute day (Friday)
         (day-sat (time-to-days (encode-time 0 0 0 27 6 2026))) ; Saturday
         (day-sun (time-to-days (encode-time 0 0 0 28 6 2026))) ; Sunday
         (day-mon (time-to-days (encode-time 0 0 0 29 6 2026))) ; Monday
         (day-tue (time-to-days (encode-time 0 0 0 30 6 2026))) ; Tuesday
         (habit (list "Scrolling Social Media"
                      ".+1d" nil nil
                      ;; done dates (Friday and Sunday)
                      (list day-fri day-sun)))
         (org-atomic-day-groups '(("workdays" 1 2 3 4 5))))
    (cl-letf (((symbol-function 'org-atomic--parse-habit)
               (lambda (&optional _marker _txt)
                 (org-atomic-habit-create
                  :type "bad"
                  :days '(1 2 3 4 5)))))
      ;; Test day-fri (active: yes, done: yes -> bad-done/filled!)
      ;; Test day-sat (active: no, done: no -> skipped!)
      ;; Test day-sun (active: no, done: yes -> skipped!)
      ;; Test day-mon (active: yes, done: no -> bad-avoided/empty!)
      ;; Test day-tue (active: yes, done: no -> bad-avoided/empty!)
      (let* ((starting (encode-time 0 0 0 26 6 2026)) ; Friday
             (current (encode-time 0 0 0 30 6 2026))  ; Tuesday
             (ending (encode-time 0 0 0 2 7 2026))   ; Thursday
             (sparkline (org-atomic-build-sparkline habit starting current ending)))
        (should (string= (substring-no-properties sparkline 0 1) "["))
        ;; 2026-06-26 (Friday): donep=t, is-bad=t -> bad-done (filled box, missed/red face)
        (should (string= (substring-no-properties sparkline 1 2) (string org-atomic-done-char)))
        (should (eq (get-text-property 1 'face sparkline) 'org-atomic-missed-face))
        ;; 2026-06-27 (Saturday): not active, donep=nil -> skipped (middle dot, skipped/gray face)
        (should (string= (substring-no-properties sparkline 2 3) (string org-atomic-skipped-char)))
        (should (eq (get-text-property 2 'face sparkline) 'org-atomic-skipped-face))
        ;; 2026-06-28 (Sunday): not active, donep=t -> skipped (middle dot, skipped/gray face)
        (should (string= (substring-no-properties sparkline 3 4) (string org-atomic-skipped-char)))
        (should (eq (get-text-property 3 'face sparkline) 'org-atomic-skipped-face))
        ;; 2026-06-29 (Monday): active, donep=nil -> bad-avoided (empty box, done/green face)
        (should (string= (substring-no-properties sparkline 4 5) (string org-atomic-missed-char)))
        (should (eq (get-text-property 4 'face sparkline) 'org-atomic-done-face))
        ;; 2026-06-30 (Tuesday): active, donep=nil -> bad-avoided (empty box, done/green face)
        (should (string= (substring-no-properties sparkline 5 6) (string org-atomic-missed-char)))
        (should (eq (get-text-property 5 'face sparkline) 'org-atomic-done-face))
        (should (string= (substring-no-properties sparkline 6 7) "]"))))))

(ert-deftest org-atomic-test-render-from-buffer ()
  "Test that sparkline renders correctly parsing actual buffer properties and logbook entries."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test/fixtures/mock-habits.org" (or (bound-and-true-p default-directory) ".")))
    (org-mode)
    (setq org-habit-show-habits t)
    (let ((org-atomic-day-groups '(("workdays" 1 2 3 4 5)
                                   ("weekends" 6 7))))
      ;; Go to "Scrolling Social Media"
      (goto-char (point-min))
      (search-forward "* TODO Scrolling Social Media")
      (beginning-of-line)
      (let* ((habit (org-habit-parse-todo))
             ;; Let's assume current is Wednesday June 24, 2026
             (current (encode-time 0 0 0 24 6 2026))
             ;; starting is 4 days ago (June 20)
             (starting (encode-time 0 0 0 20 6 2026))
             ;; ending is 2 days in the future (June 26)
             (ending (encode-time 0 0 0 26 6 2026))
             (sparkline (org-atomic-build-sparkline habit starting current ending)))
        ;; The sparkline should match the active workdays (Mon-Fri) and done dates from logbook.
        ;; In mock-habits.org, the done date is June 23, 2026 (Tuesday).
        ;; Range June 20 (Sat) to June 26 (Fri) is 7 days.
        ;; Body length: 7 - 2 = 5 days.
        ;; Truncation: future-days = 2 (June 25, 26).
        ;; So we check June 20 (Sat) to June 24 (Wed).
        ;; - June 20 (Sat): inactive -> skipped (·)
        ;; - June 21 (Sun): inactive -> skipped (·)
        ;; - June 22 (Mon): active (workdays), not done -> bad-avoided (□)
        ;; - June 23 (Tue): active (workdays), done -> bad-done (■)
        ;; - June 24 (Wed): active (workdays), not done -> bad-avoided (□)
        ;; Total sparkline string: [··□■□]
        (should (string= (substring-no-properties sparkline 0 1) "["))
        (should (string= (substring-no-properties sparkline 1 2) (string org-atomic-skipped-char))) ; Sat
        (should (string= (substring-no-properties sparkline 2 3) (string org-atomic-skipped-char))) ; Sun
        (should (string= (substring-no-properties sparkline 3 4) (string org-atomic-missed-char)))  ; Mon
        (should (string= (substring-no-properties sparkline 4 5) (string org-atomic-done-char)))    ; Tue
        (should (string= (substring-no-properties sparkline 5 6) (string org-atomic-missed-char)))  ; Wed
        (should (string= (substring-no-properties sparkline 6 7) "]"))))))

(ert-deftest org-atomic-test-stack-sorting ()
  "Test hierarchical stack key resolution and comparator sorting."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test/fixtures/mock-habits.org" (or (bound-and-true-p default-directory) ".")))
    (org-mode)
    (let ((markers nil))
      ;; Collect markers for the headers in mock-habits.org
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (push (point-marker) markers))
      (setq markers (nreverse markers))

      ;; There should be 5 habits now (including Hardcore Static Stretching)
      (should (= (length markers) 5))

      (let* ((gym-marker (nth 0 markers))       ; Strength Training (Gym)
             (code-marker (nth 1 markers))      ; Org-Atomic Coding (Code)
             (scroll-marker (nth 2 markers))    ; Scrolling Social Media (Scroll)
             (meditate-marker (nth 3 markers))  ; Daily Meditation (no ID)
             (stretch-marker (nth 4 markers)))  ; Hardcore Static Stretching (Stretch, stack: Code)

        ;; Test hierarchical keys
        (should (string= (org-atomic--get-stack-key gym-marker) "Gym"))
        (should (string= (org-atomic--get-stack-key code-marker) "Code"))
        (should (string= (org-atomic--get-stack-key scroll-marker) "Scroll"))
        (should (null (org-atomic--get-stack-key meditate-marker)))
        (should (string= (org-atomic--get-stack-key stretch-marker) "Code/Stretch"))

        ;; Test comparator
        (let ((item-code (propertize "Code" 'org-marker code-marker))
              (item-stretch (propertize "Stretch" 'org-marker stretch-marker))
              (item-gym (propertize "Gym" 'org-marker gym-marker))
              (item-none (propertize "None" 'org-marker meditate-marker)))

          ;; Code should be first
          (should (= (org-atomic-agenda-cmp item-code item-stretch) -1))
          ;; Stretch should be after
          (should (= (org-atomic-agenda-cmp item-stretch item-code) 1))

          ;; Comparing Code (Code) and Gym (Gym)
          ;; Alphabetically, "Code" < "Gym", so Code is first (-1)
          (should (= (org-atomic-agenda-cmp item-code item-gym) -1))

          ;; Comparing with item-none (no stack key) should place atomic habits first
          (should (= (org-atomic-agenda-cmp item-code item-none) -1))
          (should (= (org-atomic-agenda-cmp item-none item-code) 1))
          ;; Comparing two non-atomic items should return nil
          (should (null (org-atomic-agenda-cmp item-none item-none))))))))

(ert-deftest org-atomic-test-filter-canceled-states ()
  "Test that CANCELED logbook entries are correctly ignored during sparkline rendering."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test/fixtures/mock-habits.org" (or (bound-and-true-p default-directory) ".")))
    (org-mode)
    (setq org-habit-show-habits t)
    ;; Find Hardcore Static Stretching (has CANCELED today, and DONE yesterday)
    (goto-char (point-min))
    (search-forward "* TODO Hardcore Static Stretching")
    (beginning-of-line)
    ;; Let's evaluate non-canceled done dates
    (let ((dates (org-atomic--get-non-canceled-done-dates (point-marker))))
      ;; In mock-habits.org:
      ;; - State "CANCELED" from "TODO"       [2026-06-24 Wed 17:13]
      ;; - State "DONE"     from "TODO"       [2026-06-23 Tue]
      ;; We expect the list of done dates to ONLY contain June 23 (739790) and NOT June 24 (739791)!
      (let ((june-23 (time-to-days (encode-time 0 0 0 23 6 2026)))
            (june-24 (time-to-days (encode-time 0 0 0 24 6 2026))))
        (should (member june-23 dates))
        (should-not (member june-24 dates))))))

(ert-deftest org-atomic-test-auto-repeat-reschedule ()
  "Test that completing an atomic habit reschedules it to the next active day."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test/fixtures/mock-habits.org" (or (bound-and-true-p default-directory) ".")))
    (org-mode)
    (org-atomic-mode 1)
    (let ((org-atomic-day-groups '(("workdays" 1 2 3 4 5)
                                   ("weekends" 6 7))))
      ;; Go to "Strength Training" (which has ATOMIC_DAYS: weekends)
      (goto-char (point-min))
      (search-forward "* TODO Strength Training")
      (beginning-of-line)
      ;; Force schedule to Tuesday, June 23, 2026
      (search-forward "SCHEDULED: ")
      (delete-region (point) (line-end-position))
      (insert "<2026-06-23 Tue .+1d>")
      (beginning-of-line)
      ;; Complete the task, which triggers auto-repeat
      (org-todo "DONE")
      ;; Check that the new SCHEDULED date is Saturday, June 27, 2026
      (let ((scheduled (org-entry-get nil "SCHEDULED")))
        (should (string-match-p "2026-06-27 Sat" scheduled))))))

(ert-deftest org-atomic-test-agenda-filtering ()
  "Test that a habit is filtered out of the agenda on its rest days."
  (let ((org-agenda-files (list (expand-file-name "test/fixtures/mock-habits.org" (or (bound-and-true-p default-directory) "."))))
        (org-atomic-day-groups '(("workdays" 1 2 3 4 5)
                                 ("weekends" 6 7))))
    ;; Wednesday, June 24, 2026 (a workday)
    (let* ((workday '(6 24 2026))
           ;; Sunday, June 28, 2026 (a weekend)
           (weekend '(6 28 2026))
           ;; Run filtering advice for Strength Training (weekends) on workday
           ;; In mock-habits.org, Gym is weekends only.
           (workday-entries (org-atomic--org-agenda-get-day-entries-advice
                             (lambda (file date &rest _)
                               ;; mock returning the agenda item for Gym
                               (list (propertize "Gym" 'org-marker
                                                 (with-current-buffer (find-file-noselect (car org-agenda-files))
                                                   (save-excursion
                                                     (goto-char (point-min))
                                                     (search-forward "Gym")
                                                     (point-marker))))))
                             (car org-agenda-files) workday))
           (weekend-entries (org-atomic--org-agenda-get-day-entries-advice
                             (lambda (file date &rest _)
                               (list (propertize "Gym" 'org-marker
                                                 (with-current-buffer (find-file-noselect (car org-agenda-files))
                                                   (save-excursion
                                                     (goto-char (point-min))
                                                     (search-forward "Gym")
                                                     (point-marker))))))
                             (car org-agenda-files) weekend)))
      ;; Gym should be filtered out on workday (Wed)
      (should (null workday-entries))
      ;; Gym should be kept on weekend (Sun)
      (should (= (length weekend-entries) 1)))))

(ert-deftest org-atomic-test-agenda-prefix-rendering ()
  "Test that the agenda item formatter prepends the correct ID prefix and hierarchy branch."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test/fixtures/mock-habits.org" (or (bound-and-true-p default-directory) ".")))
    (org-mode)
    (let ((org-atomic-id-format "[%s] ")
          (org-atomic-branch-prefix " ╰─> "))
      ;; 1. Go to "Strength Training" (Gym) - single habit, good habit
      (goto-char (point-min))
      (search-forward "* TODO Strength Training")
      (beginning-of-line)
      (let* ((marker (point-marker))
             ;; We construct a text string with 'org-marker property, just like the agenda builder does
             (txt (propertize "TODO Strength Training" 'org-marker marker))
             (formatted (org-atomic--org-agenda-format-item-advice
                         (lambda (_extra text &rest _) text)
                         nil txt)))
        ;; Should contain the ID prefix "[Gym] "
        (should (string-match-p "\\[Gym\\]" formatted)))

      ;; 2. Go to "Hardcore Static Stretching" (Stretch, anchor Code) - stacked habit
      (goto-char (point-min))
      (search-forward "* TODO Hardcore Static Stretching")
      (beginning-of-line)
      (let* ((marker (point-marker))
             (txt (propertize "TODO Hardcore Static Stretching" 'org-marker marker))
             (formatted (org-atomic--org-agenda-format-item-advice
                         (lambda (_extra text &rest _) text)
                         nil txt)))
        ;; Should contain the hierarchy transition └── and the ID Stretch
        (should (string-match-p "└──" formatted))
        (should (string-match-p "Stretch" formatted))))))

(provide 'org-atomic-test)
;;; org-atomic-test.el ends here
