# org-atomic

`org-atomic` is a habit tracker for Emacs Org-mode. It replaces default asterisk-based `org-habit` graphs with clean Unicode blocks, and adds day groups, habit stacking, and tooltips inspired by the book *Atomic Habits* by James Clear.

## Features

1. **Unicode Sparklines**: Replaces default asterisks (`*` and `!`) with configurable Unicode blocks:
   - `■` (Done)
   - `□` (Missed)
   - `·` (Skipped / rest day)
2. **Flexible Day Groups**: Define active days (e.g., `workdays`, `weekends`, or specific weekdays). Rest days are represented as skipped (`·`) and do not break streaks.
3. **Agenda Prefixes**: Prepends a styled habit ID prefix (e.g. `[ Gym ] Strength Training`) in the Org Agenda.
4. **Habit Stacking**: Anchor habits to other habits to form sequential dependencies in the agenda view.
5. **Contextual Tooltips**: Displays prompts and cues on hover (e.g., motivation, ease triggers).

---

## Installation

### Built-in package-vc (Emacs 29+)

The recommended way for modern Emacs setups is to use the built-in package-vc:

```elisp
(use-package org-atomic
  :vc (:url "https://github.com/tmythicator/org-atomic")
  :init
  (org-atomic-mode 1))
```

### quelpa

```elisp
(use-package org-atomic
  :quelpa (org-atomic :repo "tmythicator/org-atomic" :fetcher github)
  :init
  (org-atomic-mode 1))
```

---

## How to use

Add `:STYLE: habit` to your Org entry and use the following properties:

### Core Options

- `ATOMIC_ID`: A unique ID for the habit (e.g. `Gym`). This ID is also shown as a prefix in your agenda.
- `ATOMIC_TYPE`: Set to `good` (default) or `bad` (for habits you want to avoid).
- `ATOMIC_DAYS`: Active days. Can be a group (`workdays`, `weekends`), day numbers (`1-7` where 1 is Monday), or day names (`mon,tue`).
- `ATOMIC_ANCHOR`: The ID of another habit to stack under (e.g. `Code`).

### Tooltip Prompts (Atomic Habit Laws)

- `ATOMIC_WHY`: Why you want to build/avoid this habit.
- `ATOMIC_OBVIOUS` (or `ATOMIC_INVISIBLE` for bad habits): 1st Law.
- `ATOMIC_ATTRACTIVE` (or `ATOMIC_UNATTRACTIVE`): 2nd Law.
- `ATOMIC_EASY` (or `ATOMIC_HARD`): 3rd Law.
- `ATOMIC_SATISFYING` (or `ATOMIC_UNSATISFYING`): 4th Law.

### Example

```org
* TODO Strength Training
  SCHEDULED: <2026-06-24 Wed .+1d>
  :PROPERTIES:
  :STYLE:             habit
  :ATOMIC_DAYS:       weekends
  :ATOMIC_ID:         Gym
  :ATOMIC_WHY:        Aids flexibility
  :ATOMIC_OBVIOUS:    Leave yoga mat out on the floor
  :ATOMIC_EASY:       10 minute session
  :END:
  - State "DONE"       from "TODO"       [2026-06-21 Sun]
```

---

## Customization

You can change the characters and default day groups:

```elisp
;; Define your own active day groups
(setq org-atomic-day-groups
      '(("workdays" 1 2 3 4 5)
        ("weekends" 6 7)
        ("gym-days" 1 3 5)))

;; Custom characters
(setq org-atomic-done-char ?■
      org-atomic-missed-char ?□
      org-atomic-skipped-char ?·)

;; Exclude specific terminal states from being counted as habit completions
(setq org-atomic-excluded-logbook-states
      '("CANCELED" "CANCELLED" "SKIPPED" "FAILED"))
```

Faces available for customization:

- `org-atomic-done-face` (default: green/blue)
- `org-atomic-missed-face` (default: red/orange)
- `org-atomic-skipped-face` (default: gray)
- `org-atomic-id-face` (default: teal badge)
- `org-atomic-bad-habit-face` (default: rose badge)
- `org-atomic-sparkline-pill-face` (background container color)
