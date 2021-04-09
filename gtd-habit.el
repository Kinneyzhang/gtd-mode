;;; gtd-habit.el --- purpose

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Kinney Zhang <kinneyzhang666 AT gmail DOT com>
;; URL: http://github.com/usrname/gtd-habit
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;;; Code:

(defvar gtd-habits
  '("early to rise" "drink water" "learn new words" "get news updates"
    "walk the dog" "be a good cat keeper" "early to bed"
    "eat breakfast" "stretch" "reading" "self-reflection"
    "plan your day" "running" "eat fruits" "yoga" "track expenses"
    "keep a diary" "eat veggies" "do push-ups" "quit snacks"
    "go cycling" "no dirty words" "quit sugar"
    "swimming" "connect a loved one" "take a walk"
    "learn musical instruments" "meditate" "take a shower"
    "water flowers" "quit smoking" "watch a movie" "no video games"
    "take medicine" "clean up" "quit alcohol" "reduce screen time"
    "do housework" "do skincare" "smile to yourself")
  "The list of built-in habits.")

(defvar gtd-habit-frequency
  '("by day" "by week" "by period"))

(defvar gtd-habit-frequency-by-day
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

;;;###autoload
(defun gtd-habit-new ()
  "Create a new habit."
  (interactive)
  (let* ((habit (gtd-completing-read
                 "Choose or input the habit name" gtd-habits))
         (frequency (gtd-completing-read
                     "Choose the frequency type of the habit"
                     gtd-habit-frequency nil t))
         (frequency-arg
          (pcase frequency
            ("by day"
             (gtd-completing-read-multiple
              "In these days" gtd-habit-frequency-by-day nil t))
            ("by week"
             (gtd-completing-read
              "How many days each week"
              (mapcar #'number-to-string (number-sequence 1 6)) nil t))
            ("by period"
             (gtd-completing-read
              "Every few days"
              (mapcar #'number-to-string (number-sequence 2 30)) nil t))))
         (goal (gtd-completing-read
                "How many times each day" nil)))
    ))

(provide 'gtd-habit)
;;; gtd-habit.el ends here
