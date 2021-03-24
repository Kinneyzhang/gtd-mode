;;; gtd-calendar.el --- Gtd calendar module  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: gtd convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/gtd-mode
;; Package-Requires: ((emacs "26.1"))

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

;;;; Dependencies

(require 'gtd-db)

;;;; Variables

(defvar gtd-calendar-buf "*Gtd Calendar*")

(defvar gtd-calendar-mode-map nil)

(defvar gtd-calendar-column-blanks 3
  "The number of blanks between each column in calendar.")

(defvar gtd-calendar-weekday-blanks 2
  "The number of blanks that inserted at places with date.
This value equals to the length of weekday.")

;;;; Functions

(defun gtd-calendar--date-seq (type date)
  "Return a sequence of TYPE calendar date
according to date DATE."
  (let ((year-str (substring date 0 4))
        (month-str (substring date 5 7)))
    (pcase type
      ('month
       (let* ((year (string-to-number year-str))
              (month (string-to-number month-str))
              (day-num (date-days-in-month year month))
              (first-day (format "%s-%s-01" year-str month-str))
              (day-of-week (string-to-number
                            (format-time-string
                             "%u" (gtd-time-to-seconds first-day))))
              (date-seq (number-sequence 1 day-num)))
         (dotimes (_ (1- day-of-week)) (push nil date-seq))
         (seq-partition date-seq 7))))))

(defun gtd--tasks-count-by-date (date)
  "Return the count of tasks according to date DATE."
  (caar
   (gtd-db-query
    `[:select (funcall count id) :from task
              :where ,(gtd--parse-rules
                       `(date ,date))])))

(defun gtd-calendar--task-num-seq (date)
  "Return the seq of the task number of all date in calendar
according to date DATE."
  (let ((month (substring date 0 7)))
    (mapcar (lambda (lst)
              (mapcar (lambda (item)
                        (when item
                          (gtd--tasks-count-by-date
                           (gtd--day-to-date month item))))
                      lst))
            (gtd-calendar--date-seq 'month date))))

(defun gtd-calendar-month-insert (date)
  "Draw a month calendar according to DATE.
Highlight the current selected date and show the number
of tasks belonging to each date."
  (let* ((seconds (gtd-date-to-seconds date))
         (month-header (format-time-string "%B, %Y" seconds))
         (curr-day (string-to-number (substring date 8)))
         (date-seq (gtd-calendar--date-seq 'month date))
         (count-seq (gtd-calendar--task-num-seq date))
         (len (length date-seq)))
    (insert (propertize month-header
                        'face '(gtd-calendar-month-header
                                :height 1.4))
            "\n\n")
    (dotimes (i (length gtd-weekday-seq))
      (pcase 5
        ((pred (< i))
         (insert (propertize
                  (nth i gtd-weekday-seq)
                  'face '(gtd-calendar-weekday-header
                          )))
         (self-insert-command gtd-calendar-column-blanks ? ))
        ((pred (>= i))
         (insert (propertize
                  (nth i gtd-weekday-seq)
                  'face '(gtd-calendar-weekend-header)))
         (self-insert-command gtd-calendar-column-blanks ? ))))
    (insert "\n\n")
    (dotimes (i len)
      (dolist (day (nth i date-seq))
        (if day
            (let* ((day-str (if (< day 10) (format " %s" day)
                              (format "%s" day)))
                   (day (string-to-number day-str)))
              (if (= curr-day day)
                  (insert-text-button day-str
                                      'face 'region
                                      'action #'gtd-calendar-show-task
                                      'help-echo "Show tasks"
                                      'follow-link t)
                (insert-text-button day-str
                                    'face nil
                                    'action #'gtd-calendar-show-task
                                    'help-echo "Show tasks"
                                    'follow-link t)))
          (self-insert-command gtd-calendar-weekday-blanks ? ))
        (self-insert-command gtd-calendar-column-blanks ? ))
      (insert "\n")
      (dolist (count (nth i count-seq))
        (pcase count
          ((or 'nil 0)
           (self-insert-command (+ gtd-calendar-weekday-blanks
                                   gtd-calendar-column-blanks)
                                ? ))
          ((pred (< 0))
           (insert (propertize (if (< count 10) (format " %s" count)
                                 (format "%s" count))
                               'face '(shadow :height 0.9)))
           (self-insert-command gtd-calendar-column-blanks ? ))))
      (insert "\n"))
    (insert (propertize
             (format-time-string "%b %d" seconds)
             'face '(italic :height 1.2))
            "\n")))

(defun gtd-calendar-pp (data)
  "Pretty printer for gtd calendar."
  (pcase data
    ((pred listp)
     (let ((sym (car data))
           (rests (cdr data)))
       (pcase sym
         ('calendar
          (let ((type (plist-get rests :type))
                (date (plist-get rests :date)))
            (gtd-calendar-insert type date))))))
    ((pred org-uuidgen-p)
     (gtd-tasks-pp data))
    ("\nDONE"
     (insert (propertize data 'face 'bold)))
    (_ (insert data))))

(defun gtd-calendar--show (type &optional date)
  "Show the TYPE of calendar and tasks on DATE."
  (let* ((date (or date (gtd-format-date)))
         (month (substring date 0 7))
         (_ (gtd--switch-to-buffer gtd-calendar-buf))
         (tasks (gtd-db-tasks-by-date date))
         (ewoc (ewoc-create 'gtd-calendar-pp
                            (propertize "📅 Calendar\n"
                                        'face 'gtd-header-face)
                            (substitute-command-keys
                             "\n\\{gtd-calendar-mode-map}"))))
    (set (make-local-variable 'gtd-ewoc) ewoc)
    (set (make-local-variable 'gtd-month) month)
    (set (make-local-variable 'gtd-date) date)
    (pcase type
      ('month
       (ewoc-enter-last ewoc `(calendar :type month :date ,date)))
      ('week
       (ewoc-enter-last ewoc `(calendar :type week :date ,date)))
      ('day
       (ewoc-enter-last ewoc `(calendar :type day :date ,date))))
    (gtd--show-tasks gtd-ewoc tasks)
    (read-only-mode 1)))

;;;###autoload
(defun gtd-calendar-show-month (&optional date)
  "Show gtd month calendar. If DATE is non-nil, 
show the tasks on DATE."
  (interactive)
  (gtd-calendar--show 'month date))

;;;###autoload
(defun gtd-calendar-show-week (&optional date)
  "Show gtd week calendar. If DATE is non-nil, 
show the tasks on DATE."
  (interactive)
  (gtd-calendar--show 'week date))

;;;###autoload
(defun gtd-calendar-show-day (&optional date)
  "Show gtd day calendar. If DATE is non-nil,
show the tasks on DATE."
  (interactive)
  (gtd-calendar--show 'day date))

;;;###autoload
(defun gtd-calendar-show-task (&optional btn)
  "Show the tasks of a specific date."
  (let* ((str (button-label (button-at (point))))
         (day (if (string= (substring str 0 1) " ")
                  (concat "0" (string-trim-left str " "))
                str))
         (date (concat gtd-month "-" day)))
    (setq gtd-date date)
    (gtd-calendar-show-month gtd-date)))

(defun gtd-calendar-insert (type date)
  "Insert the TYPE of calendar according to TIME."
  (pcase type
    ('month (gtd-calendar-month-insert date))
    ('week (gtd-calendar-week-insert date))
    ('day (gtd-calendar-day-insert date))))

;;;###autoload
(define-minor-mode gtd-calendar-mode
  "Minor mode for gtd calendar."
  nil nil nil)

(provide 'gtd-calendar)
;;; gtd-calendar.el ends here
