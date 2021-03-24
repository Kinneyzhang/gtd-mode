;;; gtd-utils.el --- Utilities functions  -*- lexical-binding: t; -*-

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
(require 'seq)

;;;; Declarations
(defvar gtd-data)
(defvar gtd-checklists)
(defvar gtd-smart-checklists)

(defvar gtd-weekday-seq '("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"))

;;;; Utilities
(defun gtd-ewoc-data ()
  "Return the ewoc data at point."
  (ewoc-data (ewoc-locate gtd-ewoc)))

(defun gtd-plist->alist (plist)
  (if (null plist) '()
    (cons
     (list (car plist) (cadr plist))
     (gtd-plist->alist (cddr plist)))))

(defun gtd-alist->plist (alist)
  (if (null alist) '()
    (let ((hd (car alist))
          (tl (cdr alist)))
      (cons (car hd) (cons (cadr hd) (gtd-alist->plist tl))))))

(defun gtd-construct-args (slot-lst val-lst)
  "Construct class args according to SLOT-LST and VAL-LST."
  (let (plist alist)
    (dotimes (i (length slot-lst))
      (push (nth i slot-lst) plist)
      (push (nth i val-lst) plist))
    (setq plist (reverse plist))
    (setq alist (gtd-plist->alist plist))
    (gtd-alist->plist
     (seq-filter (lambda (item)
                   (not (equal (cadr item) "")))
                 alist))))

(defun gtd-checklists-attrs (&optional attribute)
  "Return a list of ATTRIBUTE's values of checklists.
If ATTRIBUTE is nil, return a name list of checklists."
  (mapcar (lambda (lst)
            (if attribute
                (plist-get (cdr lst) attribute)
              (car lst)))
          gtd-checklists))

(defun gtd-checklist-attr (checklist attribute)
  "Return the value of ATTRIBUTE of checklist CHECKLIST."
  (plist-get (cdr (or (assoc checklist gtd-checklists)
                      (assoc checklist gtd-smart-checklists)))
             attribute))

(defun gtd-plist-get (keyword lst prop)
  "Return the value of property PROP in a list LST with KEYWORD."
  (plist-get (cdr (assoc keyword lst)) prop))

(defun gtd-common-attr (attr1 value lst attr2)
  "Return the value of ATTR2 in the LST with ATTR1 whose value is VALUE."
  (plist-get (cdr (seq-find (lambda (item)
                              (equal (plist-get (cdr item) attr1) value))
                            lst))
             attr2))

(defun gtd-task-args (id task-lst)
  "Return the task with id ID in a TASK-LIST."
  (seq-find (lambda (lst)
              (equal (plist-get lst :id) id))
            task-lst))

(defun gtd-task-attr (id attribute)
  "Return the ATTRIBUTE of a task with id ID."
  (plist-get (gtd-task-args id gtd-data) attribute))

(defun gtd-curr-week-range ()
  "Return the cons cell (start . end) of current week.
'today' means the time 00:00:00 of today.

The current week range should be 'start <= date < end'."
  (let* ((day-of-week (string-to-number (format-time-string "%u")))
         (start (gtd-date-change '- (1- day-of-week)))
         (end (gtd-date-change '+ (- 7 day-of-week))))
    (cons start end)))

(defun gtd-task-date-format (date)
  "Return the new format of DATE in each specific checklist view.

If the year of date equals to current year, return the date 
format 'month-day'.  Otherwise, return the date format 'year-month-day'."
  (if (string= (substring date 0 4) (format-time-string "%Y"))
      (substring date 5)
    date))

(defun gtd-date-to-seconds (time)
  "Transform the 'year-month-day (Hour:Minute:second)' 
format of TIME to the seconds of format 'year-month-day 00:00:00'."
  (let ((time-spec (concat (substring time 0 10) " 00:00:00")))
    (time-to-seconds (date-to-time time-spec))))

(defun gtd-time-to-seconds (time)
  "Transform the 'year-month-day (Hour:Minute:second)' 
format of TIME to the precise seconds."
  (let* ((time-spec (if (= (length time) 10)
                        (concat time " 00:00:00")
                      time)))
    (time-to-seconds (date-to-time time-spec))))

(defun gtd-format-date (&optional time)
  "Format TIME to 'year-month-day' format.
If TIME is nil, fomrat current time."
  (format-time-string "%Y-%m-%d" (or time (current-time))))

(defun gtd-seconds-to-date (seconds)
  "Transform seconds to 'year-month-day' time format."
  (gtd-format-date seconds))

(defun gtd-date-change (operation num &optional from)
  "Make NUM times of OPERATION change to FROM date.
If FROM is nil, make changes to the current date."
  (let ((from (or from (gtd-format-date))))
    (eval `(gtd-seconds-to-date (,operation ,(gtd-date-to-seconds from)
                                            ,(* num 86400))))))

(defun gtd--day-to-str (day &optional with-zero)
  "Convert the number DAY to a string.
If WITH-ZERO is non-nil, concat zero to number below ten."
  (if with-zero
      (if (< day 10)
          (format "0%s" day)
        (number-to-string day))
    (number-to-string day)))

(defun gtd--day-to-date (month day)
  "Convert the number DAY to date string according to MONTH."
  (concat month "-" (gtd--day-to-str day t)))

;; '(date "2021-03-03")
;; => `(and (>= date ,(gtd-date-to-seconds "2021-03-03"))
;;          (< date ,(gtd-date-to-seconds "2021-03-04")))
;; '(date "2021-03-03" "2021-03-09")
;; => `(and (>= date ,(gtd-date-to-seconds "2021-03-03"))
;;          (< date ,(gtd-date-to-seconds "2021-03-10")))

;; today
;; `(date ,(gtd-format-date))

;; '(checklist "Next") => '(= checklist "Next")
;; '(checklist "Next" "Someday/Maybe") => '(in checklist ["Next" "Someday/Maybe"])

;; '(priority "high") '(priority "middle") '(priority "low") '(priority "none") => '(= priority 3)
;; '(priority "high" "middle") => '(in priority ["high" "middle"])

;; '(tags "free") => '(= tags "free")
;; '(tags "free" "home") => '(in tags ["free" "home"])

;; project
;; '(and (parent "false") (children "true"))
;; => (and (is parent nil) (is-not children nil))

;; (gtd-db-query '[:select name :from task
;;                         :where (is parent nil)])

;; 补全日期比较
;; (gtd-db-query `[:select name :from task
;;                         :where (>= date "2021-03-07%")])
;; (gtd-db-query `[:select [name date] :from task])

;; (gtd-db-query `[:select [name] :from task
;;                         :where (is-not date nil)])

(defvar gtd-rule-attrs
  '(date tags priority checklist parent children))

(defun gtd--parse-rules (rules)
  "Parse the rules of `gtd-smart-checklists' 
into the database query conditions."
  (let ((symbol (car rules))
        (rests (cdr rules)))
    (pcase symbol
      ((or 'and 'or)
       (let (res)
         (dolist (elem rests)
           (push (gtd-parse-rules elem) res))
         (append `(,symbol) (reverse res))))
      ((and elem (guard (member elem gtd-rule-attrs)))
       (pcase elem
         ('date
          (pcase nil
            ((and (guard (= (length rests) 1))
                  (let time (car rests)))
             (pcase time
               ('t '(is-not date nil))
               ('nil '(is date nil))
               (_ `(and (>= date ,(gtd-date-to-seconds time))
                        (< date ,(+ (gtd-date-to-seconds time) 86400))))))
            ((and (guard (= (length rests) 2))
                  (let time1 (car rests))
                  (let time2 (cadr rests)))
             `(and (>= date ,(gtd-date-to-seconds time1))
                   (< date ,(+ (gtd-date-to-seconds time2) 86400))))))
         ('tags
          ()
          )
         ((or 'parent 'children)
          (pcase (car rests)
            ('t `(is-not ,symbol nil))
            ('nil `(is ,symbol nil))))
         ((or 'priority 'checklist)
          (pcase nil
            ((and (guard (= (length rests) 1))
                  (let val (car rests)))
             (if (eq elem 'priority)
                 `(= ,symbol
                     ,(gtd-plist-get
                       (concat val " priority") gtd-priorities :id))
               `(= ,symbol ,val)))
            ((guard (> (length rests) 1))
             (if (eq elem 'priority)
                 `(in ,symbol,(vconcat (mapcar (lambda (val)
                                                 (gtd-plist-get
                                                  (concat val " priority")
                                                  gtd-priorities :id))
                                               rests)))
               `(in ,symbol ,(vconcat rests))))))))
      (_ (error "Invalid `gtd-smart-checklists' rules!")))))

;; test
;; (gtd--parse-rules '(or (date t)
;;                        (date "2021-09-09")))
;; (gtd--parse-rules '(and (parent nil)
;;                         (children t)))
;; (gtd--parse-rules `(and
;;                     (date ,(gtd-format-date))
;;                     (checklist "Next" "Inbox")
;;                     (priority "high" "low")))

(defun gtd--priority-val-lst (lst)
  "Transform a priority string list into a number list."
  (mapcar (lambda (item)
            (cdr (assoc (concat item " priority") gtd-priorities)))
          lst))

(provide 'gtd-utils)
;;; gtd-utils.el ends here
