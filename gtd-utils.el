;;; gtd-utils.el --- purpose

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Kinney Zhang <kinneyzhang666 AT gmail DOT com>
;; URL: http://github.com/usrname/gtd-utils
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

;;;; Dependencies
(require 'seq)

;;;; Declarations
(defvar gtd-checklists)
(defvar gtd-smart-checklists)
(defvar gtd-smart-default-rules)

;;;; Utilities
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
                (plist-get (cadr lst) attribute)
              (car lst)))
          gtd-checklists))

(defun gtd-checklist-attr (checklist attribute)
  "Return the value of ATTRIBUTE of checklist CHECKLIST."
  (plist-get (cadr (or (assoc checklist gtd-checklists)
                       (assoc checklist gtd-smart-checklists)))
             attribute))

(defun gtd-curr-week-range ()
  "Return the cons cell (start . end) of current week.
'today' means the time 00:00:00 of today.

The current week range should be 'start <= date < end'."
  (let* ((day-of-week (string-to-number (format-time-string "%u")))
         (start (format "today-%s" (1- day-of-week)))
         (end (format "today+%s" (- 8 day-of-week))))
    (cons start end)))

(defun gtd-format-date (&optional time)
  "Format TIME to 'year-month-day' format.
If TIME is nil, fomrat current time."
  (format-time-string "%Y-%m-%d" (or time (current-time))))

(defun gtd-date-to-seconds (date)
  "Transform 'year-month-day' time format to seconds."
  (let* ((time-spec (concat date " 00:00:00"))
         (seconds (time-to-seconds (date-to-time time-spec))))
    seconds))

(defun gtd-seconds-to-date (seconds)
  "Transform seconds to 'year-month-day' time format."
  (gtd-format-date seconds))

(defun gtd--parse-rule-sugar (keyword sugar)
  (pcase keyword
    ('date
     (let ((str (string-trim-left sugar "today"))
           plus-or-minus num date)
       (if (string-empty-p str)
           (gtd-format-date)
         (setq plus-or-minus (intern (substring str 0 1)))
         (setq num (string-to-number (substring str 1)))
         (setq date (gtd-seconds-to-date
                     (eval `(,plus-or-minus
                             ,(gtd-date-to-seconds (gtd-format-date))
                             ,(* num 86400))))))))
    ('status
     (pcase sugar
       ("todo" 0)
       ("done" 1)))))

(defun gtd--parse-rule (rules)
  "Parse the keyword in smart checklist rules."
  (let (lst)
    (setq lst (pcase (car rules)
                ;; ((or 'and 'or))
                ('date
                 (let* ((date-lst (cdr rules))
                        (date-len (length date-lst)))
                   (cond
                    ((= 1 date-len)
                     (let* ((sugar (car date-lst))
                            (date (gtd--parse-rule-sugar 'date sugar)))
                       `(like date ,(concat date "%"))))
                    ((= 2 date-len)
                     (let ((start-date (gtd--parse-rule-sugar 'date (nth 0 date-lst)))
                           (end-date (gtd--parse-rule-sugar 'date (nth 1 date-lst))))
                       `(and (>= date (like ,(concat start-date "%")))
                             (< date (like ,(concat end-date "%")))))))))
                ('priority
                 (let* ((lst (cdr rules))
                        (len (length lst)))
                   (pcase len
                     (1 `(= priority
                            ,(cdr (assoc
                                   (concat (car lst) " priority")
                                   gtd-priorities))))
                     (_ `(in priority ,(gtd--priority-val-lst lst))) ;; need query res?
                     )))
                ('checklist
                 (let* ((lst (cdr rules))
                        (len (length lst)))
                   (pcase len
                     (1 `(= checklist ,(car lst)))
                     (_ `(in priority ,lst)) ;; need query res?
                     )))
                ;; ..............
                ))
    (if (equal (car lst) (or 'and 'or))
        (setq lst (append lst '((= status 0))))
      (setq lst (list 'and lst '(= status 0))))
    lst))

(defun gtd--priority-val-lst (lst)
  "Transform a priority string list into a number list."
  (mapcar (lambda (item)
            (cdr (assoc (concat item " priority") gtd-priorities)))
          lst))

(defun gtd-task-conditions-smartly (checklist)
  "Return the conditions of CHECKLIST used for database querying."
  (let ((rules (cadr (gtd-checklist-attr checklist :rules))))
    (gtd--parse-rule rules)))

(provide 'gtd-utils)
;;; gtd-utils.el ends here

