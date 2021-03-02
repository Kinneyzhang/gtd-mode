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
If ATTRIBUTE is nil, the default one is :name."
  (mapcar (lambda (lst)
            (plist-get lst (or attribute :name)))
          gtd-checklists))

(defun gtd-checklist-attr (checklist attribute)
  "Return the value of ATTRIBUTE of checklist CHECKLIST."
  (plist-get (seq-find (lambda (lst)
                         (string= (plist-get lst :name) checklist))
                       gtd-checklists)
             attribute))

(provide 'gtd-utils)
;;; gtd-utils.el ends here

