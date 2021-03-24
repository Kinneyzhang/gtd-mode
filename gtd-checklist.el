;;; gtd-checklist.el --- Gtd checklist module  -*- lexical-binding: t; -*-

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

(require 'ewoc)
(require 'gtd-db)
(require 'gtd-faces)
(require 'gtd-utils)

;;;; Variables

(defvar gtd-checklist-buf "*Gtd Checklist*")

(defvar gtd-checklist-default-icon "🗒")

(defvar gtd-current-checklist nil)

(defvar gtd-checklist-mode-map nil)

(defvar gtd-checklists
  '(("Inbox"
     :key "I" :icon "✉️" :color "black")
    ("Next"
     :key "N" :icon "⏰" :color "red")
    ("Someday/Maybe"
     :key "S" :icon "🗓" :color "Blue")
    ("Waiting"
     :key "W" :icon "🤝" :color "cyan"))
  "Basic gtd checklists.")

(defvar gtd-smart-checklists
  `(("All"
     :key "a" :icon "🗂")
    ("Today"
     :key "t" :icon "📆" :rules (date ,(gtd-format-date)))
    ("Tomorrow"
     :key "m" :icon "☀" :rules (date ,(gtd-date-change '+ 1)))
    ("This week"
     :key "w" :rules (date ,(car (gtd-curr-week-range))
                           ,(cdr (gtd-curr-week-range))))
    ("Latest 7 days"
     :key "7" :icon "📅" :rules (date ,(gtd-format-date)
                                      ,(gtd-date-change '+ 6)))
    ("Most important today"
     :key "i" :rules (and (date ,(gtd-format-date))
                          (priority "high"))))
  "Smart checklists.")

(defvar gtd-shown-smart-checklists
  '("Today" "Tomorrow" "Latest 7 days" "This week" "All")
  "The list of smart checklists shown in 'all checklists' view.
The built-in smart checklists are 'All', 'Today', 'Tomorrow',
'Latest 7 days' etc.")

;;;; Functions

(defun gtd-checklists-pp (checklist)
  "Pretty printer for showing all checklists."
  (if checklist
      (let* ((name checklist)
             (key (gtd-checklist-attr name :key))
             (icon (or (gtd-checklist-attr name :icon)
                       gtd-checklist-default-icon)))
        (insert (format "[%s] " key))
        (insert-text-button
         (concat icon " " name)
         'face nil
         'action (lambda (_btn)
                   (gtd-checklist-enter))
         'help-echo "Enter this checklist"
         'follow-link t))
    (insert "")))

;;;###autoload
(defun gtd-show-checklists ()
  "Show all checklists in gtd buffer."
  (interactive)
  (gtd--switch-to-buffer gtd-checklist-buf)
  (let ((ewoc (ewoc-create 'gtd-checklists-pp
                           (propertize "All Checklists\n"
                                       'face 'gtd-header-face)
                           "\nPress the key in front or click checklist button.")))
    (set (make-local-variable 'gtd-ewoc) ewoc)
    (when gtd-shown-smart-checklists
      (dolist (item gtd-shown-smart-checklists)
        (ewoc-enter-last ewoc item)))
    (ewoc-enter-last ewoc nil)
    (when gtd-checklists
      (dolist (item gtd-checklists)
        (ewoc-enter-last ewoc (car item))))
    (gtd-checklist-mode 1)
    (read-only-mode 1)))

;;;###autoload
(defun gtd-checklist-enter ()
  "View the content of the checklist."
  (interactive)
  (let* ((checklist (ewoc-data (ewoc-locate gtd-ewoc))))
    (gtd-show-checklist-tasks checklist)))

;;;###autoload
(define-minor-mode gtd-checklist-mode
  "Minor mode for gtd checklist."
  nil nil nil)

(progn
  (setq gtd-checklist-mode-map (make-sparse-keymap))
  (dolist (data (append gtd-checklists gtd-smart-checklists))
    (let* ((checklist (car data))
           (key (gtd-checklist-attr checklist :key)))
      (define-key gtd-checklist-mode-map (kbd key)
        (lambda ()
          (interactive)
          (gtd-show-checklist-tasks checklist))))))

(provide 'gtd-checklist)
;;; gtd-checklist.el ends here

