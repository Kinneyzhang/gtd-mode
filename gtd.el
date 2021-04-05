;;; gtd.el --- Gtd implement in emacs  -*- lexical-binding: t; -*-

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

;; Gtd-mode is a GTD management system implement in emacs,
;; relaying on database.

;;; Code:

;;;; Dependencies

(require 'ewoc)
(require 'gtd-db)
(require 'gtd-faces)
(require 'gtd-utils)
(require 'gtd-task)
(require 'gtd-calendar)

;;;; Variables

(defgroup gtd nil
  "Gtd implement in emacs.")

(defvar gtd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "B") #'gtd-calendar-show-month)
    (define-key map (kbd "C") #'gtd-show-checklists)
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "g") #'gtd-refresh-buffer)
    (define-key map (kbd "+") #'gtd-add-task)
    map)
  "Keymap for `gtd-mode'")

(defvar gtd-window-margin 4)

(defvar gtd-ewoc nil)

(defvar gtd-data nil)

;; checklist variables

(defvar gtd-checklist-buf "*Gtd Checklist*")

(defvar gtd-checklist-default-icon "🗒")

(defvar gtd-current-checklist nil)

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

(defvar gtd-checklist-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (data (append gtd-checklists gtd-smart-checklists))
      (let* ((checklist (car data))
             (key (gtd-checklist-attr checklist :key)))
        (define-key map (kbd key)
          (lambda ()
            (interactive)
            (gtd-show-checklist-tasks checklist)))))
    (define-key map (kbd "+") #'gtd-add-task))
  "Keymap for `gtd-checklist-mode'")

;;;; Functions

;; checklist functions

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
                           (substitute-command-keys
                            "\n\\{gtd-mode-map}"))))
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

;; main functions
(defun gtd--switch-to-buffer (name)
  "Switch to gtd buffer named NAME and do some settings."
  (switch-to-buffer (get-buffer-create name))
  (read-only-mode -1)
  (kill-all-local-variables)
  (gtd-mode)
  (erase-buffer)
  (buffer-disable-undo)
  (set-window-margins
   (selected-window) gtd-window-margin gtd-window-margin))

;;;###autoload
(defun gtd-refresh-buffer ()
  (interactive)
  (let ((buf (buffer-name)))
    (cond
     ((string= buf gtd-checklist-buf)
      (gtd-show-checklists))
     ((string= buf gtd-calendar-buf)
      (gtd-calendar-show-month gtd-date))
     ((string= buf gtd-task-buf)
      (gtd-show-checklist-tasks gtd-current-checklist)))))

;; major mode

(define-derived-mode gtd-mode fundamental-mode "GTD"
  (use-local-map gtd-mode-map))

(defun gtd-kill-buffer ()
  (interactive)
  (kill-buffer gtd-checklist-buf))

(provide 'gtd)
;;; gtd.el ends here
