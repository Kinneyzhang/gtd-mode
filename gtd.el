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

(require 'gtd-checklist)
(require 'gtd-task)
(require 'gtd-calendar)

;;;; Variables

(defgroup gtd nil
  "Gtd implement in emacs.")

(defvar gtd-window-margin 4)

(defvar gtd-mode-map nil "Keymap for `gtd-mode'")

(defvar gtd-ewoc nil)

(defvar gtd-data nil)

;; (defconst gtd-types '("task" "note"))

;;;; Functions

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

(progn
  (setq gtd-mode-map (make-sparse-keymap))
  (define-key gtd-mode-map (kbd "C") #'gtd-show-checklists)
  (define-key gtd-mode-map (kbd "q") #'kill-current-buffer)
  (define-key gtd-mode-map (kbd "g") #'gtd-refresh-buffer))

(provide 'gtd)
;;; gtd.el ends here
