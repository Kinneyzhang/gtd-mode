;;; gtd-faces.el --- Face definitions  -*- lexical-binding: t; -*-

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

(defgroup gtd-faces nil
  "Gtd-mode related faces."
  :group 'gtd)

(defface gtd-header-face
  '((t :height 1.6))
  "Faces for header in gtd buffer.")

(defface gtd-calendar-month-header
  '((t :inherit font-lock-function-name-face))
  "Face used for month headers in the calendar."
  :group 'gtd-faces)

(defface gtd-calendar-weekday-header
  '((t :inherit font-lock-constant-face))
  "Face used for weekday column headers in the calendar.
See also the face `gtd-calendar-weekend-header'."
  :group 'gtd-faces)

(defface gtd-calendar-weekend-header
  '((t :inherit font-lock-comment-face))
  "Face used for weekend column headers in the calendar.
See also the face `gtd-calendar-weekday-header'."
  :group 'gtd-faces)


(provide 'gtd-faces)
;;; gtd-faces.el ends here

