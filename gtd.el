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
(require 'gtd-habit)
(require 'gtd-note)

;;;; Variables

(defgroup gtd nil
  "Gtd implement in emacs.")

(defvar gtd-multilingual-words
  '(("task" :zh-cn "任务")
    ("note" :zh-cn "笔记")
    ("habit" :zh-cn "习惯")
    ("today" :zh-cn "今天")
    ("tomorrow" :zh-cn "明天")
    ("next Monday" :zh-cn "下周一")
    ("other" :zh-cn "其它")
    
    ;; habit
    ("by day" :zh-cn "按天")
    ("by week" :zh-cn "按周")
    ("by period" :zh-cn "按时间间隔")

    ("Monday" :zh-cn "周一")
    ("Tuesday" :zh-cn "周二")
    ("Wednesday" :zh-cn "周三")
    ("Thursday" :zh-cn "周四")
    ("Friday" :zh-cn "周五")
    ("Saturday" :zh-cn "周六")
    ("Sunday" :zh-cn "周日")

    ("Choose or input the habit name" :zh-cn "选择或输入习惯名称")
    ("Choose the frequency type of the habit" :zh-cn "选择习惯打卡的频率类型")
    ("In these days" :zh-cn "在这些天")
    ("How many days each week" :zh-cn "一周几天")
    ("Every few days" :zh-cn "每隔几天")
    ("How many times each day" :zh-cn "每天几次")
    
    ("early to bed" :zh-cn "早睡")
    ("early to rise" :zh-cn "早起")
    ("drink water" :zh-cn "喝水")
    ("learn new words" :zh-cn "背单词")
    ("get news updates" :zh-cn "看新闻")
    ("walk the dog" :zh-cn "遛狗")
    ("be a good cat keeper" :zh-cn "做铲屎官")
    ("eat breakfast" :zh-cn "吃早餐")
    ("stretch" :zh-cn "拉伸")
    ("reading" :zh-cn "阅读")
    ("self-reflection" :zh-cn "自我反思")
    ("plan your day" :zh-cn "计划一天")
    ("running" :zh-cn "跑步")
    ("eat fruits" :zh-cn "吃水果")
    ("yoga" :zh-cn "瑜伽")
    ("track expenses" :zh-cn "记账")
    ("keep a diary" :zh-cn "写日记")
    ("eat veggies" :zh-cn "吃蔬菜")
    ("do push-ups" :zh-cn "做俯卧撑")
    ("quit snacks" :zh-cn "戒零食")
    ("quit sugar" :zh-cn "戒糖")
    ("quit alcohol" :zh-cn "戒酒")
    ("go cycling" :zh-cn "骑行")
    ("no dirty words" :zh-cn "不说脏话")
    ("swimming" :zh-cn "游泳")
    ("connect a loved one" :zh-cn "保持联系")
    ("take a walk" :zh-cn "散步")
    ("learn musical instruments" :zh-cn "学习乐器")
    ("meditate" :zh-cn "冥想")
    ("take a shower" :zh-cn "洗澡")
    ("water flowers" :zh-cn "浇花")
    ("quit smoking" :zh-cn "戒烟")
    ("watch a movie" :zh-cn "看电影")
    ("take medicine" :zh-cn "吃药")
    ("clean up" :zh-cn "打扫卫生")
    ("do housework" :zh-cn "做家务")
    ("reduce screen time" :zh-cn "少看手机")
    ("do skincare" :zh-cn "护肤")
    ("smile to yourself" :zh-cn "面带微笑")
    ("no video games" :zh-cn "禁游戏")

    ("Choose a type" :zh-cn "选择类型")
    ("Input the task name" :zh-cn "请输入任务名称")
    ("Input the task date" :zh-cn "请选择任务日期")
    ("Choose the task priority" :zh-cn "请选择任务优先级")
    ("Choose the task tags" :zh-cn "请选择任务标签")
    ("Choose the task checklist" :zh-cn "请选择任务所属清单")))

(defvar gtd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "B") #'gtd-calendar-show-month)
    (define-key map (kbd "C") #'gtd-show-checklists)
    (define-key map (kbd "q") #'kill-current-buffer)
    (define-key map (kbd "g") #'gtd-refresh-buffer)
    (define-key map (kbd "+") #'gtd-add-task)
    map)
  "Keymap for `gtd-mode'")

(defvar gtd-types
  '("task" "habit" "note"))

(defvar gtd-dates
  '("today" "tomorrow" "other"))

(defvar gtd-chinese-p nil
  "Non-nil means translating all keywords in gtd-mode to chinese.")

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

;;;###autoload
(defun gtd-new ()
  "Create a new gtd task, habit or note."
  (interactive)
  (let* ((type (gtd-completing-read "Choose a type" gtd-types nil t)))
    (pcase type
      ("task" (gtd-add-task))
      ("habit" (gtd-habit-new))
      ("note" (gtd-note-new)))))

(provide 'gtd)
;;; gtd.el ends here
