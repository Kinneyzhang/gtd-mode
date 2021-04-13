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

;;;; Variables

(defgroup gtd-habit nil
  "Habit of gtd."
  :group 'gtd)

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

(defvar gtd-habit-multilingual
  '(("by day" :zh-cn "按天")
    ("by week" :zh-cn "按周")
    ("by period" :zh-cn "按时间间隔")

    ("Monday" :zh-cn "周一")
    ("Tuesday" :zh-cn "周二")
    ("Wednesday" :zh-cn "周三")
    ("Thursday" :zh-cn "周四")
    ("Friday" :zh-cn "周五")
    ("Saturday" :zh-cn "周六")
    ("Sunday" :zh-cn "周日")

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

    ("Choose or input the habit name" :zh-cn "选择或输入习惯名称")
    ("Choose the frequency type of the habit" :zh-cn "选择习惯打卡的频率类型")
    ("In these days" :zh-cn "在这些天")
    ("How many days each week" :zh-cn "一周几天")
    ("Every few days" :zh-cn "每隔几天")
    ("How many times each day" :zh-cn "每天几次")
    ("Time to remind (separated by comma, eg: '09:00,18:30')"
     :zh-cn "提醒时间（用逗号隔开，例：'09:00,18:30'）")
    ("Sentence to remind" :zh-cn "提醒语句")))

;;;; Functions

;;;###autoload
(defun gtd-habit-new ()
  "Create a new habit."
  (interactive)
  (let* ((id (org-id-uuid))
         (habit (gtd-completing-read
                 "Choose or input the habit name" gtd-habits))
         (frequency-type (gtd-completing-read
                          "Choose the frequency type of the habit"
                          gtd-habit-frequency nil t))
         (frequency-value
          (pcase frequency-type
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
                "How many times each day" nil))
         (remind-time (gtd-completing-read-multiple
                       "Time to remind (separated by comma, eg: '09:00,18:30')" nil))
         (remind-string (gtd-completing-read
                         "Sentence to remind" nil))
         (timestamp (gtd-current-seconds))
         (db-vec `[,id ,habit ,frequency-type ,frequency-value ,goal
                       ,remind-time ,remind-string ,timestamp 0]))
    (when (gtd-db-query `[:insert :into habit :values (,db-vec)])
      (message "Habit '%s' is added successfully!" habit))))

(defvar gtd-habit-buf "*Gtd Habit*")

(defun gtd-habit-show (&optional date)
  "Show gtd habits on date DATE."
  (interactive)
  (gtd--switch-to-buffer gtd-habit-buf)
  (let* ((date (or date (gtd-format-date)))
         (ewoc (ewoc-create 'gtd-habit-pp
                            (propertize "Habit Record\n"
                                        'face 'gtd-header-face)
                            (substitute-command-keys
                             "\n\\{gtd-habit-mode-map}")))
         (seconds (gtd-date-to-seconds (gtd-date-change '+ 1 date)))
         (ids (mapcar #'car (gtd-db-query `[:select id :from habit
                                                    :where (< timestamp ,seconds)]))))
    (ewoc-enter-last ewoc date)
    (dolist (id ids)
      (ewoc-enter-last ewoc id))
    (set (make-local-variable 'gtd-ewoc) ewoc)
    (set (make-local-variable 'gtd-date) date)
    (gtd-habit-mode 1)
    (read-only-mode 1)))

(defun gtd-habit-pp (data)
  "Pretty printer for gtd habit."
  (pcase data
    ((pred org-uuidgen-p)
     (let ((habit (caar (gtd-db-query
                         `[:select name :from habit
                                   :where (= id ,data)]))))
       (insert habit)))
    (_ (insert (propertize data 'face '(bold :height 1.2)) "\n"))))

(defun gtd-habit-next-date ()
  (interactive)
  (let* ((curr-date gtd-date)
         (date (gtd-date-change '+ 1 curr-date)))
    (setq gtd-date date)
    (gtd-habit-show date)))

(defvar-local gtd-date nil)

(defun gtd-habit-previous-date ()
  (interactive)
  (let* ((curr-date gtd-date)
         (date (gtd-date-change '- 1 curr-date)))
    (setq gtd-date date)
    (gtd-habit-show date)))

(defvar gtd-habit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ">") #'gtd-habit-next-date)
    (define-key map (kbd "<") #'gtd-habit-previous-date)
    map))

(define-minor-mode gtd-habit-mode
  "Minor mode for gtd habit."
  nil nil nil)

(provide 'gtd-habit)
;;; gtd-habit.el ends here
