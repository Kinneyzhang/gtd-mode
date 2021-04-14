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

;;;; Requires

(require 'gtd-utils)

;;;; Variables

(defgroup gtd-habit nil
  "Habit of gtd."
  :group 'gtd)

(defvar gtd-habit-buf "*Gtd Habit*")

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

(defvar-local gtd-date nil)

(defvar gtd-habit-show-archived t)

;;;; Functions

(defun gtd-habit-pp (data)
  "Pretty printer for gtd habit."
  (pcase data
    ((and (pred listp)
          (let date (plist-get data :date))
          (guard date))
     (insert (propertize date 'face '(:height 1.2))))
    ((and (pred listp)
          (guard (org-uuidgen-p (car-safe data))))
     (let* ((id (nth 0 data))
            (habit (nth 1 data))
            (frequency-type (nth 2 data))
            (frequency-value (nth 3 data))
            (goal (string-to-number (nth 4 data)))
            (is_archived (nth 8 data))
            (count (caar (gtd-db-query
                          `[:select (funcall count habit)
                                    :from habit-record
                                    :where (= habit ,id)])))
            (is_done (= goal count)))
       (insert
        (propertize (format "[%s/%s] %s" count goal habit)
                    'line-prefix
                    (propertize (if is_done "✅ " "⬜ ")
                                'display '((height 0.8)))))))
    ((pred stringp)
     (insert (propertize data 'face '(bold :height 1.1))))
    (_ (insert ""))))

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
    (gtd-db-query `[:insert :into habit :values (,db-vec)])
    (gtd-habit-refresh)))

;;;###autoload
(defun gtd-habit-archive ()
  "Archive the habit at point."
  (interactive)
  (let* ((node (ewoc-locate gtd-ewoc))
         (id (nth 0 (ewoc-data node))))
    (gtd-db-query `[:update habit :set (= is_archived 1)
                            :where (= id ,id)])
    (gtd-habit-refresh)))

;;;###autoload
(defun gtd-habit-active ()
  "Active the habit at point."
  (interactive)
  (let* ((id (nth 0 (gtd-ewoc-data))))
    (gtd-db-query `[:update habit :set (= is_archived 0)
                            :where (= id ,id)])
    (gtd-habit-refresh)))

;;;###autoload
(defun gtd-habit-delete ()
  "Delete the habit at point."
  (interactive)
  (let* ((node (ewoc-locate gtd-ewoc))
         (data (ewoc-data node))
         (id (nth 0 data)))
    (if (y-or-n-p "Do you want to delete this habit\
 and all the records?")
        (progn
          (let ((inhibit-read-only 1))
            (ewoc-delete gtd-ewoc node))
          (gtd-db-query `[:delete :from habit
                                  :where (= id ,id)]))
      (message ""))))

;;;###autoload
(defun gtd-habit-show (&optional date)
  "Show gtd habits on date DATE."
  (interactive)
  (gtd--switch-to-buffer gtd-habit-buf)
  (let* ((date (or date (gtd-format-date)))
         (slash-date (replace-regexp-in-string "-" "/" date))
         (seconds (gtd-date-to-seconds date))
         (week (format-time-string "%a" seconds))
         (full-week (format-time-string "%A" seconds))
         (ewoc (ewoc-create 'gtd-habit-pp
                            (propertize "🌀 Habit Record\n"
                                        'face 'gtd-header-face)
                            (substitute-command-keys
                             "\n\\{gtd-habit-mode-map}")))
         (next-seconds (gtd-date-to-seconds (gtd-date-change '+ 1 date)))
         (habits (gtd-db-query `[:select * :from habit
                                         :where (< timestamp ,next-seconds)]))
         (habits (seq-filter (lambda (habit)
                               (let* ((frequency-type (nth 2 habit))
                                      (frequency-value (nth 3 habit))
                                      (timestamp (nth 7 habit))
                                      (date (gtd-seconds-to-date timestamp))
                                      (date-seconds (gtd-date-to-seconds date)))
                                 (pcase frequency-type
                                   ("by day"
                                    (member full-week frequency-value))
                                   ("by period"
                                    (= (% (floor (- seconds date-seconds))
                                          (* (string-to-number frequency-value)
                                             86400))
                                       0))
                                   (_ t))))
                             habits))
         (grouped-habits (seq-group-by
                          (lambda (habit) (nth 8 habit))
                          habits))
         (active-habits (cdr (assoc 0 grouped-habits)))
         (archived-habits (cdr (assoc 1 grouped-habits))))
    (message "seconds: %s" seconds)
    (ewoc-enter-last ewoc `(:date ,(concat slash-date " " week "\n")))
    (dolist (habit active-habits)
      (ewoc-enter-last ewoc habit))
    (when gtd-habit-show-archived
      (ewoc-enter-last ewoc "\nArchived Habits:")
      (dolist (habit archived-habits)
        (ewoc-enter-last ewoc habit)))
    (set (make-local-variable 'gtd-ewoc) ewoc)
    (set (make-local-variable 'gtd-date) date)
    (gtd-habit-mode 1)
    (read-only-mode 1)))

;;;###autoload
(defun gtd-habit-record ()
  "Record the habit at point."
  (interactive)
  (let* ((node (ewoc-locate gtd-ewoc))
         (data (ewoc-data node))
         (habit-id (nth 0 data))
         (goal (string-to-number (nth 4 data)))
         (count
          (caar (gtd-db-query `[:select (funcall count habit)
                                        :from habit-record
                                        :where (= habit ,habit-id)])))
         timestamp comment)
    (if (< count goal)
        (progn
          (setq timestamp (gtd-time-to-seconds (gtd-format-time)))
          (setq comment (gtd-completing-read "Say something" nil))
          (gtd-db-query `[:insert :into habit-record
                                  :values ([,timestamp ,habit-id ,comment])])
          (ewoc-invalidate gtd-ewoc node))
      (message "The habit has been finished!"))))

;;;###autoload
(defun gtd-habit-withdraw-record ()
  "Withdraw the last record of habit at point."
  (interactive)
  (let ((habit-id (nth 0 (gtd-ewoc-data))))
    (gtd-db-query `[:delete :from habit-record
                            :where (= habit ,habit-id)
                            :order-by (desc timestamp)
                            :limit 1])
    (ewoc-invalidate gtd-ewoc (gtd-ewoc-node))))

;;;###autoload
(defun gtd-habit-archived-toggle ()
  "Toggle function for showing archived habit or not."
  (interactive)
  (if gtd-habit-show-archived
      (setq gtd-habit-show-archived nil)
    (setq gtd-habit-show-archived t))
  (gtd-habit-show gtd-date))

;;;###autoload
(defun gtd-habit-previous-date ()
  "Show the habits of previous date."
  (interactive)
  (let ((date (gtd-date-change '- 1 gtd-date)))
    (gtd-habit-show date)
    (setq gtd-date date)))

;;;###autoload
(defun gtd-habit-next-date ()
  "Show the habits of next date."
  (interactive)
  (let ((date (gtd-date-change '+ 1 gtd-date)))
    (gtd-habit-show date)
    (setq gtd-date date)))

;;;###autoload
(defun gtd-habit-current-date ()
  "Show the habits of current date."
  (interactive)
  (gtd-habit-show (gtd-format-date)))

;;;###autoload
(defun gtd-habit-refresh ()
  "Refresh `gtd-habit-buf' buffer."
  (interactive)
  (gtd-habit-show gtd-date))

;;;###autoload
(define-minor-mode gtd-habit-mode
  "Minor mode for gtd habit."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "N") #'gtd-habit-new)
            (define-key map (kbd "A") #'gtd-habit-archive)
            (define-key map (kbd "a") #'gtd-habit-active)
            (define-key map (kbd "D") #'gtd-habit-delete)
            (define-key map (kbd "d") #'gtd-habit-record)
            (define-key map (kbd "u") #'gtd-habit-withdraw-record)
            (define-key map (kbd "<") #'gtd-habit-previous-date)
            (define-key map (kbd ">") #'gtd-habit-next-date)
            (define-key map (kbd "T") #'gtd-habit-archived-toggle)
            (define-key map (kbd "g") #'gtd-habit-refresh)
            (define-key map (kbd ".") #'gtd-habit-current-date)
            map))

(provide 'gtd-habit)
;;; gtd-habit.el ends here
