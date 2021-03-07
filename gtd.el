;;; gtd.el --- purpose

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Kinney Zhang <kinneyzhang666 AT gmail DOT com>
;; URL: http://github.com/Kinneyzhang/gtd
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

(require 'ewoc)
(require 'gtd-db)
(require 'gtd-utils)

(defgroup gtd nil
  "Gtd implement in emacs.")

(defface gtd-header-face
  '((t :height 200))
  "Faces for header in gtd buffer.")

(defvar gtd-buffer "*Gtd*")

(defvar gtd-window-margin 4)

(defvar gtd-mode-map nil "Keymap for `gtd-mode'")

(defvar gtd-ewoc nil)

(defvar gtd-data nil)

(defvar gtd-current-checklist nil)

(defvar gtd-checklist-default-icon "🗒")

;; (defconst gtd-types '("task" "note"))

(defvar gtd-checklists
  '(("Inbox" (:icon "✉️" :color "black"))
    ("Next" (:icon "⏰" :color "red"))
    ("Someday/Maybe" (:icon "🗓" :color "Blue"))
    ("Waiting" (:icon "🤝" :color "cyan")))
  "Basic gtd checklists.")

;; default status is todo
(defvar gtd-smart-default-rules
  '((status . "todo"))
  "Default rules for smart checklists.")

(defvar gtd-smart-checklists
  '(("All"
     (:icon "🗂"))
    ("Today"
     (:icon "📆" :rules '(date "today")))
    ("Tomorrow"
     (:icon "☀" :rules '(date "today+1")))
    ("This week"
     (:rules `(date ,(car (gtd-curr-week-range))
                    ,(cdr (gtd-curr-week-range)))))
    ("Latest 7 days"
     (:icon "📅" :rules '(date "today" "today+7")))
    ("Most important today"
     (:rules '(and (date "today")
                   (priority "high"))))
    ("Done"
     (:icon "✅" :rules '(status "done"))))
  "Smart checklists.")


;; date 前端如何友好的选择不同类型的日期
;; 2021-03-06
;; 2021-03-06 19:30
;; 2021-03-06 ~ 2021-03-09
;; 2021-03-06 17:00 ~ 2021-03-09 17:00

(defvar gtd-shown-smart-checklists
  '("Today" "Tomorrow" "Done")
  "The list of smart checklists shown in 'all checklists' view.
The built-in smart checklists are 'All', 'Today', 'Tomorrow',
'Latest 7 days', 'Done'.")

(defvar gtd-priorities '(("none priority" . 0)
                         ("low priority" . 1)
                         ("middle priority" . 2)
                         ("high priority" . 3)))

(defvar gtd-tags '("work" "home" "free" "telephone" "computer"))

;; eieio

;; (defclass gtd-checklist ()
;;   ((name :initarg :name :type string)
;;    (type :initarg :type :initform "task" :type string)
;;    (icon :initarg :icon :initform "☰" :type string)
;;    (color :initarg :color :initform "grey" :type string)
;;    (category :initarg :folder :initform "default" :type string))
;;   "A class for processing checklist.")

;; (cl-defmethod gtd-checklist--add-checklist ((cklist checklist))
;;   "Add a new checklist."
;;   (let ((name (oref cklist name))
;;         (type (oref cklist type))
;;         (icon (oref cklist icon))
;;         (color (oref cklist color))
;;         (category (oref cklist category)))
;;     (gtd-checklist-db-add-gtd-checklist name type icon color category)))

;; gtd-checklist functions

(defun gtd--switch-to-buffer ()
  "Switch to gtd buffer and do some settings."
  (switch-to-buffer (get-buffer-create gtd-buffer))
  (read-only-mode -1)
  (kill-all-local-variables)
  (gtd-mode)
  (erase-buffer)
  (buffer-disable-undo)
  (set-window-margins
   (selected-window) gtd-window-margin gtd-window-margin))

;; (defface gtd-fringe-face
;;   '((t :inherit fringe)))

(defun gtd-checklists-pp (checklist)
  "Pretty printer for showing all checklists."
  (if checklist
      (let* ((name checklist)
             (icon (or (gtd-checklist-attr name :icon)
                       gtd-checklist-default-icon)))
        (insert (concat icon " " name)))
    (insert "")))

;;;###autoload
(defun gtd-show-checklists ()
  "Show all checklists in gtd buffer."
  (interactive)
  (gtd--switch-to-buffer)
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
    (read-only-mode 1)))

;;;###autoload
(defun gtd-refresh-buffer ()
  "Refresh gtd buffer."
  (interactive)
  (ewoc-refresh gtd-ewoc))

;;;###autoload
(defun gtd-checklist-enter ()
  "View the content of the checklist."
  (interactive)
  (let* ((checklist (ewoc-data (ewoc-locate gtd-ewoc))))
    (gtd-show-tasks checklist)))

;;;###autoload
(defun gtd-task-enter ()
  "View the details of the task."
  (interactive)
  )

;;;###autoload
(defun gtd-enter ()
  (interactive)
  )

;; task

(defclass gtd-task ()
  ((id :initarg :id :initform (org-id-uuid) :type string)
   (name :initarg :name :initform "Default task" :type string)
   (status :initarg :status :initform 0 :type number)
   (date :initarg :date :initform nil :type (or null string))
   (tags :initarg :tags :initform nil :type (or null list))
   (priority :initarg :priority :initform 0 :type number)
   (checklist :initarg :checklist :initform "Inbox" :type string)
   (memo :initarg :memo :initform nil :type (or null string))
   (parent :initarg :parent :initform nil :type (or null string)))
  "A class for processing task.")

(defvar gtd-task-slots
  '(:id :name :status :date :tags
        :priority :checklist :memo :parent))

(cl-defmethod gtd-task--add ((task gtd-task) task-args)
  (let* ((id (oref task :id))
         (name (oref task :name))
         (status (oref task :status))
         (date (oref task :date))
         (tags (oref task :tags))
         (priority (oref task :priority))
         (checklist (oref task :checklist))
         (memo (oref task :memo))
         (parent (oref task :parent))
         (args `[,id ,name ,status ,date ,tags
                     ,priority ,checklist ,memo ,parent])
         new-node)
    (gtd-db-add-task args)
    (gtd-data-update)
    (setq new-node (ewoc-enter-last gtd-ewoc id))
    (ewoc-invalidate gtd-ewoc new-node)))

(cl-defmethod gtd-task--finish ((task gtd-task))
  (let ((id (oref task :id))
        (node (ewoc-locate gtd-ewoc)))
    (gtd-db-finish-task id)
    (gtd-data-update)
    (ewoc-invalidate gtd-ewoc node)))

;; 写数据库 -> 从数据库获取和更新 gtd-data -> 更新视图

(defun gtd-tasks-pp (id)
  "Pretty printer for showing all tasks in a checklist."
  (when id
    (let* ((item (gtd-task-args id gtd-data))
           (task (apply #'gtd-task item))
           (status (oref task :status))
           (name (oref task :name))
           (date (oref task :date))
           (checklist (oref task :checklist))
           (tags (oref task :tags))
           (icon (gtd-checklist-attr checklist :icon))
           (color (gtd-checklist-attr checklist :color)))
      (insert (propertize (format "%s %s" (if (= status 0) "☐" "☑") name)
                          'line-prefix
                          (propertize
                           "▎" 'face `(:foreground ,color)))))))

;;;###autoload
(defun gtd-show-tasks (&optional checklist)
  "Show all tasks in a specific CHECKLIST."
  (interactive)
  (let* ((checklist (or checklist
                        (completing-read "Choose a checklist: "
                                         (gtd-checklists-attrs) nil t)))
         (_ (gtd--switch-to-buffer))
         (icon (or (gtd-checklist-attr checklist :icon)
                   gtd-checklist-default-icon))
         (tasks (cond
                 ((assoc checklist gtd-checklists)
                  (gtd-db-retrive-tasks checklist))
                 ((assoc checklist gtd-smart-checklists)
                  (gtd-db-retrive-tasks-smartly
                   (gtd-task-conditions-smartly checklist)))))
         (ewoc (ewoc-create 'gtd-tasks-pp
                            (propertize (format "%s %s\n" icon checklist)
                                        'face 'gtd-header-face)
                            (substitute-command-keys
                             "\n\\{gtd-mode-map}")))
         task-args task-args-lst)
    (set (make-local-variable 'gtd-current-checklist) checklist)
    (when tasks
      (dolist (task tasks)
        (setq task-args (gtd-construct-args gtd-task-slots task))
        (push task-args task-args-lst))
      (setq task-args-lst (reverse task-args-lst))
      (set (make-local-variable 'gtd-ewoc) ewoc)
      (set (make-local-variable 'gtd-data) task-args-lst)
      (dolist (task tasks)
        (ewoc-enter-last ewoc (car task))))
    (read-only-mode 1)))

(defun gtd-data-update ()
  "Update the value of `gtd-data' in ewoc."
  (let ((tasks (gtd-db-retrive-tasks gtd-current-checklist))
        task-args task-args-lst)
    (dolist (task tasks)
      (setq task-args (gtd-construct-args gtd-task-slots task))
      (push task-args task-args-lst))
    (setq gtd-data task-args-lst)))

;;;###autoload
(defun gtd-add-task ()
  "Add a task."
  (interactive)
  (let ((name (completing-read "Input the task name: " nil))
        (date (completing-read "Input the task date: " nil))
        (priority (completing-read "Choose the task priority: "
                                   (mapcar #'car gtd-priorities) nil t))
        (tags (completing-read-multiple "Choose the task tags: "
                                        gtd-tags nil t))
        (checklist (or gtd-current-checklist
                       (completing-read "Choose the task checklist: "
                                        (gtd-checklists-attrs) nil t)))
        memo parent task-args)
    (setq priority (cdr (assoc priority gtd-priorities)))
    (setq task-args
          (gtd-construct-args
           gtd-task-slots
           `(,(org-id-uuid) ,name 0 ,date ,tags
             ,priority ,checklist ,memo ,parent)))
    (gtd-task--add (apply #'gtd-task task-args) task-args)))

;;;###autoload
(defun gtd-finish-task ()
  "Finish the task at point."
  (interactive)
  (let* ((id (ewoc-data (ewoc-locate gtd-ewoc)))
         (args (gtd-task-args id gtd-data)))
    (gtd-task--finish (apply #'gtd-task args))))

(defun gtd-task-args (id task-lst)
  "Return the task with id ID in a TASK-LIST."
  (seq-find (lambda (lst)
              (string= (plist-get lst :id) id))
            task-lst))

;; task functions

;; major mode

(define-derived-mode gtd-mode fundamental-mode "GTD"
  (use-local-map gtd-mode-map))

(progn
  (setq gtd-mode-map (make-sparse-keymap))
  (define-key gtd-mode-map (kbd "C") #'gtd-show-checklists)
  (define-key gtd-mode-map (kbd "q") #'kill-current-buffer)
  (define-key gtd-mode-map (kbd "g") #'gtd-refresh-buffer)
  (define-key gtd-mode-map (kbd "<RET>") #'gtd-checklist-enter)
  (define-key gtd-mode-map (kbd "+") #'gtd-add-task)
  (define-key gtd-mode-map (kbd "d") #'gtd-finish-task))

(provide 'gtd)
;;; gtd.el ends here
