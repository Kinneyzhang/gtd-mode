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
  '(("Inbox" :icon "✉️" :color "black")
    ("Next" :icon "⏰" :color "red")
    ("Someday/Maybe" :icon "🗓" :color "Blue")
    ("Waiting" :icon "🤝" :color "cyan"))
  "Basic gtd checklists.")

;; default status is todo
(defvar gtd-smart-default-rules
  '((status . "todo"))
  "Default rules for smart checklists.")

(defvar gtd-smart-checklists
  `(("All"
     :icon "🗂")
    ("Today"
     :icon "📆" :rules (date ,(gtd-format-date)))
    ("Tomorrow"
     :icon "☀" :rules (date ,(gtd-date-change '+ 1)))
    ("This week"
     :rules (date ,(car (gtd-curr-week-range))
                  ,(cdr (gtd-curr-week-range))))
    ("Latest 7 days"
     :icon "📅" :rules (date ,(gtd-format-date) ,(gtd-date-change '+ 6)))
    ("Most important today"
     :rules (and (date ,(gtd-format-date))
                 (priority "high"))))
  "Smart checklists.")


;; date 前端如何友好的选择不同类型的日期
;; 2021-03-06
;; 2021-03-06 19:30
;; 2021-03-06 ~ 2021-03-09
;; 2021-03-06 17:00 ~ 2021-03-09 17:00

(defvar gtd-shown-smart-checklists
  '("Today" "Tomorrow" "Latest 7 days" "This week" "All")
  "The list of smart checklists shown in 'all checklists' view.
The built-in smart checklists are 'All', 'Today', 'Tomorrow',
'Latest 7 days' etc.")

(defvar gtd-priorities
  '(("none priority" :id 0 :symbol "!!!" :color "grey")
    ("low priority" :id 1 :symbol "!" :color "blue")
    ("middle priority" :id 2 :symbol "!!" :color "yellow")
    ("high priority" :id 3 :symbol "!!!" :color "red")))

(defvar gtd-tags
  '(("work" :foreground "white" :background "red")
    ("home" :foreground "white" :background "red")
    ("free" :foreground "white" :background "blue")
    ("telephone" :foreground "white" :background "blue")
    ("computer" :foreground "white" :background "green")))

(defvar gtd-task-default-details '(date)
  "The default details that are shown for a task.")

(defvar gtd-show-all-details nil
  "Non-nil means show all details of a task.")

(defvar gtd-show-all-finished nil
  "Non-nil means show all finished tasks.")

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

(defun gtd--switch-to-buffer (name)
  "Switch to gtd buffer named NAME and do some settings."
  (switch-to-buffer (get-buffer-create (format "*%s*" name)))
  (read-only-mode -1)
  (kill-all-local-variables)
  (gtd-mode)
  (erase-buffer)
  (buffer-disable-undo)
  (set-window-margins
   (selected-window) gtd-window-margin gtd-window-margin))

(defun gtd-checklists-pp (checklist)
  "Pretty printer for showing all checklists."
  (if checklist
      (let* ((name checklist)
             (icon (or (gtd-checklist-attr name :icon)
                       gtd-checklist-default-icon)))
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
  (gtd--switch-to-buffer "Gtd Checklists")
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

;; task

(defclass gtd-task ()
  ((id :initarg :id :initform (org-id-uuid) :type string)
   (name :initarg :name :initform "Default task" :type string)
   (status :initarg :status :initform 0 :type number)
   (date :initarg :date :initform nil :type (or null number))
   (tags :initarg :tags :initform nil :type (or null list))
   (priority :initarg :priority :initform 0 :type number)
   (checklist :initarg :checklist :initform "Inbox" :type string)
   (memo :initarg :memo :initform nil :type (or null string))
   (parent :initarg :parent :initform nil :type (or null string))
   (children :initarg :children :initform nil :type (or null list)))
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
         (children (oref task :children))
         (args `[,id ,name ,status ,date ,tags
                     ,priority ,checklist ,memo ,parent ,children])
         new-node)
    (gtd-db-query `[:insert :into task
                            :values (,args)])
    (gtd-data-update)
    (setq new-node (ewoc-enter-last gtd-ewoc id))
    (ewoc-invalidate gtd-ewoc new-node)))

(cl-defmethod gtd-task--finish ((task gtd-task))
  (let ((id (oref task :id)))
    (gtd-db-query `[:update task :set (= status 1)
                            :where (= id ,id)])
    (gtd-data-update)
    (gtd-show-tasks gtd-current-checklist)))

(cl-defmethod gtd-task--withdraw ((task gtd-task))
  (let ((id (oref task :id)))
    (gtd-db-query `[:update task :set (= status 0)
                            :where (= id ,id)])
    (gtd-db-query `[:update task :set (= status 0)
                            :where (= id ,id)])
    (gtd-data-update)
    (gtd-show-tasks gtd-current-checklist)))

(cl-defmethod gtd-task--rename ((task gtd-task))
  (let ((id (oref task :id))
        (name (oref task :name))
        (node (ewoc-locate gtd-ewoc)))
    (gtd-db-query `[:update task :set (= name ,name)
                            :where (= id ,id)])
    (gtd-data-update)
    (ewoc-invalidate gtd-ewoc node)))

(cl-defmethod gtd-task--delete ((task gtd-task))
  (let ((id (oref task :id))
        (node (ewoc-locate gtd-ewoc)))
    (gtd-db-query `[:delete :from task
                            :where (= id ,id)])
    (gtd-data-update)
    (let ((inhibit-read-only t))
      (ewoc-delete gtd-ewoc node))))

(cl-defmethod gtd-task--edit-memo ((task gtd-task))
  (let ((id (oref task :id))
        (memo (oref task :memo))
        (node (ewoc-locate gtd-ewoc)))
    (gtd-db-query `[:update task :set (= memo ,memo)
                            :where (= id ,id)])
    (gtd-data-update)
    (ewoc-invalidate gtd-ewoc node)))

;; 写数据库 -> 从数据库获取和更新 gtd-data -> 更新视图

(defun gtd-uuid-p (str)
  "Judge if the STR is a uuid."
  (and (stringp str)
       (string-match "[a-z0-9]\\{8\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{4\\}-[a-z0-9]\\{12\\}" str)))

(defun gtd-tasks-pp (id)
  "Pretty printer for showing all tasks in a checklist."
  (pcase id
    ((pred org-uuidgen-p)
     (let* ((item (gtd-task-args id gtd-data))
            (task (apply #'gtd-task item))
            (status (oref task :status))
            (name (oref task :name))
            (date (oref task :date))
            (checklist (oref task :checklist))
            (priority (oref task :priority))
            (tags (oref task :tags))
            (memo (oref task :memo))
            (icon (gtd-checklist-attr checklist :icon))
            (color (gtd-checklist-attr checklist :color)))
       (if (= status 1)
           (progn
             (insert-text-button
              (propertize "☑" 'line-prefix (propertize
                                            "▎" 'face `(:foreground ,color)))
              'face 'shadow 'action (lambda (_btn) (gtd-task-finish-toggle))
              'help-echo "Click to change the task status"
              'follow-link t)
             (insert "  " (propertize name 'face 'shadow))
             (when (and date (or gtd-show-all-details
                                 (member 'date gtd-task-default-details)))
               (insert
                " " (propertize (gtd-task-date-format
                                 (gtd-seconds-to-date date))
                                'display '(raise 0.1)
                                'face 'shadow)))
             (when (and tags (or gtd-show-all-details
                                 (member 'tags gtd-task-default-details)))
               (insert "  ")
               (dolist (tag tags)
                 (insert (propertize (concat "#" tag) 'face 'shadow) " ")))
             (when (and priority (or gtd-show-all-details
                                     (member 'priority gtd-task-default-details)))
               (insert "  ")
               (let ((symbol (gtd-common-attr :id priority gtd-priorities :symbol))
                     (color (gtd-common-attr :id priority gtd-priorities :color)))
                 (insert (propertize symbol 'face 'shadow))))
             (when (and memo (or gtd-show-all-details
                                 (member 'memo gtd-task-default-details)))
               (insert (propertize (concat "\n   " memo)
                                   'line-prefix (propertize
                                                 "▎" 'face
                                                 `(:foreground ,color))
                                   'face 'shadow))))
         (insert-text-button
          (propertize (if (= status 0) "☐" "☑")
                      'line-prefix (propertize
                                    "▎" 'face `(:foreground ,color)))
          'face nil
          'action (lambda (_btn) (gtd-task-finish-toggle))
          'help-echo "Click to change the task status"
          'follow-link t)
         (insert "  ")
         (insert-text-button
          name
          'face nil
          'action (lambda (_btn) (gtd-edit-task-details))
          'help-echo "Click to edit the task details"
          'follow-link t)
         (when (and date (or gtd-show-all-details
                             (member 'date gtd-task-default-details)))
           (insert
            " " (propertize (gtd-task-date-format (gtd-seconds-to-date date))
                            'display '(raise 0.1)
                            'face '(:foreground "red" :height 0.8))))
         (when (and tags (or gtd-show-all-details
                             (member 'tags gtd-task-default-details)))
           (insert "  ")
           (dolist (tag tags)
             (insert (propertize (concat "#" tag) 'face
                                 ;; (plist-put
                                 ;;  (cadr (assoc tag gtd-tags))
                                 ;;  :box '(:line-width
                                 ;;         1
                                 ;;         :color "white"))
                                 '(:foreground "blue")
                                 )
                     " ")))
         (when (and priority (or gtd-show-all-details
                                 (member 'priority gtd-task-default-details)))
           (insert "  ")
           (let ((symbol (gtd-common-attr :id priority gtd-priorities :symbol))
                 (color (gtd-common-attr :id priority gtd-priorities :color)))
             (insert (propertize symbol 'face `(:foreground ,color)))))
         (when (and memo (or gtd-show-all-details
                             (member 'memo gtd-task-default-details)))
           (insert (propertize (concat "\n   " memo)
                               'line-prefix (propertize
                                             "▎" 'face
                                             `(:foreground ,color))
                               'face '(:foreground "#888")))))))
    ((pred stringp)
     (insert (propertize id 'face 'bold)))))

;; show all finished tasks
;; gtd-show-all-finished

;;;###autoload
(defun gtd-show-tasks (checklist)
  "Show all tasks in a specific CHECKLIST."
  (interactive)
  (let* ((_ (gtd--switch-to-buffer "Gtd Tasks"))
         (icon (or (gtd-checklist-attr checklist :icon)
                   gtd-checklist-default-icon))
         (tasks (cond
                 ((assoc checklist gtd-checklists)
                  (gtd-db-query `[:select * :from task
                                          :where (= checklist ,checklist)]))
                 ((assoc checklist gtd-smart-checklists)
                  (let ((rules (gtd-checklist-attr checklist :rules)))
                    (if rules
                        (gtd-db-query
                         `[:select * :from task
                                   :where ,(gtd--parse-rules rules)])
                      (gtd-db-query `[:select * :from task]))))))
         (group-tasks (seq-group-by (lambda (lst) (nth 2 lst)) tasks))
         (todo-tasks (cdr (assoc 0 group-tasks)))
         (finished-tasks (cdr (assoc 1 group-tasks)))
         (ewoc (ewoc-create 'gtd-tasks-pp
                            (propertize (format "%s %s\n" icon checklist)
                                        'face 'gtd-header-face)
                            (substitute-command-keys
                             "\n\\{gtd-mode-map}")))
         task-args task-args-lst pos)
    (set (make-local-variable 'gtd-ewoc) ewoc)
    (set (make-local-variable 'gtd-task-default-details)
         gtd-task-default-details)
    (set (make-local-variable 'gtd-current-checklist) checklist)
    (if (not tasks)
        (ewoc-enter-last ewoc "Press + to add a task.")
      (dolist (task tasks)
        (setq task-args (gtd-construct-args gtd-task-slots task))
        (push task-args task-args-lst))
      (setq task-args-lst (reverse task-args-lst))
      (set (make-local-variable 'gtd-ewoc) ewoc)
      (set (make-local-variable 'gtd-data) task-args-lst)
      ;; (message "todo: %S" todo-tasks)
      ;; (message "done: %S" finished-tasks)
      (if todo-tasks
          (dolist (task todo-tasks)
            (ewoc-enter-last ewoc (car task)))
        (ewoc-enter-last ewoc "Press + to add a task."))
      (when gtd-show-all-finished
        (when finished-tasks
          (ewoc-enter-last ewoc "\nDONE")
          (dolist (task finished-tasks)
            (ewoc-enter-last ewoc (car task))))))
    ;; FIXME: how to preserve the cursor position when update ewoc nodes.
    ;; idea: use ewoc-goto-node
    (read-only-mode 1)))

;;;###autoload
(defun gtd-task-show-details-toggle ()
  "Show all details of each task in gtd-tasks buffer."
  (interactive)
  (if gtd-show-all-details
      (setq gtd-show-all-details nil)
    (setq gtd-show-all-details t))
  (gtd-show-tasks gtd-current-checklist))

;;;###autoload
(defun gtd-task-show-finished-toggle ()
  "Show all finished tasks in gtd-tasks buffer."
  (interactive)
  (if gtd-show-all-finished
      (setq gtd-show-all-finished nil)
    (setq gtd-show-all-finished t))
  (gtd-show-tasks gtd-current-checklist))

(defun gtd-data-update ()
  "Update the value of `gtd-data' in ewoc."
  (let* ((checklist gtd-current-checklist)
         (tasks (gtd-db-query `[:select * :from task :where
                                        (= checklist ,checklist)]))
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
    (setq date (gtd-date-to-seconds date))
    (setq priority (gtd-plist-get priority gtd-priorities :id))
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

;;;###autoload
(defun gtd-withdraw-finished-task ()
  "Withdraw the finished task."
  (interactive)
  (let* ((id (ewoc-data (ewoc-locate gtd-ewoc)))
         (args (gtd-task-args id gtd-data)))
    (gtd-task--withdraw (apply #'gtd-task args))))

(defun gtd-task-finish-toggle ()
  "Finish the task if the task is unfinished.
Withdraw the finished task if the task is finished."
  (let* ((id (ewoc-data (ewoc-locate gtd-ewoc)))
         (status (gtd-task-attr id :status)))
    (pcase status
      (0 (gtd-finish-task))
      (1 (gtd-withdraw-finished-task)))))



;; (defun gtd-task-details-pp (id)
;;   (when id
;;     (insert (format "%s %s" (if (= status 0) "☐" "☑") name))))

;; (defun gtd-show-task-details (data)
;;   "Show the details of a task with DATA."
;;   (interactive)
;;   (let* ((id (plist-get data :id))
;;          (name (plist-get data :name))
;;          (status (plist-get data :status))
;;          (priority (plist-get data :priority))
;;          (date (plist-get data :date))
;;          (tags (plist-get data :tags))
;;          (checklist (plist-get data :checklist))
;;          (icon (gtd-checklist-attr checklist :icon))
;;          (_ (gtd--switch-to-buffer "Gtd Details"))
;;          (ewoc (ewoc-create 'gtd-task-details-pp
;;                             (propertize (format "%s %s\n" icon checklist)
;;                                         'face 'gtd-header-face)
;;                             (substitute-command-keys
;;                              "\n\\{gtd-mode-map}"))))
;;     (set (make-local-variable 'gtd-ewoc) ewoc)
;;     (set (make-local-variable 'gtd-data) data)
;;     (set (make-local-variable 'gtd-current-checklist) checklist)
;;     (set (make-local-variable 'gtd-current-task) id)
;;     (ewoc-enter-last ewoc id)
;;     (read-only-mode 1)))

;;;###autoload
(defun gtd-task-rename (&optional id)
  "Rename the task with id ID.  
If ID is nil, rename the task at point."
  (interactive)
  (let* ((id (or id (ewoc-data (ewoc-locate gtd-ewoc))))
         (arg-lst (gtd-task-args id gtd-data))
         (name (plist-get arg-lst :name))
         (new-name (completing-read
                    (format "Rename the task '%s': " name) nil))
         task-args)
    (unless (string= new-name name)
      (setq task-args (plist-put arg-lst :name new-name))
      (gtd-task--rename (apply #'gtd-task task-args)))))

;;;###autoload
(defun gtd-task-edit-memo (&optional id)
  "Add a memo to the task with id ID.
If ID is nil, edit the memo of task at point."
  (interactive)
  (let* ((id (or id (ewoc-data (ewoc-locate gtd-ewoc))))
         (arg-lst (gtd-task-args id gtd-data))
         (name (plist-get arg-lst :name))
         (memo (plist-get arg-lst :memo))
         (new-memo
          (completing-read
           (format "Edit the memo of task '%s': " name) nil nil nil memo))
         task-args)
    (unless (string= new-memo memo)
      (setq task-args (plist-put arg-lst :memo new-memo))
      (gtd-task--edit-memo (apply #'gtd-task task-args)))))

;;;###autoload
(defun gtd-task-add-subtask ())
;;;###autoload
(defun gtd-task-change-date ())
;;;###autoload
(defun gtd-task-change-priority ())
;;;###autoload
(defun gtd-task-change-checklist ())
;;;###autoload
(defun gtd-task-change-tags ())

(defvar gtd-task-edit-actions
  '(("Rename Task" . gtd-task-rename)
    ("Edit Memo" . gtd-task-edit-memo)
    ("Add Subtask" . gtd-task-add-subtask)
    ("Change Date" . gtd-task-change-date)
    ("Change Priority" . gtd-task-change-priority)
    ("Change Checklist" . gtd-task-change-checklist)
    ("Change Tags" . gtd-task-change-tags)))

;;;###autoload
(defun gtd-task-edit ()
  "Edit the details of a task at point."
  (interactive)
  (let* ((id (ewoc-data (ewoc-locate gtd-ewoc)))
         (name (gtd-task-attr id :name))
         (action (completing-read
                  (format "Choose the action to task '%s'" name)
                  gtd-task-edit-actions nil t))
         (func (cdr (assoc action gtd-task-edit-actions))))
    (pcase action
      ("Rename Task" (apply func `(,id)))
      ("Edit Memo" (apply func `(,id))))))

;;;###autoload
(defun gtd-task-delete ()
  "Delete the task at point."
  (interactive)
  (let* ((id (gtd-ewoc-data))
         (task-args (gtd-task-args id gtd-data)))
    (gtd-task--delete (apply #'gtd-task task-args))))

;; task functions

;; major mode

(define-derived-mode gtd-mode fundamental-mode "GTD"
  (use-local-map gtd-mode-map))

(progn
  (setq gtd-mode-map (make-sparse-keymap))
  (define-key gtd-mode-map (kbd "C") #'gtd-show-checklists)
  (define-key gtd-mode-map (kbd "D") #'gtd-task-show-details-toggle)
  (define-key gtd-mode-map (kbd "A") #'gtd-task-show-finished-toggle)
  (define-key gtd-mode-map (kbd "e") #'gtd-task-edit)
  (define-key gtd-mode-map (kbd "q") #'kill-current-buffer)
  (define-key gtd-mode-map (kbd "g") #'gtd-refresh-buffer)
  ;; (define-key gtd-mode-map (kbd "<RET>") #'gtd-checklist-enter)
  (define-key gtd-mode-map (kbd "+") #'gtd-add-task)
  (define-key gtd-mode-map (kbd "d") #'gtd-finish-task)
  (define-key gtd-mode-map (kbd "u") #'gtd-withdraw-finished-task))

(provide 'gtd)
;;; gtd.el ends here
