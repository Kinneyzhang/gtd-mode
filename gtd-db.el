;;; gtd-db.el --- purpose

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Kinney Zhang <kinneyzhang666 AT gmail DOT com>
;; URL: http://github.com/Kinneyzhang/gtd-db
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

(require 'org-id)
(require 'emacsql)
(require 'emacsql-sqlite3)

(defcustom gtd-db-location
  (expand-file-name "gtd.db" (concat user-emacs-directory "gtd"))
  "Database of gtd."
  :type 'string
  :group 'gtd)

(defvar gtd-version "gtd-v1")

(defvar gtd-db--connection (make-hash-table :test #'equal)
  "Database connection to Gtd database.")

(defconst gtd-db--table-schemata
  '((task
     [(id :primary-key)
      (name :not-null)
      (status :not-null)
      date tags priority
      (checklist :not-null)
      memo parent]))
  "Table schemata of gtd-db.")

(defun gtd-db--get-connection ()
  "Return the database connection, if any."
  (gethash gtd-version gtd-db--connection))

(defun gtd-db--init (db)
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) gtd-db--table-schemata)
      (emacsql db `[:create-table ,table ,schema]))))

(defun gtd-db ()
  "Entrypoint to the gtd sqlite database.
Initializes and stores the database, and the database connection."
  (unless (and (gtd-db--get-connection)
               (emacsql-live-p (gtd-db--get-connection)))
    (let ((init-db (not (file-exists-p gtd-db-location))))
      (make-directory (file-name-directory gtd-db-location) t)
      (let ((conn (emacsql-sqlite3 gtd-db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash gtd-version conn gtd-db--connection)
        (when init-db
          (gtd-db--init conn)))))
  (gtd-db--get-connection))

(defun gtd-db-close (&optional db)
  "Closes the database connection for database DB."
  (unless db
    (setq db (gtd-db--get-connection)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun gtd-db-query (sql &rest args)
  "Run SQL query on Gtd database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if  (stringp sql)
      (emacsql (gtd-db) (apply #'format sql args))
    (apply #'emacsql (gtd-db) sql args)))

;; gtd functions.

(defun gtd-db-retrive-tasks-smartly (conditions)
  "Retrive all tasks which satisfy the CONDITIONS."
  (gtd-db-query `[:select * :from task
                          :where ,conditions]))

(provide 'gtd-db)
;;; gtd-db.el ends here
