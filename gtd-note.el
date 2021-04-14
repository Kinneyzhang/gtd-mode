;;; gtd-note.el --- purpose

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Kinney Zhang <kinneyzhang666 AT gmail DOT com>
;; URL: http://github.com/usrname/gtd-note
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

(defvar gtd-note-buf "*Gtd Note*")

(defvar gtd-note-capture-buf "*Gtd Note Capture*"
  "Buffer name of gtd note capture.")

(defvar gtd-note-preview-buf "*Gtd Note Preview*"
  "Buffer name of gtd note preview.")

(defvar gtd-return-wconf nil
  "The returned window configuration.")

(defvar gtd-note-id nil
  "Id of gtd note.")

(defvar gtd-note-title nil
  "Title of gtd note.")

(defvar gtd-note-seconds nil
  "Seconds of gtd note created timestamp.")

(defvar gtd-note-flag nil
  "Flag to tell whether it's a new inserted note or updated note.")

(defvar gtd-note-multilingual
  '(("Input the title of the note" :zh-cn "请输入笔记的标题"))
  "Multilingual words of note.")

(defvar gtd-note-title-symbol "■")

(defvar gtd-note-full-p nil
  "Judge whether to show full content of note.")

;;;; Functions

;;;###autoload
(defun gtd-note-capture-finalize ()
  "Finish the process of capturing gtd notes."
  (interactive)
  (let ((flag gtd-note-flag)
        (id gtd-note-id)
        (title gtd-note-title)
        (seconds gtd-note-seconds)
        (content (buffer-substring-no-properties (point-min) (point-max))))
    (set-window-configuration gtd-return-wconf)
    (ewoc-enter-first gtd-ewoc `(,id ,title ,content ,seconds))
    (pcase flag
      ('insert
       (gtd-db-query `[:insert :into note
                               :values ([,id ,title ,content ,seconds])]))
      ('update
       (let ((inhibit-read-only 1))
         (ewoc-delete gtd-ewoc (ewoc-locate gtd-ewoc)))
       (gtd-db-query `[:update note :set [(= title ,title)
                                          (= content ,content)
                                          (= timestamp ,seconds)]
                               :where (= id ,id)])))
    (kill-buffer gtd-note-capture-buf)))

;;;###autoload
(defun gtd-note-capture-cancel ()
  "Cancel the process of capturing gtd notes."
  (interactive)
  (set-window-configuration gtd-return-wconf)
  (kill-buffer gtd-note-capture-buf))

;;;###autoload
(define-minor-mode gtd-capture-mode
  "Minor mode for capturing gtd notes.
Turning on this mode runs the normal hook `gtd-capture-mode-hook'."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'gtd-note-capture-finalize)
            (define-key map (kbd "C-c C-k") #'gtd-note-capture-cancel)
            map)
  (if gtd-capture-mode
      (setq header-line-format
            (substitute-command-keys "\\<gtd-capture-mode-map>Capture Notes: `\\[gtd-note-capture-finalize]' to finish, `\\[gtd-note-capture-cancel]' to cancel."))
    (setq header-line-format nil)))

;;;###autoload
(defun gtd-note-capture ()
  "Capture gtd notes in `gtd-note-capture-buf' buffer."
  (interactive)
  (let ((capture-buf (get-buffer-create gtd-note-capture-buf)))
    (setq gtd-return-wconf (current-window-configuration))
    (delete-other-windows)
    (pop-to-buffer capture-buf)
    (gtd-capture-mode 1)))

;;;###autoload
(defun gtd-note-new ()
  "Create a new note."
  (interactive)
  (let* ((id (org-id-uuid))
         (title (gtd-completing-read "Input the title of the note" nil))
         (seconds (gtd-time-to-seconds (gtd-format-time))))
    (gtd-note-capture)
    (setq-local gtd-note-flag 'insert)
    (setq-local gtd-note-id id)
    (setq-local gtd-note-title title)
    (setq-local gtd-note-seconds seconds)))

(defvar gtd-note-abstract-count 25)

(defun gtd-note-pp (data)
  (pcase data
    ((guard (org-uuidgen-p (car data)))
     (let* ((title (nth 1 data))
            (content (nth 2 data))
            (abstract (if (> (length content) gtd-note-abstract-count)
                          (concat (substring content 0 25) "...")
                        content))
            (date (gtd-seconds-to-date (nth 3 data)))
            (timestamp (replace-regexp-in-string "-" "/" date)))
       (insert (propertize (format "%s" title)
                           'face 'bold
                           'line-prefix
                           (propertize (concat gtd-note-title-symbol " ")
                                       'face 'bold))
               " " (propertize (concat "[" timestamp "]") 'face 'shadow) "\n"
               (propertize
                (if gtd-note-full-p
                    content
                  abstract)
                'line-prefix "  ") "\n")))))

;;;###autoload
(defun gtd-note-show ()
  "Show all gtd notes."
  (interactive)
  (gtd--switch-to-buffer gtd-note-buf)
  (let ((ewoc (ewoc-create 'gtd-note-pp
                           (propertize "📗 Gtd Note\n"
                                       'face 'gtd-header-face)
                           (substitute-command-keys
                            "\n\\{gtd-note-mode-map}")))
        (notes (gtd-db-query `[:select * :from note
                                       :order-by (desc timestamp)])))
    (dolist (note notes)
      (ewoc-enter-last ewoc note))
    (set (make-local-variable 'gtd-ewoc) ewoc)
    (gtd-note-mode 1)
    (read-only-mode 1)))

;;;###autoload
(defun gtd-note-delete ()
  "Delete the note at point."
  (interactive)
  (let* ((ewoc gtd-ewoc)
         (node (ewoc-locate ewoc))
         (id (car (ewoc-data node)))
         (inhibit-read-only 1))
    (ewoc-delete ewoc node)
    (gtd-db-query `[:delete :from note :where (= id ,id)])))

;;;###autoload
(defun gtd-note-edit ()
  "Reedit the title or content of note at point."
  (interactive)
  (let* ((ewoc gtd-ewoc)
         (data (ewoc-data (ewoc-locate ewoc)))
         (seconds (gtd-time-to-seconds (gtd-format-time)))
         (id (nth 0 data))
         (title-old (nth 1 data))
         (content-old (nth 2 data))
         (title (gtd-completing-read "Edit the note title" nil nil nil title-old)))
    (gtd-note-capture)
    (insert content-old)
    (setq-local gtd-note-flag 'update)
    (setq-local gtd-note-id id)
    (setq-local gtd-note-title title)
    (setq-local gtd-note-seconds seconds)))

;;;###autoload
(defun gtd-note-full-toggle ()
  "Toggle function for showing the full content of notes or not."
  (interactive)
  (if gtd-note-full-p
      (setq gtd-note-full-p nil)
    (setq gtd-note-full-p t))
  (ewoc-refresh gtd-ewoc)
  (goto-char (point-min)))

;;;###autoload
(defun gtd-note-preview ()
  "Preview the full content of the note at point in a pop buffer."
  (interactive)
  (let* ((data (ewoc-data (ewoc-locate gtd-ewoc)))
         (title (nth 1 data))
         (content (nth 2 data))
         (buf (get-buffer-create gtd-note-preview-buf)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (propertize title 'face '(bold :height 1.2))
              "\n\n" content)
      (view-buffer buf))))

;;;###autoload
(defun gtd-note-refresh ()
  "Refresh the `gtd-note-buf' buffer."
  (interactive)
  (gtd-note-show))

(define-minor-mode gtd-note-mode
  "Minor mode for gtd note."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "N") #'gtd-note-new)
            (define-key map (kbd "D") #'gtd-note-delete)
            (define-key map (kbd "E") #'gtd-note-edit)
            (define-key map (kbd "T") #'gtd-note-full-toggle)
            (define-key map (kbd "<RET>") #'gtd-note-preview)
            (define-key map (kbd "g") #'gtd-note-refresh)
            map))

(provide 'gtd-note)
;;; gtd-note.el ends here
