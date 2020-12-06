;;; ecrib.el --- A simple key-value store

;; Copyright (C) 2010-2020 Martin Puttke

;; Author: Martin Puttke <m.s.p@posteo.de>
;; Created: 06 Dec 2020
;; Keywords: convenience
;; URL: https://github.com/matlantis/emacs-ecrib
;; Version: 0.1
;; Package-Requires: (helm)
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; The Package provides a place to store key-value pairs in global and
;; persistent location.
;;
;; Essential Commands:
;; - ecrib-add: add a new key value pair
;; - ecrib-helm: browse key-value pairs with helm. There are helm actions
;; defined for insertion, opening, editing and deletion of selected keys
;; (C-z in helm buffer so see them).

;;; Change Log:
;;
;; empty

;;; Code:

(require 'helm)
(require 'helm-source)

;;; Customize

(defcustom ecrib-definitions ()
  "An alist, where each key is the name of an ecrib definition and value it's value"
  :type '(alist :key-type string :value-type string))

;;;###autoload
(defun ecrib-add ()
  (interactive)
  (let* ((value (read-string "New ecrib value: "))
         (key (read-string "New ecrib key: ")))
    (customize-save-variable 'ecrib-definitions (push (cons key value)
                                                      ecrib-definitions))))
(defun ecrib-delete (&optional key)
  (interactive)
  (setq key (or key (read-string "delete ecrib with key: ")))
  (customize-save-variable 'ecrib-definitions
                           (assoc-delete-all key ecrib-definitions 'string=)))

(defun ecrib-edit (curkey)
  (interactive)
  (let* ((value (read-string "new ecrib value: " (cdr (assoc curkey ecrib-definitions 'string=))))
         (key (read-string "new ecrib key: " (car (assoc curkey ecrib-definitions 'string=)))))
    (let ((ecribs (assoc-delete-all curkey ecrib-definitions 'string=)))
      (customize-save-variable
       'ecrib-definitions
        (push (cons key value)
              ecribs)))))

(defun ecrib-kill-new (key)
  (kill-new (cdr (assoc key ecrib-definitions 'string=))))

(defun ecrib-insert (key)
  (insert (cdr (assoc key ecrib-definitions 'string=))))

(defun ecrib-helm-candidates ()
  (let ((value))
    (dolist (entry ecrib-definitions value)
      (setq value (cons (cons (concat (car entry) " (" (cdr entry) ")") (car entry)) value)))))

;;;###autoload
(defun ecrib-helm ()
  (interactive)
  (helm :sources (helm-build-sync-source "helm-ecrib-source"
                   :candidates (ecrib-helm-candidates)
                   :action '(("Insert value at point" . ecrib-insert)
                             ("Copy value to kill-ring" . ecrib-kill-new)
                             ("Edit" . ecrib-edit)
                             ("Delete" . ecrib-delete))
                   :match-on-real t)
        :buffer "*helm ecrib*"))

(provide 'ecrib)
;;; ecrib.el ends here
