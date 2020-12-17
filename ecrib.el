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

(defun ecrib-read-key-value (&optional key-val)
  "Ask the user to enter a new key-value pair, with key-val as default input. Returns a cons."
  (setq defkey (or (car key-val) ""))
  (setq defval (or (cdr key-val) ""))
  (setq key (read-string "New ecrib key: " defkey))
  (setq value (read-string "New ecrib value: " defval))
  (cons key value))

;;;###autoload
(defun ecrib-add ()
  (interactive)
  (customize-save-variable 'ecrib-definitions
                           (push (ecrib-read-key-value) ecrib-definitions)))

(defun ecrib-delete (&optional key)
  (interactive)
  (setq key (or key (read-string "delete ecrib with key: ")))
  (customize-save-variable 'ecrib-definitions
                           (assoc-delete-all key ecrib-definitions 'string=)))

(defun ecrib-key-value (key)
  "Return (k . v) from ecrib-definitions where key equals k."
  (assoc key ecrib-definitions 'string=))

(defun ecrib-value (key)
  "Return the value for a key in ecrib-definitions"
  (cdr (ecrib-key-value key)))

(defun ecrib-edit (curkey)
  (interactive)
  (setq new-key-value (ecrib-read-key-value (ecrib-key-value curkey)))
  (setq ecribs (assoc-delete-all curkey ecrib-definitions 'string=))
  (customize-save-variable
   'ecrib-definitions (push new-key-value ecribs)))

(defun ecrib-kill-new (key)
  (kill-new (ecrib-value key)))

(defun ecrib-insert (key)
  (insert (ecrib-value key)))

(defun ecrib-find-file (key)
  (find-file (ecrib-value key)))

(defun ecrib-xdg-open (key)
  (browse-url-xdg-open (ecrib-value key)))

(defun ecrib-string-oneliner (value)
  "Creates a shortened preview of a multiline string"
  (let ((nlines (seq-count (lambda (elt)(eq elt ?\n)) value)))
    (if (eq 0 nlines) value
      (format "%s... (%s more lines)"
              (substring value 0 (+ 0 (cl-search "\n" value)))
              nlines))))

(defun ecrib-helm-candidates ()
  (let ((value))
    (dolist (entry ecrib-definitions value)
      (setq value (cons (cons
                         (format "%s (%s)"
                                 (car entry)
                                 (ecrib-string-oneliner (cdr entry)))
                         (car entry))
                        value)))))

;;;###autoload
(defun ecrib-helm ()
  (interactive)
  (helm :sources (helm-build-sync-source "helm-ecrib-source"
                   :candidates (ecrib-helm-candidates)
                   :action '(("Insert at point" . ecrib-insert)
                             ("Copy to kill-ring" . ecrib-kill-new)
                             ("Find file" . ecrib-find-file)
                             ("Open externally" . ecrib-xdg-open)
                             ("Edit" . ecrib-edit)
                             ("Delete" . ecrib-delete))
                   :match-on-real t)
        :buffer "*helm ecrib*"))

(provide 'ecrib)
;;; ecrib.el ends here
