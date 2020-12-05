;; read cribs from file
(defun my-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun my-eval-string (string)
  "Evaluate elisp code stored in a string."
  (eval (car (read-from-string string))))

(defun ecrib-read-definitions (filename)
  "Read the crib definitions from the definition file"
  (my-eval-string (my-file-contents filename)))

(defun ecrib-write-definitions (definitions filename)
  "Write the crib definitions to the definitions file"
  (write-region (concat "'"
                        (prin1-to-string definitions))
                nil
                filename))

(defun ecrib-append-to-definitions (key value filename)
  "Appends a new entry to the cribs file"
  (ecrib-write-definitions (let ((defs (ecrib-read-definitions filename)))
                             (push (cons key value)
                                   defs))
                           filename))

(setq ecrib-filepath "cribs_definitions.el")

(defun ecrib-read-file-definitions ()
  (ecrib-read-definitions ecrib-filepath))

;; as custom variable
(defcustom ecrib-definitions ()
  "An alist, where each key is the name of an ecrib definition and value it's value"
  :type '(alist :key-type string :value-type string))

(defun ecrib-read-definition-keys ()
  (map-keys ecrib-definitions))

(defun ecrib-action-copy-value-to-kill-ring (key)
  (kill-new (cdr (assoc key ecrib-definitions))))

;;; Helm stuff
(defun ecrib-helm-candidates ()
  (let ((value))
    (dolist (entry ecrib-definitions value)
      (setq value (cons (cons (concat (car entry) " (" (cdr entry) ")") (cdr entry)) value)))))

(defun ecrib-helm ()
  (interactive)
  (helm :sources (helm-build-sync-source "helm-ecrib-source"
                   :candidates (ecrib-helm-candidates)
                   :action '(("Copy value to kill-ring" . kill-new))
                   :match-on-real t)
        :buffer "*helm ecrib*"))
