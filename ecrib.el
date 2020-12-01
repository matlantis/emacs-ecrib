;; package content
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

