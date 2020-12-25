;; sandbox
(cdr '(a . c))
(car '())

;; same object
(let ((list1 '(x y)) (list2 '(x . (y . nil))))
  (equal list1 list2)) ;; -> t
(let ((list1 '(x z)) (list2 '(x . (y . nil))))
  (equal list1 list2)) ;; -> nil

;;
(equal (cons 'a 'b) '(a . b)) ;; -> t

(symbolp '(x y))
(listp '(x y))
(consp '(x y))

(setq number 1)
(symbolp 'number) ;; t
(symbolp number) ;; nil
(numberp number) ;; t
(numberp 'number) ;; t

(let ((value)) (setq value "hithere"))
value ;; error
number ;; 1

(progn
  (setq mylist (list 'a 'b 'c))
  (setcdr mylist (list 'd))
  mylist
  )

(print number)

(setq filename "temp_file.el")
(setq key "x" value "vx")
(ecrib-append-to-definitions key value filename)
(push (cons "x" "xv")
      (ecrib-read-definitions filename))

(ecrib-read-definitions "cribs_definitions.el")
(stringp (prin1-to-string '(("a" . "va") ("b". "vb"))))
(write-region (prin1-to-string '(("a" . "va") ("b". "vb"))) nil "temp_file.el")

(ecrib-write-definitions '(("a" . "va") ("b". "vb") ("c" . "vc")) "temp_file.el")

(setq defs (ecrib-read-definitions "cribs_definitions.el"))
defs
(push (cons "animal moblitity walks url" "www.animal-mobility-walks.org") defs)
(push (cons "animal moblitity walks url" "www.animal-mobility-walks.org") (ecrib-read-definitions "cribs_definitions.el"))

(consp '( 4 5))

(setq cribs (ecrib-read-definitions "cribs_definitions.el"))
(map-keys cribs)

(cdr (assoc "trikstik url" cribs))

(let ((value))
  (dolist (entry cribs value)
    (setq value (cons (cons (concat (car entry) " (" (cdr entry) ")") (cdr entry)) value))))

(setq ecrib-definitions (ecrib-read-file-definitions))

(assoc-delete-all "bbbb" ecrib-definitions 'string=)
ecrib-definitions
(ecrib-read-key-value "a")
(customize-save-variable 'ecrib-definitions (remove nil ecrib-definitions))
(ecrib-key-value "teast")
(magit-dispatch)

(setq text "multi\nline\ntext")
(cl-search "\n" text)
(substring text (+ 1 pos))

(defun count-char (text char)
  "Count char in string"
  (setq counter 1)
  (setq pos (cl-search char text))
  (while (not (eq pos 'nil))
    (setq counter (+ 1 counter))
    (setq text (substring text (+ 1 pos)))
    (setq pos (cl-search "\n" text))
    text)
  counter)

(defun count-char-rec (text char)
  (if (eq 0 (cl-search "\n" text))
      ))

(count-char "ein\nviel" "\n")

(seq-count (lambda (elt)(eq elt ?\n)) text)
(car "\n")
(ecrib-value-oneliner (cons "ein" "test\ntext"))

(ecrib-export "testecribs.el")
(ecrib-import "testecribs.el")

(pretty-print-elisp (ecrib-helper-read-file-contents "testecribs.el"))


