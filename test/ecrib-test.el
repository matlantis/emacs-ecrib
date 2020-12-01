;; package tests
(let ((definitions '(("a" . "va")
                     ("c" . "vc")))
      (filename "temp_file.el"))
  (progn
    (ecrib-write-definitions definitions filename)
    (equal definitions (ecrib-read-definitions filename))))

(let ((definitions '(("a" . "va") ("c". "vc")))
      (filename "temp_file.el"))
  (progn
    (ecrib-write-definitions definitions filename)
    (equal definitions
           (ecrib-read-definitions filename))))

(let ((definitions '(("a" . "va") ("c". "vc")))
      (key "x")
      (value "vx")
      (filename "temp_file.el"))
  (progn
    (ecrib-write-definitions definitions filename)
    (ecrib-append-to-definitions key value filename)
    (equal (push (cons key value) definitions)
           (ecrib-read-definitions filename))))
