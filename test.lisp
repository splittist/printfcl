;;;; test.lisp

(defpackage #:printfcl-test
  (:use #:cl #:printfcl))

(in-package #:printfcl-test)

(defun clean-line (line)
  (let ((step1 (string-trim " " line)))
    (if (string= "" step1)
        nil
        (unless (or (char= #\# (char step1 0))
                    (char= #\! (char step1 0)))
          (let* ((step2 (string-right-trim "UVLL" step1))
                 (step3 (substitute #\" #\' step2)))
            step3)))))

(defun read-tests (file)
  (with-open-file (f file)
    (loop with results = '()
          for line = (read-line f nil nil)
          while line
          do (let ((str (clean-line line)))
               (when str
                 (with-input-from-string (s str)
                   (let* ((number (read s))
                          (result (read s))
                          (format (read s))
                          (args (loop for arg = (read s nil nil)
                                      while arg
                                      collecting arg)))
                     (push (list number result format args) results)))))
          finally (return (nreverse results)))))

(defparameter *skiplist* '(105 106 107 108 109 110 111 112 ; integer wraparound
                           170 ; integer wraparound
                           179 ; char as int
                           ))

(defun run (&optional (file #p"test.txt"))
  (let ((tests (read-tests file)))
    (dolist (test tests)
      (destructuring-bind (number result format args) test
        (if (or (string= "?" result) (member number *skiplist*))
            (format t "Text ~3D skipped~%" number)
            (let ((res (with-output-to-string (s)
                         (apply #'fprintf s format args))))
              (if (string= res result)
                  (format t "Test ~3D succeeded~%" number)
                  (format t "Test ~3D failed: '~A' gave '~A' not '~A'. ~A~%"
                          number format res result args))))))))
(defun test-a (num)
  (= num (parse-hex-float
          (with-output-to-string (s)
            (fprintf s "%a" num)))))

(defun test-alot (&optional (count 1000))
  (loop for i below count
        for num = (* (random most-positive-double-float) (if (evenp count) -1 1))
        for success = (test-a num)
        unless success
          collect num into failures
        finally (if failures
                    (format t "Test-A failures: ~A" failures)
                    (format t "Test-A passed ~D times." count))))
