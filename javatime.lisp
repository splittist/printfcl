(defpackage #:printfcl/javatime
  (:use #:cl #:printfcl #:local-time))

(in-package #:printfcl/javatime)

(defclass javatime-converter (standard-converter)
  ())

(defparameter *javatime-conversion-character-suffixes*
  "HIklMSLNpzZsQBbhAaCYyjMdeRTrDFc")

(defmethod collect-conversion-specifier ((converter javatime-converter)
                                         format-string
                                         format-string-index)
  (if (not (member (char format-string format-string-index) '(#\t #\T) :test #'char=))
      (call-next-method)
      (let ((tee (char format-string format-string-index)))
        (incf format-string-index)
        (let ((suffix (find (char format-string format-string-index)
                            *javatime-conversion-character-suffixes*)))
          (when (null suffix)
            (error "Unknown javatime suffix: '~C'" (char format-string format-string-index)))
          (values (intern (coerce (list tee suffix) 'string) :keyword)
                  (incf format-string-index))))))

(defmethod convert ((converter javatime-converter) conversion-specifier
                    argument flags field-width precision)
  (let* ((symbol-string (string conversion-specifier))
         (first-char (char symbol-string 0)))
    (if (member first-char (list #\T #\t))
        (let ((timestring (convert-time
                           conversion-specifier
                           (unix-to-timestamp argument))))
          (with-flags (flags)
            (let* ((field-width (or field-width 0))
                   (ts-length (length timestring))
                   (pad-length (- field-width ts-length))
                   (padp (plusp pad-length))
                   (field (cond ((and padp minus-flag)
                                 (strcat timestring (spaces pad-length)))
                                (padp
                                 (strcat (spaces pad-length) timestring))
                                (t
                                 timestring))))
              field)))
        (call-next-method))))

(defun day-of-year (timestamp)
  (let ((year (timestamp-year timestamp))
        (month (timestamp-month timestamp))
        (day (timestamp-day timestamp)))
    (loop for m from 1 below month
          summing (days-in-month m year) into count
          finally (return (+ count day)))))

(defun short-year-string (timestamp)
  (format nil "~2,'0D" (nth-value 1 (truncate (timestamp-year timestamp) 100))))

(defun convert-time (conversion-specifier-symbol timestamp)
  (let* ((symbol-string (string conversion-specifier-symbol))
         (upperp (char= #\T (char symbol-string 0)))
         (suffix (char symbol-string 1))
         (format
           (case suffix
             (#\H (list (list :hour 2)))
             (#\I (list (list :hour12 2)))
             (#\k (list :hour))
             (#\l (list :hour12))
             (#\M (list (list :min 2)))
             (#\S (list (list :sec 2)))
             (#\L (list (list :msec 3)))
             (#\N (list (list :nsec 9)))
             (#\p (list :ampm))
             (#\z (list :gmt-offset-or-z))
             (#\Z (list :timezone))
             (#\s (list (princ-to-string  (timestamp-to-unix timestamp))))
             (#\Q (list (princ-to-string (+ (* 1000 (timestamp-to-unix timestamp))
                                            (timestamp-millisecond timestamp)))))
             (#\B (list :long-month))
             ((#\b #\h) (list :short-month))
             (#\A (list :long-weekday))
             (#\a (list :short-weekday))
             (#\C (list (format nil "~2,'0D" (truncate (timestamp-year timestamp) 100))))
             (#\Y (list (list :year 4)))
             (#\y (list (short-year-string timestamp)))
             (#\j (list (format nil "~3,'0D" (day-of-year timestamp))))
             (#\m (list (list :month 2)))
             (#\d (list (list :day 2)))
             (#\e (list :day))
             (#\R (list (list :hour 2) #\: (list :min 2)))
             (#\T (list (list :hour 2) #\: (list :min 2) #\: (list :sec 2)))
             (#\r (list (list :hour12 2) #\: (list :min 2) #\: (list :sec 2) #\Space :ampm))
             (#\D (list (list :month 2) #\/ (list :day 2) #\/ (short-year-string timestamp)))
             (#\F (list (list :year 4) #\- (list :month 2) #\- (list :day 2)))
             (#\c (list :short-weekday #\Space
                        :short-month #\Space
                        (list :day 2) #\Space
                        (list :hour 2) #\: (list :min 2) #\: (list :sec 2) #\Space
                        :timezone #\Space
                        (list :year 4)))))
         (conversion (format-timestring nil timestamp :format format)))
    (if upperp (string-upcase conversion) conversion)))

#+(or)(defun test1 ()
  (dolist (tee (list #\t #\T))
    (loop for suffix across *javatime-conversion-character-suffixes*
          for cs = (coerce (list tee suffix) 'string)
          do (format t "~A: ~A~%" cs (convert-time cs (now))))))

#+(or)(let ((*converter* (make-instance 'javatime-converter))
            (*length-modifiers* (remove #\t *length-modifiers*)))
        (printf "%s %tc" "The time is:" (timestamp-to-unix (now))))
