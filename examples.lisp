(in-package #:printfcl)

;;;; IEEE floats

(defun decodeb64 (n)
  (let ((sign (ldb (byte 1 63) n))
        (exponent (ldb (byte 11 52) n))
        (significand (ldb (byte 52 0) n)))
    (when (= exponent 1023)
      (return-from decodeb64
        (cond ((not (zerop significand))
               :nan)
              ((zerop sign)
               :+inf)
              (t
               :-inf))))
    (if (zerop exponent)
        (setf exponent 1)
        (setf (ldb (byte 1 52) significand) 1))
    (scale-float (* significand (if (zerop sign) 1d0 -1d0))
                 (- exponent 1075))))

(defun decodeb32 (n)
  (let ((sign (ldb (byte 1 31) n))
        (exponent (ldb (byte 8 23) n))
        (significand (ldb (byte 23 0) n)))
    (when (= exponent 255)
      (return-from decodeb32
        (cond ((not (zerop significand))
               :nan)
              ((zerop sign)
               :+inf)
              (t
               :-inf))))
    (if (zerop exponent)
        (setf exponent 1)
        (setf (ldb (byte 1 23) significand) 1))
    (scale-float (* significand (if (zerop sign) 1e0 -1e0))
                 (- exponent 150))))

(defun decodee80 (n)
  (let ((sign (ldb (byte 1 79) n))
        (exponent (ldb (byte 15 64) n))
        (significand (ldb (byte 64 0) n)))
    (when (= exponent 32767)
      (return-from decodee80
        (cond ((= significand #.(ash 1 63))
               (if (zerop sign) :+inf :-inf))
              (t :nan))))
    (scale-float (* significand (if (zerop sign) 1d0 -1d0))
                 (- exponent 16446))))

(defclass ieee-float-converter (standard-converter)
  ())

(defmethod retrieve-argument ((converter ieee-float-converter) (cs (eql :|f|))
                              length-modifier arguments index)
  (let* ((n (elt arguments index))
         (extendedp (equal length-modifier '(#\L))))
    (if extendedp (decodee80 n) (decodeb64 n))))

(defmethod retrieve-argument ((converter ieee-float-converter) (cs (eql :|e|))
                              length-modifier arguments index)
  (let* ((n (elt arguments index))
         (extendedp (equal length-modifier '(#\L))))
    (if extendedp (decodee80 n) (decodeb64 n))))

(defmethod float-nan-p ((converter ieee-float-converter) obj)
  (eq obj :nan))

(defmethod float-infinity-p ((converter ieee-float-converter) obj)
  (or (eq obj :+inf) (eq obj :-inf)))

(defmethod negative-infinity-p ((converter ieee-float-converter) obj)
  (eq obj :-inf))

#+(or)(let ((*converter* (make-instance 'ieee-float-converter)))
  (printf "%f %Lf %e %Le %Lf"
          #b0100100011111110001000100010111010000010011001101101001001111111
          #x400c8ca2000000000000
          #b0100100011111110001000100010111010000010011001101101001001111111
          #x400c8ca2000000000000
          #x7fff8000000000000000
         ))

(defclass golike-t-converter (standard-converter)
  ())

(defmethod convert ((converter golike-t-converter) (cs (eql :|T|))
                    argument flags field-width precision)
  (convert-string (prin1-to-string (type-of argument)) flags field-width nil))

(defmethod convert ((converter golike-t-converter) (cs (eql :|t|))
                    argument flags field-width precision)
  (let ((arg (if argument "true" "false")))
    (convert-string arg flags field-width nil)))

#+(or)(let ((*converter* (make-instance 'golike-T-converter))
            (*length-modifiers* (remove #\t *length-modifiers*)))
        (printf "%T %T %T %t %t" 1 "foo" *converter* t nil))

(defclass javaesque-h-converter ()
  ())

(defun hash-helper (argument upperp)
  (convert-integer (sxhash argument)
                   (set-flag 0 #\#)
                   0
                   nil
                   :radix 16
                   :upperp upperp))

(defmethod convert ((converter javaesque-h-converter) (cs (eql :|h|))
                    argument flags field-width precision)
  (convert-string (if (null argument)
                      "null"
                      (hash-helper argument nil))
                  flags field-width precision))

(defmethod convert ((converter javaesque-h-converter) (cs (eql :|H|))
                    argument flags field-width precision)
  (convert-string (if (null argument)
                      "NULL"
                      (hash-helper argument t))
                  flags field-width precision))

(defclass my-converter (javaesque-h-converter ieee-float-converter)
  ())

#+(or)(let ((*converter* (make-instance 'my-converter))
                (*length-modifiers* "L"))
            (printf "%.e %h" #x48FE222E8266D27F *converter*))

(defclass pythonish-strings-converter (standard-converter)
  ())

(defmethod convert ((converter pythonish-strings-converter) (cs (eql :|s|))
                    argument flags field-width precision)
  (convert-string (princ-to-string argument)
                  flags field-width precision))

(defmethod convert ((converter pythonish-strings-converter) (cs (eql :|r|))
                    argument flags field-width precision)
  (convert-string (prin1-to-string argument)
                  flags field-width precision))

#+(or)(let ((*converter* (make-instance 'pythonish-strings-converter)))
  (printf "%s %r" "Hello" "World"))

