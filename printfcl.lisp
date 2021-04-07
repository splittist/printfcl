;;;; printfcl.lisp

;;; TODO
;;; [+] sprintf - return strings, not print to stream
;;; [+] %a and %A
;;; [ ] *d+$ for argument, field-width and precision
;;; [ ] errors
;;; [x] ~/format-control/
;;; [+] pass length modifiers
;;; [ ] example of c-like bit conversion
;;; [-] example of NaN/Infinity handling
;;; [ ] python version
;;; [ ] java version

(in-package #:printfcl)

;;; FUNCTIONS

(defun printf (format-string &rest arguments)
  (doprintf *standard-output* format-string arguments))

(defun fprintf (stream format-string &rest arguments)
  (cond ((eq t stream)
         (apply #'printf format-string arguments))
        ((null stream)
         (apply #'sprintf format-string arguments))
        (t
         (doprintf stream format-string arguments))))

(defun sprintf (format-string &rest arguments)
  (with-output-to-string (s)
    (doprintf s format-string arguments)))

;;; PROTOCOL

(defgeneric collect-length-modifier (converter format-string format-string-index)
  (:documentation "Returns two values, the length modifier (in a form to be used by RETRIEVE-ARGUMENT)
and the updated FORMAT-STRING-INDEX"))

(defgeneric collect-conversion-specifier (converter format-string format-string-index)
  (:documentation "Returns two values, the conversion specifier (a symbol)
and the updated FORMAT-STRING-INDEX"))

(defgeneric retrieve-argument (converter conversion-specifier length-modifer arguments index)
  (:documentation "Return the argument from ARGUMENTS (a list) pointed to by INDEX (an integer),
performing any processing as indicated by CONVERTER, CONVERSION-SPECIFIER and LENGTH-MODIFIER."))

(defgeneric convert (converter conversion-specifier argument flags field-width precision)
  (:documentation "Return a string representing ARGUMENT converted according to
CONVERSION-SPECIFIER (a symbol),
FLAGS (a bitfield - see WITH-FLAGS),
FIELD-WIDTH (nil or an integer), and
PRECISION (nil or an integer).")
  (:method (converter conversion-specifier argument flags field-width precision)
    (error 'unknown-conversion-specifier :specifier conversion-specifier)))

(defgeneric float-nan-p (converter obj)
  (:documentation "Is OBJ a floating point Not-a-Number?"))

(defgeneric float-infinity-p (converter obj)
  (:documentation "Is OBJ a floating point Infinity?"))

(defgeneric negative-infinity-p (converter obj)
  (:documentation "Is OBJ a negative floating point Infinity?"))

;;; FLAGS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *flag-characters* "-+ #0"))

(defun set-flag (flags char)
  (logior flags (expt 2 (position char *flag-characters*))))

(defmacro with-flags ((flags) &body body)
  `(symbol-macrolet
       ((minus-flag (logbitp ,(position #\- *flag-characters*) ,flags))
        (plus-flag (logbitp ,(position #\+ *flag-characters*) ,flags))
        (space-flag (logbitp ,(position #\Space *flag-characters*) ,flags))
        (hash-flag (logbitp ,(position #\# *flag-characters*) ,flags))
        (zero-flag (logbitp ,(position #\0 *flag-characters*) ,flags)))
     ,@body))

;;; CONDITIONS

(define-condition printfcl-error (simple-error)
  ()
  (:documentation "The base for all PRINTFCL errors."))

(define-condition unknown-conversion-specifier (printfcl-error)
  ((%specifier :initarg :specifier :reader unknown-conversion-specifier-specifier))
  (:report (lambda (condition stream)
             (format stream "Unknown conversion specifier: ~A"
                     (unknown-conversion-specifier-specifier condition)))))

;;; STANDARD-CONVERTER

(defclass standard-converter ()
  ())

(defparameter *converter* (make-instance 'standard-converter))

(defmethod float-nan-p ((converter standard-converter) num)
  (and (floatp num)
       #+sbcl(sb-ext:float-nan-p num)
       #-sbcl(handler-case (not (= num num))
               (floating-point-invalid-operation () t))))

(defmethod float-infinity-p ((converter standard-converter) num)
  (and (floatp num)
       #+sbcl(sb-ext:float-infinity-p num)
       #-sbcl(not (< most-negative-long-float
                     num
                     most-positive-long-float))))

(defmethod negative-infinity-p ((converter standard-converter) float)
  (and (float-infinity-p converter float)
       (minusp float)))

(defparameter *length-modifiers* "hlqLjzZt")

(defmethod collect-length-modifier ((converter standard-converter) format-string format-string-index)
  (loop while (find (char format-string format-string-index) *length-modifiers*)
        collect (char format-string format-string-index) into result
        do (incf format-string-index)
        finally (return (values result format-string-index))))

(defmethod collect-conversion-specifier ((converter standard-converter)
                                         format-string format-string-index)
  (let* ((conversion-specifier (char format-string format-string-index))
         (conversion-specifier-symbol (intern (string conversion-specifier) :keyword)))
    (values conversion-specifier-symbol (incf format-string-index))))


(defmethod retrieve-argument ((converter standard-converter) cs lm arguments index)
    (elt arguments index))

(defmethod convert ((converter standard-converter) (cs (eql :|d|))
                    argument flags field-width precision)
  (convert-integer argument flags field-width precision
                   :radix 10 :upperp nil :signedp t))

(defmethod convert ((converter standard-converter) (cs (eql :|i|))
                    argument flags field-width precision)
  (convert-integer argument flags field-width precision
                   :radix 10 :upperp nil :signedp t))

(defmethod convert ((converter standard-converter) (cs (eql :|u|))
                    argument flags field-width precision)
  (convert-integer argument flags field-width precision
                   :radix 10 :upperp nil :signedp nil))

(defmethod convert ((converter standard-converter) (cs (eql :|o|))
                    argument flags field-width precision)
  (convert-integer argument flags field-width precision
                   :radix 8 :upperp nil :signedp nil))

(defmethod convert ((converter standard-converter) (cs (eql :|x|))
                    argument flags field-width precision)
  (convert-integer argument flags field-width precision
                   :radix 16 :upperp nil :signedp nil))

(defmethod convert ((converter standard-converter) (cs (eql :|X|))
                    argument flags field-width precision)
  (convert-integer argument flags field-width precision
                   :radix 16 :upperp t :signedp nil))

(defmethod convert ((converter standard-converter) (cs (eql :|c|))
                    argument flags field-width precision)
  (convert-char argument flags field-width))

(defmethod convert ((converter standard-converter) (cs (eql :|s|))
                    argument flags field-width precision)
  (convert-string argument flags field-width precision))

(defmethod convert ((converter standard-converter) (cs (eql :|f|))
                    argument flags field-width precision)
  (convert-fixed-float argument flags field-width precision :upperp nil))

(defmethod convert ((converter standard-converter) (cs (eql :|F|))
                    argument flags field-width precision)
  (convert-fixed-float argument flags field-width precision :upperp t))

(defmethod convert ((converter standard-converter) (cs (eql :|e|))
                    argument flags field-width precision)
  (convert-exponential-float argument flags field-width precision
                             :upperp nil))

(defmethod convert ((converter standard-converter) (cs (eql :|E|))
                    argument flags field-width precision)
  (convert-exponential-float argument flags field-width precision
                             :upperp t))

(defmethod convert ((converter standard-converter) (cs (eql :|g|))
                    argument flags field-width precision)
  (convert-general-float argument flags field-width precision
                         :upperp nil))

(defmethod convert ((converter standard-converter) (cs (eql :|G|))
                    argument flags field-width precision)
  (convert-general-float argument flags field-width precision
                         :upperp t))

(defmethod convert ((converter standard-converter) (cs (eql :|a|))
                    argument flags field-width precision)
  (convert-hex-float argument flags field-width precision
                     :upperp nil))

(defmethod convert ((converter standard-converter) (cs (eql :|A|))
                    argument flags field-width precision)
  (convert-hex-float argument flags field-width precision
                     :upperp t))

;;; PADDING

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun spaces (length)
  (make-string length :initial-element #\Space))

(defun zeros (length)
  (make-string length :initial-element #\0))

;;; CONVERSION FUNCTIONS

(defun convert-inifinity-or-nan (argument flags field-width precision &key upperp)
  (declare (ignore precision))
  (with-flags (flags)
    (let* ((num (cond ((float-nan-p *converter* argument)
                       (if upperp "NAN" "nan"))
                      ((float-infinity-p *converter* argument)
                       (if upperp "INF" "inf"))
                      (t (error "Cannot determine if '~A' is Infinity or NaN." argument))))
           (sign (cond ((negative-infinity-p *converter* argument)
                        "-")
                       (plus-flag
                        "+")
                       (space-flag
                        " ")
                       (t
                        "")))
           (field-width (or field-width 0))
           (num-length (+ (length num) (length sign)))
           (padp (< num-length field-width))
           (pad-length (- field-width num-length))
           (field (cond ((and minus-flag padp)
                         (strcat sign num (spaces pad-length)))
                        (padp
                         (strcat (spaces pad-length) sign num))
                        (t
                         (strcat sign num)))))
      field)))

(defun convert-string (argument flags field-width precision)
  (with-flags (flags)
    (let* ((end (if (null precision) nil (min precision (length argument))))
           (string (subseq argument 0 end))
           (string-length (length string))
           (field-width (or field-width 0))
           (padp (< string-length field-width))
           (pad-length (- field-width string-length))
           (field (cond ((and minus-flag padp)
                         (strcat string (spaces pad-length)))
                        (padp
                         (strcat (spaces pad-length) string))
                        (t
                         string))))
    field)))

(defun convert-char (argument flags field-width)
  (with-flags (flags)
    (let* ((char (cond ((characterp argument)
                        (string argument))
                       ((and (stringp argument)
                             (= 1 (length argument)))
                        argument)
                       ((integerp argument)
                        (string (code-char argument)))
                       (t
                        (error "Wrong argument for %c: '~A'" argument))))
           (field-width (or field-width 0))
           (padp (< 1 field-width))
           (pad-length (1- field-width))
           (field (cond ((and minus-flag padp)
                         (strcat char (spaces pad-length)))
                        (padp
                         (strcat (spaces pad-length) char))
                        (t
                         char))))
      field)))

(defun gtrim (string)
  "Remove trailing #\0s and, afterwards, any trailing #\."
  (string-right-trim "." (string-right-trim "0" string)))

(defun convert-general-float (argument flags field-width precision &key upperp)
  (when (or (float-nan-p *converter* argument)
            (float-infinity-p *converter* argument))
    (return-from convert-general-float
      (convert-inifinity-or-nan argument flags field-width precision :upperp upperp)))
  (with-flags (flags)
    (let* ((p (cond ((null precision) 6)
                    ((zerop precision) 1)
                    (t precision)))
           (x (if (zerop argument) 0 (floor (log (abs argument) 10))))
           (num
             (if (and (> p x) (>= x -4))
                 (let ((pre-num (format nil "~,VF" (- p (1+ x)) (abs argument))))
                   (if hash-flag
                       pre-num
                       (gtrim pre-num)))
                 (let* ((pre-num (format nil "~,V,2,,,,VE"
                                         (1- p)
                                         (if upperp #\E #\e)
                                         (abs argument)))
                        (epos (position #\e pre-num :test #'char-equal))
                        (significand (subseq pre-num 0 epos))
                        (exponent (subseq pre-num epos))
                        (trimmed-significand (if hash-flag
                                                 significand
                                                 (gtrim significand))))
                   (strcat trimmed-significand exponent))))
           (field-width (or field-width 0))
           (sign (cond ((and plus-flag (not (minusp argument)))
                        "+")
                       ((and space-flag (not (minusp argument)))
                        " ")
                       ((minusp argument)
                        "-")
                       (t
                        "")))
           (num-length (+ (length num) (length sign)))
           (padp (< num-length field-width))
           (pad-length (- field-width num-length))
           (field (cond ((and minus-flag padp)
                         (strcat sign num (spaces pad-length)))
                        ((and zero-flag padp)
                         (strcat sign (zeros pad-length) num))
                        (padp
                         (strcat (spaces pad-length) sign num))
                        (t
                         (strcat sign num)))))
      field)))


(defun convert-fixed-float (argument flags field-width precision &key upperp)
  (when (or (float-nan-p *converter* argument)
            (float-infinity-p *converter* argument))
    (return-from convert-fixed-float
      (convert-inifinity-or-nan argument flags field-width precision :upperp upperp)))
  (with-flags (flags)
    (let* ((drop-dot (and precision (zerop precision)))
           (precision (or precision 6))
           (field-width (or field-width 0))
           (pre-num (format nil "~,VF" precision (abs argument)))
           (num (if (and drop-dot (not hash-flag))
                    (subseq pre-num 0 (1- (length pre-num)))
                    pre-num))
           (sign (cond ((and plus-flag (not (minusp argument)))
                        "+")
                       ((and space-flag (not (minusp argument)))
                        " ")
                       ((minusp argument)
                        "-")
                       (t
                        "")))
           (num-length (+ (length num) (length sign)))
           (padp (< num-length field-width))
           (pad-length (- field-width num-length))
           (field (cond ((and minus-flag padp)
                         (strcat sign num (spaces pad-length)))
                        ((and zero-flag padp)
                         (strcat sign (zeros pad-length) num))
                        (padp
                         (strcat (spaces pad-length) sign num))
                        (t
                         (strcat sign num)))))
      field)))

(defun convert-exponential-float (argument flags field-width precision
                                  &key upperp)
  (when (or (float-nan-p *converter* argument)
            (float-infinity-p *converter* argument))
    (return-from convert-exponential-float
      (convert-inifinity-or-nan argument flags field-width precision :upperp upperp)))
  (with-flags (flags)
    (let* ((drop-dot (and precision (zerop precision)))
           (precision (or precision 6))
           (field-width (or field-width 0))
           (e-char (if upperp #\E #\e))
           (pre-num (format nil "~,V,2,,,,VE" precision e-char (abs argument)))
           (num (if (and drop-dot (not hash-flag))
                    (remove #\. pre-num)
                    pre-num))
           (sign (cond ((and plus-flag (not (minusp argument)))
                        "+")
                       ((and space-flag (not (minusp argument)))
                        " ")
                       ((minusp argument)
                        "-")
                       (t
                        "")))
           (num-length (+ (length num) (length sign)))
           (padp (< num-length field-width))
           (pad-length (- field-width num-length))
           (field (cond ((and minus-flag padp)
                         (strcat sign num (spaces pad-length)))
                        ((and zero-flag padp)
                         (strcat sign (zeros pad-length) num))
                        (padp
                         (strcat (spaces pad-length) sign num))
                        (t
                         (strcat sign num)))))
      field)))

(defun convert-hex-float (argument flags field-width precision
                          &key upperp)
  (with-flags (flags)
    (let* ((argument (float argument 1d0))
           (p (if upperp #\P #\p))
           (x (if upperp #\X #\x))
           (field-width (or field-width 0))
           (precision-count (or precision -1))
           (exponent 0)
           (sign 1)
           (pre-pre-num
             (cond
               ((and (zerop argument) (or hash-flag (and precision (plusp precision))))
                "0.")
               ((zerop argument)
                "0")
               (t
                (multiple-value-bind (s exp sgn)
                    (decode-float argument)
                  (setf exponent exp
                        sign sgn)
                  (with-output-to-string (str)
                    (loop with first = t
                          until (or (zerop s) (zerop precision-count))
                          do (setf s (* 16 s))
                             (multiple-value-bind (h f)
                                 (truncate s)
                               (setf s f)
                               (let ((char (digit-char h 16)))
                                 (if upperp
                                     (princ char str)
                                     (princ (char-downcase char) str))))
                          if first
                            do (princ #\. str)
                               (setf first nil)
                          else do (decf precision-count)))))))
           (trailing-zeros (if (plusp precision-count)
                               (zeros precision-count)
                               ""))
           (pre-num (strcat pre-pre-num trailing-zeros))
           (sign (cond ((and plus-flag (not (minusp sign)))
                        "+")
                       ((and space-flag (not (minusp sign)))
                        " ")
                       ((minusp sign)
                        "-")
                       (t
                        "")))
           (prefix (format nil "~A0~C" sign x))
           (num (format nil "~A~C~@D" pre-num p (if (zerop argument) 0 (- exponent 4))))
           (num-length (+ (length num) (length prefix)))
           (padp (< num-length field-width))
           (pad-length (- field-width num-length))
           (field (cond ((and minus-flag padp)
                         (strcat prefix num (spaces pad-length)))
                        ((and zero-flag padp)
                         (strcat prefix (zeros pad-length) num))
                        (padp
                         (strcat (spaces pad-length) prefix num))
                        (t
                         (strcat prefix num)))))
      field)))

(defun convert-integer (argument flags field-width precision
                        &key radix upperp signedp)
  (with-flags (flags)
    (let ((field-width (or field-width 0)))
      (let* ((sign (cond ((not signedp)
                          "")
                         ((minusp argument)
                          "-")
                         ((and plus-flag (not (minusp argument)))
                          "+")
                         ((and space-flag (not (minusp argument)))
                          " ")
                         (t
                          "")))
             (sign-length (length sign))
             (num (if (and (zerop argument) precision (zerop precision))
                      (if (and hash-flag (= 8 radix))
                          "0"
                          "")
                      (format nil "~V,V,'0R" radix (or precision 1) (abs argument))))
             (alt (if hash-flag
                      (ecase radix
                        (10 "")
                        (16 (if (zerop argument) "" "0X"))
                        (8 (if (char= #\0 (char num 0)) "" "0")))
                      ""))
             (alt-length (length alt))
             (num-length (+ (length num) sign-length alt-length))
             (padp (< num-length field-width))
             (pad-length (- field-width num-length))
             (field (cond ((and minus-flag padp)
                           (strcat sign alt num (spaces pad-length)))
                          ((and zero-flag padp (not precision))
                           (strcat sign alt (zeros pad-length) num))
                          (padp
                           (strcat (spaces pad-length) sign alt num))
                          (t
                           (strcat sign alt num)))))
        (if upperp field (string-downcase field))))))

;;; MAIN PARSER

(defun handle-conversion-specification (stream format-string format-string-index
                                        arguments arguments-index)
  (let ((flags 0)
        (field-width nil)
        (precision nil)
        (length-modifier '()))
    (loop for char = (char format-string format-string-index)
          while (find char *flag-characters*)
          do (setf flags (set-flag flags char))
             (incf format-string-index))
    (if (char= #\* (char format-string format-string-index))
        (progn (setf field-width :arg) (incf format-string-index))
        (let ((digit? (digit-char-p (char format-string format-string-index))))
          (when (and digit? (plusp digit?))
            (setf field-width digit?)
            (incf format-string-index)
            (loop for char = (char format-string format-string-index)
                  for digit = (digit-char-p char)
                  while digit
              do (setf field-width (+ (* 10 field-width) digit))
                 (incf format-string-index)))))
    (when (char= #\. (char format-string format-string-index))
      (incf format-string-index)
      (if (char= #\* (char format-string format-string-index))
          (progn (setf precision :arg) (incf format-string-index))
          (progn
            (setf precision 0)
            (loop for char = (char format-string format-string-index)
                  for digit = (digit-char-p char)
                  while digit
                  do (setf precision (+ (* 10 precision) digit))
                     (incf format-string-index)))))
    (multiple-value-bind (lm idx)
        (collect-length-modifier *converter* format-string format-string-index)
      (setf length-modifier lm
            format-string-index idx))
    (multiple-value-bind (conversion-specifier-symbol idx)
        (collect-conversion-specifier *converter* format-string format-string-index)
      (setf format-string-index idx)
      (let* ((field-width (if (eq :arg field-width)
                              (let ((arg (elt arguments arguments-index)))
                                (incf arguments-index)
                                (when (minusp arg)
                                  (setf flags (set-flag flags #\-)))
                                (abs arg))
                              field-width))
             (precision (if (eq :arg precision)
                            (let ((arg (elt arguments arguments-index)))
                              (incf arguments-index)
                              (if (minusp arg)
                                  nil
                                  arg))
                            precision))
             (argument
               (retrieve-argument *converter*
                                  conversion-specifier-symbol
                                  length-modifier
                                  arguments
                                  arguments-index))
             (text
               (convert *converter*
                        conversion-specifier-symbol
                        argument
                        flags
                        field-width
                        precision))
             (printed (length text)))
        (princ text stream)
        (values printed (incf arguments-index) format-string-index)))))

(defun doprintf (stream format-string arguments)
  (let ((characters-printed 0)
        (arguments-index 0)
        (format-string-index 0))
    (loop while (< format-string-index (length format-string))
          for char = (char format-string format-string-index)
          if (char= #\% char)
            do
               (incf format-string-index)
               (let ((next (char format-string format-string-index)))
                 (if (char= #\% next)
                     (progn (princ char stream)
                            (incf characters-printed)
                            (incf format-string-index))
                     (multiple-value-bind
                           (printed args index)
                         (handle-conversion-specification
                          stream
                          format-string format-string-index
                          arguments arguments-index)
                       (incf characters-printed printed)
                       (setf arguments-index args
                             format-string-index index))))
          else
            do
               (princ char stream)
               (incf characters-printed)
               (incf format-string-index))
    characters-printed))

;;; PARSE-HEX-FLOAT

(defparameter *whitespace* (list #\Tab #\Newline #\Linefeed #\Page #\Return #\Space))

(defun whitespacep (char)
  (find char *whitespace*))

(defun parse-hex-float-error-reporter (format-string condition stream)
  (format stream format-string
          (parse-hex-float-error-index condition)
          (parse-hex-float-error-string condition)))

(define-condition parse-hex-float-error (simple-error)
  ((%string :initarg :string :reader parse-hex-float-error-string)
   (%index :initarg :index :reader parse-hex-float-error-index))
  (:documentation "The base for all PARSE-HEX-FLOAT errors."))

(define-condition missing-zero (parse-hex-float-error)
  ()
  (:report (lambda (condition stream)
             (parse-hex-float-error-reporter
              "Missing '0' at index ~D in '~A'"
              condition stream))))

(define-condition missing-x (parse-hex-float-error)
  ()
  (:report (lambda (condition stream)
             (parse-hex-float-error-reporter
              "Missing 'X' or 'x' at index ~D in '~A'"
              condition stream))))

(define-condition missing-p (parse-hex-float-error)
  ()
  (:report (lambda (condition stream)
             (parse-hex-float-error-reporter
              "Missing 'P' or 'p' at index ~D in '~A'"
              condition stream))))

(define-condition junk-in-string (parse-hex-float-error)
  ()
  (:report (lambda (condition stream)
             (parse-hex-float-error-reporter
              "Junk at index ~D in '~A'"
              condition stream))))

(define-condition missing-exponent (parse-hex-float-error)
  ()
  (:report (lambda (condition stream)
             (parse-hex-float-error-reporter
              "Expected exponent ([+-][d]+) at index ~D in '~A'"
              condition stream))))

;; [+-]?0[xX]h[[.]h*]?[pP][+-]d+

(defun parse-hex-float (string &key (start 0) end junk-allowed)
  (let ((end (or end (length string)))
        (index start)
        (sign 1)
        (significand 0)
        (exponent 0))
    (labels ((forward ()
               (incf index))
             (next ()
               (if (>= index end)
                   nil
                   (char string index)))
             (looking-at (char)
               (char= char (char string index)))
             (skip-ws ()
               (loop for char = (next)
                     while (and char (whitespacep char))
                     do (forward))))
      (skip-ws)
      (cond ((looking-at #\+)
             (forward))
            ((looking-at #\-)
             (setf sign -1)
             (forward))
            (t nil))
      (if (looking-at #\0)
          (forward)
          (error 'missing-zero :string string :index index))
      (if (or (looking-at #\x)
              (looking-at #\X))
          (forward)
          (error 'missing-x :string string :index index))
      (multiple-value-bind (whole pos)
          (parse-integer string :start index :radix 16 :junk-allowed t)
        (setf index pos
              significand whole))
      (when (looking-at #\.)
        (forward)
        (loop for char = (next)
              for exp downfrom -1
              until (or (null char) (char-equal #\p char))
              do (setf significand
                       (+ significand (* (digit-char-p char 16)
                                         (expt 16 exp))))
                 (forward)))
      (if (or (looking-at #\p)
              (looking-at #\P))
          (forward)
          (error 'missing-p :string string :index index))
      (multiple-value-bind (whole pos)
          (parse-integer string :start index :radix 10 :junk-allowed t)
        (unless whole
          (error 'missing-exponent :string string :index index))
        (setf index pos
              exponent whole))
      (skip-ws)
      (if (or (>= index end) junk-allowed)
          (values (float (* sign significand (expt 2 exponent)) 1l0) index)
          (error 'junk-in-string :string string :index index)))))
