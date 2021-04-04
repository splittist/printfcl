;;;; package.lisp

(defpackage #:printfcl
  (:use #:cl)
  (:export #:printf
           #:fprintf
           #:sprintf
           #:parse-hex-float

           #:*converter*
           #:*length-modifiers*

           #:standard-converter

           #:collect-length-modifier
           #:collect-conversion-specifier
           #:retrieve-argument
           #:convert
           #:float-nan-p
           #:float-infinity-p
           #:negaive-infinity-p

           #:with-flags
           #:minus-flag
           #:plus-flag
           #:space-flag
           #:hash-flag
           #:zero-flag
           #:set-flag

           #:strcat
           #:spaces
           #:zeros))
