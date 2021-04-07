# printfcl

A configurable implementation of `printf` in Common Lisp.

## Quickstart

Clone this repository into your `quicklisp/local-packages` directory, then `(ql:quickload "printfcl")`, and switch into the `PRINTFCL` package.

```lisp
PRINTFCL> (printf "pi = %.5f" (* 4 (atan 1.0)))

pi = 3.14159
12

PRINTFCL> (sprintf "%10s %-10s" "Hello" "World")

"     Hello World     "

PRINTFCL> (fprintf t "%A" 123.45)

0XF.6E666P+3
12
```

* [Introduction](#introduction)
* [Functions](#functions)
* [Format](#format)
* [Configuring](#configuring)
* [Handy Utilities](#handy-utilities)
* [Examples](#examples)
  * [IEE Floats - using *length modifiers*](#ieee)
  * [Golike t/T - adding *conversion specifiers*](#golike)
  * [Javaesque h/H - converters as mix-ins](#javah)
  * [Pythonish strings - overriding a *conversion specifier*](#pythons)
  * [Javatime - a different form of *conversion specifier*](#javatime)
* [Comments, questions and suggestions](#comments)

<a id="introduction"></a>
## Introduction

Sometimes you just want to reproduce the effect of `C`-style `printf` output without having to produce your own `CL:FORMAT` string. **printfcl** reproduces the effect of `printf` conversion on lisp objects. Many of the standard conversion specifiers are supported.

**printfcl** is intended to be configurable. **printfcl** is *not* intended to be a replacement for `CL:FORMAT`.

**printfcl** has no dependencies and an MIT license.

<a id="functions"></a>
## Functions

The **printf**-family functions are:

*function* **PRINTF** `format-string` *&rest* `arguments`

Prints the `arguments`, lisp objects (but see below), to `*STANDARD-OUTPUT*` as converted in accordance with the `format-string` (see below). Returns the number of characters printed.

*function* **SPRINTF** `format-string` *&rest* `arguments`

As for **PRINTF**, but returns the result as a string.

*function* **FPRINTF** `stream` `format-string` *&rest* `arguments`

As for **PRINTF**, but prints to `stream`. If `stream` is `T`, prints to `*STANDARD-OUTPUT*`. If `stream` is `NIL`, returns a string (as for **SPRINTF**). Unless `stream` is `NIL`, returns the number of characters printed.

*function* **PARSE-HEX-FLOAT** `string` *&key* `(start 0)` `end` `junk-allowed`

Attempts to parse `string` as an `%a`-style hexidecimal float (possibly surrounded by whitespace). The other arguments are as for `CL:PARSE-INTEGER`. Returns a double-float and, as a second value, the position in `string` where parsing stopped.

<a id="format"></a>
## Format

The `format-string` is simply copied verbatim, except when a conversion specification is encountered. The following description is valid for the **STANDARD-CONVERTER**. (See below at [**Configuring**](#configuring) for more information on what this means.)

A conversion specification begins with a `%` sign, followed by (in order):

* Zero or more *flag characters* (`-`, `+`, `0`, `#` and *space*).

* An optional *field width*, either a positive decimal integer or an asterisk (indicating that the quantity should be taken from an `argument`).

* An optional *precision*, a period (`.`) optionally followed by a decimal integer or an asterisk (indicating that the quantity should be taken from an `argument`.

* An optional *length modifier*, being any number of letters in any order from the string `*LENGTH-MODIFIERS*`, the default being `"hlqLjzZt"`.

* The *conversion specifier*, a single character from the set `a`, `A`, `c`, `d`, `e`, `E`, `f`, `F`, `g`, `G`, `i`, `o`, `s`, `u`, `x` and `X`.

A `%` immediately followed by a `%` causes a single `%` to be copied to the output.

*NOTE:* Unlike C99, the STANDARD-CONVERTER accepts `F` (the upper-case version of `f`), but does not accept `p` (pointer) or `n` (which is discouraged in anyway) as being un-lispy.

The STANDARD-CONVERTER seeks to reproduce the effect of (mainly) C99 printf when applied to arguments of lisp objects. For this reason it snarfs up as many *length modifiers* as it can see, then ignores them.

For the effect of *flag characters*, *field width*, *precision* and *conversion specifier* please refer to the closest [man page](https://www.man7.org/linux/man-pages/man3/printf.3.html) or language specification.

<a id="configuring"></a>
## Configuring

Because not all printfs are C99, **printfcl** provides a variety of mechanisms for configuring the processing of conversion specifications. Configuration is primarily done by specialising generic functions on a *converter* class with an instance bound to `*CONVERTER*`. By default this is an instance of `STANDARD-CONVERTER`. A number of examples follow the description of the configuration protocol.

*special variable* **\*CONVERTER\***

Bound to an instance of a class used to specialise generic functions of the configuration protocol. By default, an instance of `STANDARD-CONVERTER`.

*special variable* **\*LENGTH-MODIFIERS\***

Bound to a sequence of characters used by the STANDARD-CONVERTER to accumulate *length modifiers* (prior to being ignored). By default `"hlqLjzZt"`.

*class* **STANDARD-CONVERTER**

The class upon which the default behaviour of **printfcl** is specialised.

*generic function* **COLLECT-LENGTH-MODIFIER** `converter` `format-string` `format-string-index`

Given the entire `format-string` passed to the **printf**-family function and the index at which to begin parsing (`format-string-index`), return two values: the *length modifier* (in a form to be used by `RETRIEVE-ARGUMENT`) and the updated `format-string-index` (beyond the last character consumed).

*generic function* **COLLECT-CONVERSION-SPECIFIER** `converter` `format-string` `format-string-index`

Given the entire `format-string` passed to the **printf**-family function and the index at which to begin parsing (`format-string-index`), return two values: the *conversion specifer* (in a form to be used by `CONVERT`, usually a symbol) and the updated `format-string-index` (beyond the last character consumed).

*generic function* **RETRIEVE-ARGUMENT** `converter` `conversion-specifier` `length-modifer` `arguments` `index`

Given the `conversion-specifier` returned by `COLLECT-CONVERSION-SPECIFIER`, the `length-modifier` returned by `COLLECT-LENGTH-MODIFIER`, all the `arguments` passed to the **printf**-family function and the `index` of the appropriate argument in `arguments` to use, return the *argument* in a form to be used by `CONVERT`.

*generic function* **CONVERT** `converter` `conversion-specifier` `argument` `flags` `field-width` `precision`

Given the `conversion-specifier` returned by `COLLECT-CONVERSION-SPECIFIER`, the `argument` returned by `RETRIEVE-ARGUMENT` and the `flags`, `field-width` and `precision` specified in the *format string* passed to the **printf**-family function, return a string representing the `argument` as appropriately converted.

*generic function* **FLOAT-NAN-P** `converter` `object`

Return a generalised boolean indicating whether `object` should be treated as a floating point *Not-a-Number*.

*generic function* **FLOAT-INFINITY-P** `converter` `object`

Return a generalised boolean indicating whether `object` should be treated as a floating point *Infinity*.

*generic function* **NEGATIVE-INFINITY-P** `converter` `object`

Return a generalised boolean indicating whetner `object` should be treated as a floating point *Negative Infinity*.

<a id="handy-utilities"></a>
## Handy Utilities

The following items assist in writing converters.

*macro* **WITH-FLAGS** `flags` *&body* `body`

Within `body`, the following symbol macros are bound to functions extracting the status of the corresponding flag.

*symbol macros* **MINUS-FLAG**, **PLUS-FLAG**, **SPACE-FLAG**, **HASH-FLAG**, **ZERO-FLAG**

Within the body of a `WITH-FLAGS`, true or false depending on whether the corresponding flag has been set.

*function* **SET-FLAG** `flags` `character`

Return a modified version of `flags` with the flag corresponding to `character` (a character) set.

*function* **STRCAT** *&rest* `strings`

Return `strings` concatenated as a single string.

*function* **SPACES** `length`

Return a string of `length` spaces.

*function* **ZEROS** `length`

Return a string of `length` zeros.

<a id="examples"></a>
## Examples

<a id="ieee"></a>
### IEEE Floats - using *length modifiers*

We have some IEEE double precision ("binary64") and extended precision ("extended 80 bit") floats (as integers) to be interpreted in accordance with the "L" length modifier. They might include infinities and NaNs. The format string includes "%Lf" and "%Le" conversion specifiers.

First, imagine we have the following decoding routines:

```lisp
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
```

We can see that these routines return `:nan`, `:+inf` and `:-inf` rather than our implementation's native *NaN* and *infinity* formats (if any). Therefore, in addition to properly dealing with the "L" length modifier, we will want to appropriately interpret these symbols.

First, we create a converter:

```lisp
(defclass ieee-float-converter (standard-converter)
  ())
```

Now we can specialise `RETRIEVE-ARGUMENT` for our converter and the *conversion specifiers* that we care about. (We keep as much of the STANDARD-CONVERTER machinery as we can.)

```lisp
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
```

Finally, we deal with the particular format used for infinities etc.

```lisp
(defmethod float-nan-p ((converter ieee-float-converter) obj)
  (eq obj :nan))

(defmethod float-infinity-p ((converter ieee-float-converter) obj)
  (or (eq obj :+inf) (eq obj :-inf)))

(defmethod negative-infinity-p ((converter ieee-float-converter) obj)
  (eq obj :-inf))
```

And we test:

```lisp
(let ((*converter* (make-instance 'ieee-float-converter)))
  (printf "%f %Lf %e %Le %Lf"
          #b0100100011111110001000100010111010000010011001101101001001111111
          #x400c8ca2000000000000
          #b0100100011111110001000100010111010000010011001101101001001111111
          #x400c8ca2000000000000
          #x7fff8000000000000000
         ))
=>
42000000000000000000000000000000000000000000.000000 9000.500000 4.200000e+43 9.000500e+03 inf
93
```

Yay.

<a id="golike"></a>
### Golike t/T - adding *conversion specifiers*

Go's version of printf has a `t` conversion specifier that prints the truth value of its corresponding argument, and a `T` conversion specifier that prints its type.

We can implement a pale simulacrum of this as follows:

```lisp
(defclass golike-t-converter (standard-converter)
  ())

(defmethod convert ((converter golike-t-converter) (cs (eql :|T|))
                    argument flags field-width precision)
  (convert-string (prin1-to-string (type-of argument)) flags field-width nil))

(defmethod convert ((converter golike-t-converter) (cs (eql :|t|))
                    argument flags field-width precision)
  (let ((arg (if argument "true" "false")))
    (convert-string arg flags field-width nil)))
```

When we test this, remember that "t" is a standard *length modifier*, so we'll have to remove it.

```lisp
(let ((*converter* (make-instance 'golike-T-converter))
      (*length-modifiers* (remove #\t *length-modifiers*)))
  (printf "%T %T %T %t %t" 1 "foo" *converter* t nil))
=>
BIT (SIMPLE-ARRAY CHARACTER (3)) GOLIKE-T-CONVERTER true false
62
```

<a id="javah"></a>
### Javaesque h/H - converters as mix-ins

Java's printf has `h` and `H` conversion specifiers that print the hashCode() of the argument (or "null"). (`H` is the upper-case version of `h`.)

In this example we combine these conversion specifiers with the IEEE converter, treating the Javaesque version as a mix-in class.

```lisp
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
```

When testing this time we keep only the *length modifier* we need:

```lisp
(let ((*converter* (make-instance 'my-converter))
      (*length-modifiers* "L"))
  (printf "%.e %h" #x48FE222E8266D27F *converter*))
=>
4e+43 0x1b969bf9c19f1840
24
```
(Your results will differ.)

<a id="pythons"></a>
### Pythonish strings - overriding a *conversion specifier*

Python's `s` conversion specifier calls `__str__` on its argument; its `r` conversion specifier calls `__repr__`. We can counterfeit this with calls to `CL:PRINC[-TO-STRING]` and `CL:PRIN1[-TO-STRING]`, respectively.

```lisp
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
```

The test is simpele:

```lisp
(let ((*converter* (make-instance 'pythonish-strings-converter)))
  (printf "%s %r" "Hello" "World"))
=>
Hello "World"
13
```

<a id="javatime"></a>
### Javatime - a different form of *conversion specifier*

Java's [version](https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html) of printf contains a large number of *conversion specifiers* relating to dates and times. The system `printfcl/javatime` contins an implementation of these conversion specifiers using the `local-time` system (but assuming unix timestamps are given as arguments).

Because the time- and date-based conversion specifiers are made up of two characters (e.g. `Ta` or `tC`) we must customize `COLLECT-CONVERSION-SPECIFIER`. Here we handle the `T/t` case and otherwise defer to the STANDARD-CONVERTER with `(call-next-method)`.

Because of the sheer number of suffix specifiers, we'll use a similary strategy by routing all `CONVERT` calls though a method specialised on our converter, handling the `T\t` case there (for the sake of the example) and handing the rest on. This method includes an example of the use of `WITH-FLAGS` and the padding logic.

The key **printfcl** items are below. (Other functions can be found in the `javatime.lisp` file.)

```lisp
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

```

Test time:

```lisp
(let ((*converter* (make-instance 'javatime-converter))
      (*length-modifiers* (remove #\t *length-modifiers*)))
  (printf "%s %tc" "The time is:" (timestamp-to-unix (now))))
=>
The time is: Sun Apr 04 12:21:58 UTC 2021
41
```

<a id="comments"></a>
## Comments, questions and suggestions

All comments, questions and suggestions are welcomed.

## License

MIT

