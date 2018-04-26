;;;; test/julian-day.lisp

;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2004 Michael J. Forster
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

;;; Date algorithms
;;; Copyright 1998, Peter Baum
;;;
;;; Although all rights are reserved under the copyright laws, programmers
;;; may implement the algorithms without permission. However, I would ask
;;; that if this is done for any of the algorithms that are not known to
;;; be fully documented elsewhere, a reference be made to this
;;; document. Doing so will assure full and consistent documentation,
;;; minimize the propagation of any errors that might be discovered, and
;;; provide a central location for any improvements that are made in the
;;; future.
;;;
;;; http://web.archive.org/web/20061029060658/http://vsg.cape.com/~pbaum/date/date0.htm

(defpackage "CL-GREGORIAN-DATE/TEST/JULIAN-DAY"
  (:use "CL"
        "LISP-UNIT")
  (:import-from "CL-GREGORIAN-DATE/CORE/JULIAN-DAY")
  (:export "TEST-ENCODE-JULIAN-DAY-REFERENCE"
           "TEST-ENCODE-MIDDLE-JULIAN-DAY-REFERENCE"
           "TEST-ENCODE-GREGORIAN-EPOCH"
           "TEST-ENCODE-UNIX-EPOCH"
           "TEST-DECODE-ENCODE"
           "UNMODIFIED-MODIFIED"))

(in-package "CL-GREGORIAN-DATE/TEST/JULIAN-DAY")

(define-test test-encode-julian-day-reference
  "Julian Day of reference date."
  (assert-equal
   (cl-gregorian-date/core/julian-day:encode-julian-day -4713 11 24.5)
   (rationalize 0.0)))

(define-test test-encode-middle-julian-day-reference
  "Julian Day of midnight of reference date."
  (assert-equal
   (cl-gregorian-date/core/julian-day:encode-julian-day -4713 11 25)
   (rationalize 0.5)))

(define-test test-encode-gregorian-epoch
  "Julian Day of Gregorian-Julian offset."
  (assert-equal
   (cl-gregorian-date/core/julian-day:encode-julian-day 0 2 29)
   cl-gregorian-date/core/julian-day:+gregorian-epoch+))

(define-test test-encode-unix-epoch
  "Julian Day of Unix Epoch."
  (assert-equal
   (cl-gregorian-date/core/julian-day:encode-julian-day 1970 1 1)
   (rationalize 2440587.5)))

(define-test test-decode-encode
  "Decoding an encoded date gives the original date."
  (assert-equal
   (multiple-value-list
    (cl-gregorian-date/core/julian-day:decode-julian-day
     (cl-gregorian-date/core/julian-day:encode-julian-day -4713 11 24.5)))
   (list -4713 11 (rationalize 24.5)))
  (assert-equal
   (multiple-value-list
    (cl-gregorian-date/core/julian-day:decode-julian-day
     (cl-gregorian-date/core/julian-day:encode-julian-day -4713 11 25)))
   (list -4713 11 25))
  (assert-equal
   (multiple-value-list
    (cl-gregorian-date/core/julian-day:decode-julian-day
     (cl-gregorian-date/core/julian-day:encode-julian-day 0 2 29)))
   (list 0 2 29))
  (assert-equal
   (multiple-value-list
    (cl-gregorian-date/core/julian-day:decode-julian-day
     (cl-gregorian-date/core/julian-day:encode-julian-day 1970 1 1)))
   (list 1970 1 1)))

(define-test unmodified-modified
  "Unmodifying a modified Julian Day gives the orignal Julian Day."
  (assert-equal
   (cl-gregorian-date/core/julian-day:unmodified-julian-day
    (cl-gregorian-date/core/julian-day:modified-julian-day
     (cl-gregorian-date/core/julian-day:encode-julian-day -4713 11 24.5)))
   (cl-gregorian-date/core/julian-day:encode-julian-day -4713 11 24.5)))
