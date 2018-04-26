;;;; test/date.lisp

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

(defpackage "CL-GREGORIAN-DATE/TEST/DATE"
  (:use "CL"
        "LISP-UNIT")
  (:import-from "CL-GREGORIAN-DATE/CORE/DATE")
  (:export "TEST-DATEP"
           "TEST-DATE="
           "TEST-DATE/="
           "TEST-DATE<"
           "TEST-DATE>"
           "TEST-DATE<="
           "TEST-DATE>="
           "TEST-PARSE-DATE"
           "TEST-ENCODE-DATE"
           "TEST-DECODE-DATE"))

(in-package "CL-GREGORIAN-DATE/TEST/DATE")

(defvar *date1* nil)

(defvar *date2* nil)

(defvar *date3* nil)

(defun setup ()
  (setf *date1*
        (make-instance 'cl-gregorian-date:date :year 2012 :month 1 :day 6)
        *date2*
        (make-instance 'cl-gregorian-date:date :year 2012 :month 1 :day 7)
        *date3*
        (make-instance 'cl-gregorian-date:date :year 2012 :month 1 :day 6)))

(define-test test-datep
  (setup)
  (assert-true (cl-gregorian-date:datep *date1*))
  (assert-false (cl-gregorian-date:datep nil)))

(define-test test-date=
  (setup)
  (assert-true (cl-gregorian-date:date= *date1* *date1*))
  (assert-false (cl-gregorian-date:date= *date1* *date2*))
  (assert-true (cl-gregorian-date:date= *date1* *date3*)))

(define-test test-date/=
  (setup)
  (assert-false (cl-gregorian-date:date/= *date1* *date1*))
  (assert-true (cl-gregorian-date:date/= *date1* *date2*))
  (assert-false (cl-gregorian-date:date/= *date1* *date3*)))

(define-test test-date<
  (setup)
  (assert-false (cl-gregorian-date:date< *date1* *date1*))
  (assert-true (cl-gregorian-date:date< *date1* *date2*))
  (assert-false (cl-gregorian-date:date< *date2* *date1*))
  (assert-false (cl-gregorian-date:date< *date1* *date3*)))

(define-test test-date>
  (setup)
  (assert-false (cl-gregorian-date:date> *date1* *date1*))
  (assert-false (cl-gregorian-date:date> *date1* *date2*))
  (assert-true (cl-gregorian-date:date> *date2* *date1*))
  (assert-false (cl-gregorian-date:date> *date1* *date3*)))

(define-test test-date<=
  (setup)
  (assert-true (cl-gregorian-date:date<= *date1* *date1*))
  (assert-true (cl-gregorian-date:date<= *date1* *date2*))
  (assert-false (cl-gregorian-date:date<= *date2* *date1*))
  (assert-true (cl-gregorian-date:date<= *date1* *date3*)))

(define-test test-date>=
  (setup)
  (assert-true (cl-gregorian-date:date>= *date1* *date1*))
  (assert-false (cl-gregorian-date:date>= *date1* *date2*))
  (assert-true (cl-gregorian-date:date>= *date2* *date1*))
  (assert-true (cl-gregorian-date:date>= *date1* *date3*)))

(define-test test-parse-date
  (setup)
  (assert-equal nil (cl-gregorian-date:parse-date ""))
  (assert-error 'type-error (cl-gregorian-date:parse-date "2012-01-32"))
  (assert-error 'type-error (cl-gregorian-date:parse-date "2012-13-06"))
  (assert-equality #'cl-gregorian-date:date= *date1*
                   (cl-gregorian-date:parse-date "2012-01-06")))

(define-test test-encode-date
  (setup)
  (assert-error 'type-error (cl-gregorian-date:encode-date 2012 1 32))
  (assert-error 'type-error (cl-gregorian-date:encode-date 2012 13 6))
  (assert-equality #'cl-gregorian-date:date= *date1*
                   (cl-gregorian-date:encode-date 2012 1 6)))

(define-test test-decode-date
  (setup)
  (assert-error 'type-error (cl-gregorian-date:decode-date nil))
  (assert-equal (list 2012 1 6)
                (multiple-value-list (cl-gregorian-date:decode-date *date1*))))
