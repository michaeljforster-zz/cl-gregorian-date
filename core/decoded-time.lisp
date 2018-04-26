;;;; core/decoded-time.lisp

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

(defpackage "CL-GREGORIAN-DATE/CORE/DECODED-TIME"
  (:use "CL")
  (:export "DECODED-TIME-DATE"
           "DECODED-TIME-MONTH"
           "DECODED-TIME-YEAR"))

(in-package "CL-GREGORIAN-DATE/CORE/DECODED-TIME")

(deftype decoded-time-date ()
  "An integer between 1 and 31, inclusive, representing a day of
month. See CLHS, Section 25.1.4.1 (Decoded Time)."
  '(integer 1 31))

(deftype decoded-time-month ()
  "An integer between 1 and 12, inclusive, representing a month, where
January is 1 and December is 12. See CLHS, Section 25.1.4.1 (Decoded
Time)."
  '(integer 1 12))

(deftype decoded-time-year ()
  "A non-negative integer representing a year. See CLHS, Section
25.1.4.1 (Decoded Time)."
  '(integer 0))
