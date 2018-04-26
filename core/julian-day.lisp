;;;; core/julian-day.lisp

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

(defpackage "CL-GREGORIAN-DATE/CORE/JULIAN-DAY"
  (:use "CL")
  (:import-from "CL-GREGORIAN-DATE/CORE/DECODED-TIME")
  (:export "JULIAN-DAY"
           "+GREGORIAN-EPOCH+"
           "+MODIFIED-JULIAN-DAY-EPOCH+"
           "ENCODE-JULIAN-DAY"
           "DECODE-JULIAN-DAY"
           "MODIFIED-JULIAN-DAY"
           "UNMODIFIED-JULIAN-DAY"))

(in-package "CL-GREGORIAN-DATE/CORE/JULIAN-DAY")

(deftype julian-day ()
  "A rational number representing the number of days and fractions
of a day since 12 hours Universal Time (Greenwich mean noon) on
January 1 of the year -4712, where the year is given in the Julian
proleptic calendar."
  '(rational 0))

(defconstant +gregorian-epoch+ #.(rationalize 1721118.5)
             "The Julian Day for midnight of February 29 of year 0 in the
Gregorian calendar.")

(defconstant +modified-julian-day-epoch+ #.(rationalize 2400000.5)
             "The Julian Day for the Modified Julian Day epoch, midnight,
Wednesday November 17, 1858.")

(defconstant +days-per-year+ 365
  "The number of days per year.")

(defparameter *days-in-previous-march-based-months*
  (vector 306 337 0 31 61 92 122 153 184 214 245 275)
  "The number of days in the previous months in the March-based
calendar.")

(defparameter *days-in-previous-january-based-months*
  (vector 0 31 61 92 122 153 184 214 245 275 306 337)
  "The number of days in the previous months in the January-based
calendar.")

(defconstant +constant-k+ 1/4
  "The constant used to correct century and 400-year approximations.")

(defconstant +avg-days-per-century+ #.(rationalize 36524.25)
             "The average number of days per century.")

(defconstant +avg-days-per-year+ #.(rationalize 365.25)
             "The average number of days per year.")

(defun days-in-previous-march-based-months (month)
  "Returns the number of days in the previous months in the
March-based calendar. Assumes month ranges 1 to 12."
  (aref *days-in-previous-march-based-months* (1- month)))

(defun days-in-previous-january-based-months (month)
  "Returns the number of days in the previous months in the
January-based calendar. Assumes month ranges 3 to 14."
  (aref *days-in-previous-january-based-months* (- month 3)))

(defun march-based-year-month (year month)
  "Returns the year and month adjusted so that January and February
are considered months 13 and 14 of the prior year."
  (if (< month 3)
      (values (1- year) (+ month 12))
      (values year month)))

(defun january-based-year-month (year month)
  "Returns the year and month adjusted so that January and February
are considered months 1 and 2 of the year."
  (if (< 12 month)
      (values (1+ year) (- month 12))
      (values year month)))

(defun year-as-days (year)
  "Returns the number of days up to and including year, accounting for
leap years. There is a leap year every 4 years, except for
centennials, unless also divisible by 400."
  (+ (* year +days-per-year+)
     (- (floor year 4)
        (floor year 100))
     (floor year 400)))

(defun encode-julian-day (year month day)
  "Returns the Julian Day given the year, month, and day in the
Gregorian Calendar."
  (+ (rationalize day)
     (days-in-previous-march-based-months month) ; Don't adjust month if using this.
     (year-as-days (march-based-year-month year month))
     +gregorian-epoch+))

(defun decode-julian-day (julian-day)
  "Returns the year, month, and day in the Gregorian Calendar
represented by the given Julian Day."
  (check-type julian-day julian-day)
  (let ((gjd (- julian-day +gregorian-epoch+))) ; 1. base date in Gregorian year 0
    (multiple-value-bind (z r)
        (floor gjd)
      (let* ((g (- z +constant-k+))
             (a (floor g +avg-days-per-century+)) ; 2. number of full centuries
             (b (- a (floor a 4))) ; 3. days w/in full centuries by adding back days removed in Greg. cal.
             (year (floor (+ b g) +avg-days-per-year+)) ; 3 & 4
             (c (- (+ b z) (floor (* +avg-days-per-year+ year)))) ; 5. day count in current year
             (month (truncate (+ (* 5 c) 456) 153)) ; 6. month in current year
             (day (+ (- c (days-in-previous-january-based-months month)) r))) ; 7 & 8. day of month
        (multiple-value-bind (year month) ; 9 conver to January based year and month
            (january-based-year-month year month)
          (values year month day))))))

(defun modified-julian-day (julian-day)
  "Returns the Modified Julian Day for the given Julian Day."
  (- julian-day +modified-julian-day-epoch+))

(defun unmodified-julian-day (modified-julian-day)
  "Returns the Julian Day for the given Modified Julian Day."
  (+ modified-julian-day +modified-julian-day-epoch+))
