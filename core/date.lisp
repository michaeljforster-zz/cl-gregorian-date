;;;; core/date.lisp

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

(defpackage "CL-GREGORIAN-DATE/CORE/DATE"
  (:use "CL")
  (:import-from "CL-PPCRE")
  (:import-from "CL-GREGORIAN-DATE/CORE/DECODED-TIME")
  (:import-from "CL-GREGORIAN-DATE/CORE/JULIAN-DAY")
  (:export "DATE"
           "DATEP"
           "DATE="
           "DATE/="
           "DATE<"
           "DATE>"
           "DATE<="
           "DATE>="
           "PARSE-DATE"
           "ENCODE-DATE"
           "DECODE-DATE"
           "GET-DATE"
           "GET-LOCAL-DATE"))

(in-package "CL-GREGORIAN-DATE/CORE/DATE")

(defclass date ()
  ((julian-day))
  (:documentation
   "The class representing a date in the Gregorian Calendar.

A DATE is a point in time with a resolution of one calendar day. A
DATE has a temporal extent of one day, and the minimal non-zero
difference between any two instances of DATE is one day.

A DATE is time zone independant and evaluates to the same year, month,
and day in any time zone."))

(defmethod initialize-instance :after
    ((object date) &key year month day julian-day)
  (if julian-day
      (progn
        (check-type julian-day cl-gregorian-date/core/julian-day:julian-day)
        (setf (slot-value object 'julian-day) julian-day))
      (progn
        (check-type year
                    cl-gregorian-date/core/decoded-time:decoded-time-year)
        (check-type month
                    cl-gregorian-date/core/decoded-time:decoded-time-month)
        (check-type day
                    cl-gregorian-date/core/decoded-time:decoded-time-date)
        (setf (slot-value object 'julian-day)
              (cl-gregorian-date/core/julian-day:encode-julian-day
               year month day)))))

(defun %decode-date (date)
  (cl-gregorian-date/core/julian-day:decode-julian-day
   (slot-value date 'julian-day)))

(defun datep (object)
  "Returns true if object is a DATE; otherwise, returns false."
  (typep object 'date))

(defun date= (date1 date2)
  "Returns true if the values of date1 and date2 are the same; otherwise, returns false."
  (= (slot-value date1 'julian-day)
     (slot-value date2 'julian-day)))

(defun date/= (date1 date2)
  "Returns true if the values of date1 and date2 are not the same; otherwise, returns false."
  (/= (slot-value date1 'julian-day)
      (slot-value date2 'julian-day)))

(defun date< (date1 date2)
  "Returns true if the value of time1 is less than that of time2; otherwise, returns false."
  (< (slot-value date1 'julian-day)
     (slot-value date2 'julian-day)))

(defun date> (date1 date2)
  "Returns true if the value of time1 is greater than that of time2; otherwise, returns false."
  (> (slot-value date1 'julian-day)
     (slot-value date2 'julian-day)))

(defun date<= (date1 date2)
  "Returns true if the value of time1 is less than or equal to that of time2; otherwise, return
false."
  (<= (slot-value date1 'julian-day)
      (slot-value date2 'julian-day)))

(defun date>= (date1 date2)
  "Returns true if the value of time1 is greater than or equal to that of time2; otherwise, return
false."
  (>= (slot-value date1 'julian-day)
      (slot-value date2 'julian-day)))

(defparameter *iso-8601-extended-date-formatter*
  (formatter "~4,'0D-~2,'0D-~2,'0D")
  "The format control for the ISO 8601 date extended format.")

(defmethod print-object ((object date) stream)
  (multiple-value-bind (year month day)
      (%decode-date object)
    (if (or *print-escape* *print-readably*)
        (print-unreadable-object (object stream :type t :identity t)
          (format stream *iso-8601-extended-date-formatter* year month day))
        (format stream *iso-8601-extended-date-formatter* year month day))))

(defparameter *iso-8601-extended-date-scanner*
  (cl-ppcre:create-scanner "^(\\d{4})-(\\d{2})-(\\d{2})$")
  "The regular expression scanner for the ISO 8601 date extended format.")

(defun parse-date (string)
  (check-type string string)
  (cl-ppcre:register-groups-bind
      ((#'parse-integer year) (#'parse-integer month) (#'parse-integer day))
      (*iso-8601-extended-date-scanner* string)
    (make-instance 'date :year year :month month :day day)))

(defun encode-date (year month day)
  "Returns a DATE for the decoded-time year, month, and day."
  (make-instance 'date :year year :month month :day day))

(defun decode-date (date)
  (check-type date date)
  (%decode-date date))

(defun get-date ()
  "Returns the current date as a DATE."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (get-universal-time) 0) ; UTC
    (declare (ignore second minute hour day daylight-p zone))
    (make-instance 'date :year year :month month :day date)))

(defun get-local-date ()
  "Returns the current date as a DATE."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (get-universal-time)) ; local time
    (declare (ignore second minute hour day daylight-p zone))
    (make-instance 'date :year year :month month :day date)))
