# cl-gregorian-date

cl-gregorian-date is a Common Lisp library for handling dates in the
Gregorian calendar.

cl-gregorian-date depends on
[CL-PPCRE](https://edicl.github.io/cl-ppcre/) and
[LISP-UNIT](https://github.com/OdonataResearchLLC/lisp-unit).

cl-gregorian-date is being developed with [SBCL](http://sbcl.org/),
[CCL](http://ccl.clozure.com/), and
[LispWorks](http://www.lispworks.com/) on OS X.  cl-gregorian-date is
being deployed with SBCL on FreeBSD/AMD64 and Linux/AMD64.

### Installation

```lisp
CL-USER> (ql:quickload "cl-gregorian-date")
```

### Example

```lisp

CL-USER> (defvar *date1*
           (make-instance 'cl-gregorian-date:date :year 2012 :month 1 :day 6))
=> *DATE1*

CL-USER> (defvar *date2* (cl-gregorian-date:encode-date 2012 1 6))
=> *DATE2*

CL-USER> (defvar *date3* (cl-gregorian-date:parse-date "2012-01-06"))
=> *DATE3*

CL-USER> (cl-gregorian-date:datep *date1*)
=> T

CL-USER> (cl-gregorian-date:date= *date1* *date2*)
=> T

CL-USER> (cl-gregorian-date:date= *date1* *date3*)
=> T

CL-USER> (cl-gregorian-date:decode-date *date1*)
=> 2012, 1, 6

CL-USER> (cl-gregorian-date:date/= *date1* *date2*)
=> NIL

CL-USER> (cl-gregorian-date:date< *date1* *date2*)
=> NIL

CL-USER> (cl-gregorian-date:date> *date1* *date2*)
=> NIL

CL-USER> (cl-gregorian-date:date<= *date1* *date2*)
=> T

CL-USER> (cl-gregorian-date:date>= *date1* *date2*)
=> T

CL-USER> (prin1-to-string *date1*)
=> "#<CL-GREGORIAN-DATE/CORE/DATE:DATE 2012-01-06 {10048EAFE3}>"

CL-USER> (princ-to-string *date1*)
=> "2012-01-06"

```

### License

cl-gregorian-date is distributed under the MIT license. See LICENSE.
