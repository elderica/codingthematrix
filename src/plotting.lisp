;;;; This is reimplementation of `plotting.py` from "Coding the Matrix"
;;;; and contains a simple plotting interface, which uses a browser with SVG to
;;;; present a plot of points represented as either complex numbers or 2-vectors.
;;;; 
;;;; First you need evaluate following expressions then load this file.
;;;; ```
;;;; (require "asdf")
;;;; (asdf:load-system "uiop")
;;;; ```
(in-package :cl-user)
(defpackage codingthematrix/plotting
  (:use :cl)
  (:export :plot))
(in-package :codingthematrix/plotting)

(defparameter *header*
  (list "<!doctype html>"
        "<head>"
        "<title>plot</title>"
        "</head>"
        "<body>"
        "<svg height=\"420\" width=420 xmlns=\"http://www.w3.org/2000/svg\">"
        "<line x1=\"0\" y1=\"210\" x2=\"420\" y2=\"210\""
        "style=\"stroke:rgb(0,0,0);stroke-width:2\"/>"
        "<line x1=\"210\" y1=\"0\" x2=\"210\" y2=\"420\""
        "style=\"stroke:rgb(0,0,0);stroke-width:2\"/>"))

(defparameter *footer*
  (list "</svg>" "</body>" "</html>"))

(defparameter *browser* "xdg-open")

(defun coerce-cons (p)
  (ctypecase p
    (complex (cons (realpart p) (imagpart p)))
    (vector (cons (aref p 0) (aref p 1)))
    (cons (cons (first p) (second p)))))


;; plot takes a list of points, optionally a scale (relative to a 200x200 frame),
;; optionally a dot size (diameter) in pixels, and optionally a browser name.
;; It produces an html file with SVG representing the given plot,
;; and opens the file in a web browser. It returns nothing.
(defun plot (points &optional (scale 4) (dot-size 3))
  (let ((scalar (/ 200.0 scale))
        (origin-x 210)
        (origin-y 210)
        (cpoints (loop for p in points collect (coerce-cons p))))
    (uiop:with-temporary-file
        (:stream out
         :pathname path
         :type "html"
         :keep t)
      (dolist (line *header*)
        (write-line line out))
      (loop for pt in cpoints
            do (format out 
                       "<circle cx=\"~a\" cy=\"~a\" r=\"\~a\" fill=\"red\"/>~%"
                       (+ origin-x (* scalar (car pt)))
                       (- origin-y (* scalar (cdr pt)))
                       dot-size))
      (dolist (line *footer*)
        (write-line line out))
      (uiop:finish-outputs out)
      (uiop:launch-program (list *browser* (uiop:unix-namestring path)))
      path)))


(defparameter *sample-vector-1*
  (list 
   #C(2 2)
   #C(3 2)
   #C(1.75 1)
   #C(2 1)
   #C(2.25 1)
   #C(2.5 1)
   #C(2.75 1)
   #(3 1)
   #C(3.25 1)))
