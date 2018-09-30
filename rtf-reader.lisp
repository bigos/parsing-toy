;;;; rtf-reader.lisp

;; https://scymtym.github.io/esrap/

(in-package #:rtf-reader)

(defparameter rtf-file #P"~/rj.rtf")

(defun read-doc ()
  (with-open-file (s rtf-file)
    (car
     (loop for l = (read-line s nil 'eof)
             then (read-line s nil 'eof)
           until (eq l 'eof)
           collect l))))

(defrule opcb "{")
(defrule clcb "}")

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defrule alphanumeric (alphanumericp character))

(defrule sexp (and (? whitespace) (or list atom))
  (:destructure (w s )
    (declare (ignore w))
    s))

(defrule list (and opcb sexp (* sexp) (? whitespace) clcb)
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (cons car cdr)))

(defrule atom (or string integer symbol))

(defrule string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule symbol (not-integer (+ alphanumeric))
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before
  ;; a STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (:lambda (list)
    (intern (text list))))
