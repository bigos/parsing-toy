;;;; rtf-reader.lisp
;; http://nikodemus.github.io/esrap/
;; https://scymtym.github.io/esrap/
;; https://quickref.common-lisp.net/esrap.html

(in-package #:rtf-reader)

(defparameter rtf-file #P"~/rj.rtf")

(defun read-file ()
  (with-open-file (s rtf-file)
    (car
     (loop for l = (read-line s nil 'eof)
             then (read-line s nil 'eof)
           until (eq l 'eof)
           collect l))))

;; Rules can transform their matches.
(add-rule 'decimal
          (make-instance 'rule
                         :expression '(+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                         :transform (lambda (list start end)
                                      (declare (ignore start end))
                                      (parse-integer (format nil "~{~A~}" list)))))

(defrule alphabetic (+ (or (+ #\Space) (character-ranges (#\A #\z)))) (:text T))

(defrule new-line #\Newline)

(defrule oper (or #\+ #\-))

(defrule optspace (* " ") (:constant nil))

;;; here we ignore optspace and produce only the operator
(defrule operator (and optspace oper optspace) (:lambda (l) (elt l 1)))

(defrule curly (and #\{ decimal operator decimal #\}))

(parse 'curly "{123   -  4}")
