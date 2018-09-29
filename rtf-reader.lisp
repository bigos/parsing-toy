;;;; rtf-reader.lisp

;; https://scymtym.github.io/esrap/

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
;;; ==============================================================
(defrule letter-sequence (+ (character-ranges (#\a #\z))))
(defrule control-word (and #\\ letter-sequence delimiter))

;;; ==============================================================

(defrule anything (+ (not #\Backspace))) ; we do not expect backspace, but what is the proper way of doing anything?

(defrule alphabetic (+ (or (+ #\Space) (character-ranges (#\A #\z)))) (:text T))

(defrule oper (or #\+ #\-))

;;; here we ignore optspace and produce only the operator
(defrule operator (and (* #\Space) oper (* #\Space)) (:lambda (l) (elt l 1)))

(defrule curly (and #\{ decimal operator decimal #\}))

(parse 'curly "{123   -  4}")
