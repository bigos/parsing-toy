;;;; rtf-reader.lisp

;; https://scymtym.github.io/esrap/

(in-package #:rtf-reader)

(defparameter rtf-file #P"~/rj.rtf")

(defun read-doc (file)
  (with-open-file (s file)
    (car
     (loop for l = (read-line s nil 'eof)
             then (read-line s nil 'eof)
           until (eq l 'eof)
           collect l))))

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(defrule opcb "{")
(defrule clcb "}")

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defrule utfseq (and "\\" "u" integer "?")
  (:destructure (bs uc int qm)
    (declare (ignore bs uc qm))
    (format nil "~A" (code-char int))))

(defrule utfstr (+ utfseq)
  (:lambda (lst)
    (text lst)))

(defrule utfic (and "\\" "u" integer "?")
  (:destructure (bs u nn qm)
    (declare (ignore bs u qm))
    (list :utf (format nil "~x"  nn))))

(defrule alphanumeric (alphanumericp character))

(defrule sexp (and (? whitespace) (or list atom ))
  (:destructure (w s )
    (declare (ignore w))
    s))

(defrule list (and opcb sexp (* sexp) (? whitespace) clcb)
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (cons car cdr)))

(defrule atom (or string integer symbol utfstr  #\\ #\* #\; #\? #\' #\. #\( #\) #\: #\,))

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

(defun words-in-file (file)
  (remove-if (lambda (x) (or (symbolp x)
                             (equal x  "\\")))
             (alexandria::flatten (parse 'sexp (read-doc file)))))

(defun main ()
  (words-in-file (car (cl-fad:list-directory #p "/tmp/rus/"))))
