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
    (format nil "~A" (code-char (if (eq int (char-code #\no-break_space)) 32 int)))))

(defrule utfstr (+ utfseq)
  (:lambda (lst)
     (cl-ppcre:split "\\s+"  (text lst))))

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

(defrule atom (or string integer symbol utfstr  #\\ #\* #\; #\? #\' #\.
                  #\( #\) #\: #\, #\! #\- #\[ #\] #\/
                  #\> #\< #\|))

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

(defun downcase-words (wl)
  (loop for w in wl collect (if (typep w 'string)
                                (string-downcase w)
                                w)))

(defun words-in-file (file)
  (format t "~&reading file ~A~%" file)
  (remove-if (lambda (x) (or (symbolp x)
                             (equal x  "\\")))
             (alexandria::flatten (parse 'sexp (read-doc file)))))

(defparameter *sorted* nil)

(defun main ()
  (let ((all-files (cl-fad:list-directory #p "/tmp/rus/"))
        (hash (make-hash-table :test #'equalp)))
    (loop for fn from 0 below (length all-files)
          do (loop for wr in
                          (downcase-words
                           (words-in-file (elt all-files
                                               fn)))
                   do (incf (gethash wr hash 0))))
    (setf *sorted* (sort
                  (loop for k being each hash-key of hash collect (list k (gethash k hash 0 )))
                  #'> :key #'cadr ))
    ;; show me first 500 words
    (subseq *sorted* 0 500)
    ;; number of word and their forms recorded
    ;; (length sorted)
    ))

(defun ascii-chars ()
  (loop for x from 60 to 126 collect (code-char x)))

(defun contains-ascii-p (word)
  (loop for c across word
        until (member c (ascii-chars))
        finally (return (member c (ascii-chars)))))

(defun print-sorted ()
  (loop for w in *sorted* do
        (format t "~A - ~A ~%" (car w) (cadr w))))
