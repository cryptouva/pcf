(defpackage :string-tokenizer 
  (:use :common-lisp)
  (:export tokenize)
  )
(in-package :string-tokenizer)

(declaim (ftype (function (string &optional character) (cons string *)) tokenize))
(defun tokenize (str &optional (delimiter #\ ))
  "Break the string into a list of strings, using the specified
  delimiter character.  For the first token, only alpha characters
  will be used, i.e. the string \"CALLI4 X\" will be tokenized
  as (\"CALLI\" \"4\" \"X\")"
  (declare (string str))
  (let ((first t))
    (if (= (length str) 1)
        (list str)
        (loop for i from 0 to (- (length str) 1) unless (equalp (char str i) delimiter) nconc
             (the (cons string *) 
               (let ((start i)
                     (end i))
                 (loop until (or (equalp end (length str))
                                 (equalp (char str end) delimiter)) do (incf end) (incf i))
                 (if first
                     (progn
                       (setf first nil)
                       (if (and (> end 1) (alpha-char-p (char str (- end 2))) (digit-char-p (char str (1- end))))
                           (list (subseq str start (1- end)) (subseq str (1- end) end))
                           (list (subseq str start end))
                           )
                       )
                     (list (subseq str start end))
                     )
                 )
               )
             )
           )
      )
  )

(defun tokenize* (str  &optional (delimiter #\ ))
  "Break the string into a list of strings, using the specified
delimiter.  No distinction is made between alpha and non-alpha
characters other than the delimiter."
  (if (= (length str) 1)
      (list str)
      (loop for i from 0 to (1- (length str)) unless (equalp (char str i) delimiter) nconc
           (let ((start i)
                 (end i))
             (loop until (or (equalp end (length str))
                             (equalp (char str end) delimiter)) do (incf end) (incf i))
             (list (subseq str start end))
             )
           )
      )
  )