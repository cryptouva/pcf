#!/usr/local/bin/sbcl --script
(require :asdf)

(declaim (sb-ext:muffle-conditions style-warning)
         (sb-ext:muffle-conditions sb-ext:compiler-note))
(setf asdf:*compile-file-warnings-behaviour* :ignore)

(asdf:operate 'asdf:compile-op :lccyao2 :verbose nil :print nil)
(asdf:operate 'asdf:load-op :lccyao2 :verbose nil :print nil)

(handler-bind
    ((file-error #'(lambda (c)
                     (format *error-output* "~&File error for ~A~%" (file-error-pathname c)) (quit)))
     (end-of-file #'(lambda (c)
                      (format *error-output* "~&Unexpected end of file: ~A~%" c) (quit)))
     (t #'(lambda (c)
            (format *error-output* "~&General error: ~A~%" c) (quit)))
     )
  (assert (= 3 (length sb-ext:*posix-argv*)))
  (lccyao-main:save-pcf-ops (third sb-ext:*posix-argv*)
                (lccyao-main:pcf-compile (second sb-ext:*posix-argv*))
                )
  )
