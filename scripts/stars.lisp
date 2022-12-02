(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:cl-ppcre :unix-opts) :silent t))

(defpackage :stars
  (:use :cl)
  (:export :toplevel))

(in-package :stars)

;;;; Configuration
(defparameter *initial* 0)

;;;; Errors
(define-condition user-error (error) ())

;;;; Functionnality
(defun copy-file (from-file to-file)
  (with-open-file (input-stream from-file
                :direction :input
                :element-type '(unsigned-byte 8))
    (with-open-file (output-stream to-file
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
      (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
    (loop for pos = (read-sequence buf input-stream)
       while (plusp pos)
       do (write-sequence buf output-stream :end pos))))))

(defun add-star (file)
  (let ((backup-file (concatenate 'string file ".bak")))
    (copy-file file backup-file)
    (with-open-file (in-stream backup-file)
      (with-open-file (out-stream file :direction :output :if-exists :supersede)
        (loop for line = (read-line in-stream nil)
           while line do
             (let ((registers (nth-value 1 (cl-ppcre::scan-to-strings "message=(\\d)+" line))))
               (if registers
                (write-line
                 (cl-ppcre:regex-replace "message=(\\d)+" line (concatenate 'string "message=" (write-to-string (+ (parse-integer (aref registers 0)) 1)))) out-stream)
                (write-line line  out-stream))))))
    (delete-file backup-file)))

;;;; Run
(defun run (file)
  (add-star file))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (let ((options (opts:get-opts)))
        (run (getf options :file))))

(opts:define-opts
    (:name :help
           :description "print this help text"
           :short #\h
           :long "help")
    (:name :file
           :description "path to file"
           :short #\f
           :long "file"
           :arg-parser #'identity))
