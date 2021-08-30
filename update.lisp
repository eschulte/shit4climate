(defpackage :update
  (:use :gt/full :cl-csv :cl-yaml)
  (:shadowing-import-from :fset :filter))
(in-package :update)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (yaml:register-mapping-converter nil #'hash-table-alist))

(define-constant legislators
    (cl-yaml:parse (file-to-string "_submodules/legislators/legislators-current.yaml"))
  :test #'equalp)

(define-constant zip-districts
    (cl-csv:read-csv (file-to-string "_submodules/zipcodes/zccd.csv"))
  :test #'equalp)

(defun key (item alist)
  (aget (string-downcase (symbol-name item)) alist :test #'string=))

(defun phone (legislator)
  (key :phone (lastcar (key :terms legislator))))

(defun district (legislator)
  (find-if #'numberp (mapcar {key :district} (key :terms legislator)) :from-end t))

(defun state (legislator)
  (find-if #'stringp (mapcar {key :state} (key :terms legislator)) :from-end t))

(defun zip (legislator)
  (third
   (find-if (lambda (row)
              (and (string= (state legislator) (second row))
                   (string= (format nil "~d" (district legislator)) (fourth row))))
            zip-districts)))

(defvar by-zip
  (remove nil
          (mapcar (lambda (legislator)
                    (when-let ((zip (zip legislator))
                               (phone (phone legislator))
                               (name (key :official_full (key :name legislator))))
                      (list zip phone name)))
                  legislators)))

(with-open-file (out "_data/zips.json" :direction :output :if-exists :supersede)
          (format out "{~:{~s:[~s,~s],~%~}~%" (butlast by-zip))
          (format out "~:{~s:[~s,~s]~}}~%" (last by-zip)))
