(defpackage :update
  (:use :gt/full :cl-csv :cl-yaml :cl-json)
  (:shadowing-import-from :fset :filter))
(in-package :update)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (yaml:register-mapping-converter nil #'hash-table-alist))

(define-constant legislators
    (cl-yaml:parse (file-to-string "_submodules/legislators/legislators-current.yaml"))
  :test #'equalp)


;;; Zip codes
(define-constant zip-districts
    (cdr (cl-csv:read-csv (file-to-string "_submodules/zipcodes/zccd.csv")))
  :test #'equalp)

(defvar zip-to-state-district
  (let ((hash (make-hash-table :test 'equalp)))
    (mapc (lambda (row)
            (destructuring-bind (state-fips state-abbr zcta cd) row
              (declare (ignorable state-fips))
              (setf (gethash zcta hash) (list state-abbr cd))))
          zip-districts)
    hash))


;;; National representatives
(defun key (item alist)
  (aget (string-downcase (symbol-name item)) alist :test #'string=))

(defun phone (legislator)
  (key :phone (lastcar (key :terms legislator))))

(defun district (legislator)
  (find-if #'numberp (mapcar {key :district} (key :terms legislator)) :from-end t))

(defun state (legislator)
  (find-if #'stringp (mapcar {key :state} (key :terms legislator)) :from-end t))

(defun name (legislator)
  (key :official_full (key :name legislator)))

(defun senatorp (legislator)
  (string= "sen" (key :type (lastcar (key :terms legislator)))))

(defun representativep (legislator)
  (string= "rep" (key :type (lastcar (key :terms legislator)))))

(define-constant senators
    (remove-if-not #'senatorp legislators)
  :test #'equalp)

(define-constant representatives
    (remove-if-not #'representativep legislators)
  :test #'equalp)

(defvar state-to-senator
  (let ((hash (make-hash-table :test 'equalp)))
    (mapc (lambda (senator)
            (if (gethash (state senator) hash)
                (push (list (phone senator) (name senator))
                      (gethash (state senator) hash))
                (setf (gethash (state senator) hash)
                      (list (list (phone senator) (name senator))))))
          senators)
    hash))

(defvar state-district-to-representative
  (let ((hash (make-hash-table :test 'equalp)))
    (mapc (lambda (rep)
            (let ((state-district (format nil "~a~a" (state rep) (district rep))))
              (if (gethash state-district hash)
                  (push (list (phone rep) (name rep))
                        (gethash state-district hash))
                  (setf (gethash state-district hash)
                        (list (list (phone rep) (name rep)))))))
          representatives)
    hash))


;;; State representatives
(defun state-district (legislator)
  (key :district (first (key :roles legislator))))

(defun state-phone (legislator)
  (first (mapcar {key :voice} (key :contact_details legislator))))

(defun state-name (legislator)
  (key :name legislator))

(defvar state-district-to-state-representative
  ;; Each element is '(state name district (list phone))
  (flet ((state-and-district (state rep)
           (format nil "~a~a" state (state-district rep))))
    (let ((hash (make-hash-table :test 'equalp)))
      (mapc
       (lambda (state-directory)
         (let ((state (string-upcase (lastcar (pathname-directory state-directory)))))
           (mapc
            (op (let* ((rep (parse (file-to-string _1)))
                       (sd (state-and-district state rep)))
                  (if (gethash sd hash)
                      (push (list (state-phone rep) (state-name rep))
                            (gethash sd hash))
                      (setf (gethash sd hash)
                            (list (list (state-phone rep) (state-name rep)))))))
            (directory-files (merge-pathnames-as-directory state-directory "legislature/")))))
       (directory (directory-wildcard "_submodules/openstates/people/data/")))
      hash)))


;;; Write out the results.
(with-open-file (out "_data/zipStateDistrict.json"
                     :direction :output :if-exists :supersede)
  (encode-json zip-to-state-district out))

(with-open-file (out "_data/stateSenator.json"
                     :direction :output :if-exists :supersede)
  (encode-json state-to-senator out))

(with-open-file (out "_data/stateDistrictRepresentative.json"
                     :direction :output :if-exists :supersede)
  (encode-json state-district-to-representative out))

(with-open-file (out "_data/stateDistrictstateRepresentative.json"
                     :direction :output :if-exists :supersede)
  (encode-json state-district-to-state-representative out))
