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

;;; Methods on legislators
(defun key (item alist)
  (aget (string-downcase (symbol-name item)) alist :test #'string=))

(defun name (legislator)
  (let ((name (key :name legislator)))
    (etypecase name
      (string name)
      (list (key :official_full (key :name legislator))))))

(defun phone (legislator)
  (if-let ((terms (key :terms legislator)))
    (key :phone (lastcar terms))
    (first (mapcar {key :voice} (key :contact_details legislator)))))

(defun email (legislator)
  (key :email legislator))

(defun contact (legislator)
  (key :contact_form (lastcar (key :terms legislator))))

(defun state (legislator)
  (find-if #'stringp (mapcar {key :state} (key :terms legislator))
           :from-end t))

(defun district (legislator)
  (if-let ((terms (key :terms legislator)))
    (find-if #'numberp (mapcar {key :district} terms)
             :from-end t)
    (key :district (first (key :roles legislator)))))

(defun senatorp (legislator)
  (string= "sen" (key :type (lastcar (key :terms legislator)))))

(defun representativep (legislator)
  (string= "rep" (key :type (lastcar (key :terms legislator)))))

(defun representative-details (legislator)
  (list (name legislator)
        (phone legislator)
        (email legislator)
        (contact legislator)))


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
                (push (representative-details senator)
                      (gethash (state senator) hash))
                (setf (gethash (state senator) hash)
                      (list (representative-details senator)))))
          senators)
    hash))

(defvar state-district-to-representative
  (let ((hash (make-hash-table :test 'equalp)))
    (mapc (lambda (rep)
            (let ((state-district (format nil "~a~a" (state rep) (district rep))))
              (if (gethash state-district hash)
                  (push (representative-details rep)
                        (gethash state-district hash))
                  (setf (gethash state-district hash)
                        (list (representative-details rep))))))
          representatives)
    hash))


;;; State representatives
(defvar state-district-to-state-representative
  ;; Each element is '(state name district (list phone))
  (flet ((state-and-district (state rep)
           (format nil "~a~a" state (district rep))))
    (let ((hash (make-hash-table :test 'equalp)))
      (mapc
       (lambda (state-directory)
         (let ((state (string-upcase (lastcar (pathname-directory state-directory)))))
           (mapc
            (op (let* ((rep (parse (file-to-string _1)))
                       (sd (state-and-district state rep)))
                  (if (gethash sd hash)
                      (push (representative-details rep)
                            (gethash sd hash))
                      (setf (gethash sd hash)
                            (list (representative-details rep))))))
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
