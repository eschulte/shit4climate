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


;;; National representatives
(defun senatorp (legislator)
  (string= "sen" (key :type (lastcar (key :terms legislator)))))

(defun representativep (legislator)
  (string= "rep" (key :type (lastcar (key :terms legislator)))))

(define-constant senators
    (remove-if-not #'senatorp legislators))

(define-constant representatives
    (remove-if-not #'representativep legislators))

(define-constant zip-districts
    (cl-csv:read-csv (file-to-string "_submodules/zipcodes/zccd.csv"))
  :test #'equalp)

(define-constant zips-by-state
    (reduce (lambda (acc row)
              (destructuring-bind (state-fips state-abbr zcta cd) row
                (declare (ignorable state-fips cd))
                (pushnew zcta (aget state-abbr acc :test #'string=)))
              acc)
            (cdr zip-districts) :initial-value '())
  :test #'equalp)

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

(defun zips (legislator)
  (cond
    ((senatorp legislator)
     (aget (state legislator) zips-by-state :test #'string=))
    ((representativep legislator)
     (mapcar #'third
             (remove-if-not (lambda (row)
                              (and (string= (state legislator) (second row))
                                   (string= (format nil "~d" (district legislator)) (fourth row))))
                            zip-districts)))))

(defun by-zip (senators-or-representatives &aux failures)
  (values (remove-duplicates
           (mappend (lambda (rep)
                      (if-let ((zips (zips rep))
                               (phone (phone rep))
                               (name (name rep)))
                        (mapcar {list _ phone name} zips)
                        (prog1 nil
                          (warn "Missing information for ~s" (list zips phone name))
                          (push (list zips phone name) failures))))
                    senators-or-representatives)
           :test #'equalp)
          (reverse failures)))


;;; State representatives
(defun state-district (legislator)
  (key :district (first (key :roles legislator))))

(defun state-phones (legislator)
  (mapcar {key :voice} (key :contact_details legislator)))

(defun state-name (legislator)
  (key :name legislator))

(defvar state-representatives
  ;; Each element is '(state name district (list phone))
  (mappend
   (op (let ((state (lastcar (pathname-directory _1))))
         (mapcar [«list (constantly state) #'state-name #'state-district #'state-phones»
                  #'cl-yaml:parse #'file-to-string]
                 (directory-files (merge-pathnames-as-directory _1 "legislature/")))))
   (directory (directory-wildcard "_submodules/openstates/people/data/"))))

(defvar state-by-zip
  ;; Each element is '(zip (list (state name district (list phone))))
  (mapcar (lambda (zip)
            (cons (third zip)
                  (remove-if-not (op (and (string= (string-downcase (second zip)) (first _1))
                                          (string= (string-left-trim "0" (first zip)) (third _1))))
                                 state-zips)))
          (cdr zip-districts)))


;;; Write out the results.

(let ((by-zip (by-zip senators)))
  (with-open-file (out "_data/senator_zips.json" :direction :output :if-exists :supersede)
    (format out "[~:{[~s,[~s,~s]],~%~}" (butlast by-zip))
    (format out "~:{[~s,[~s,~s]]~}]~%" (last by-zip))))

(let ((by-zip (by-zip representatives)))
  (with-open-file (out "_data/representative_zips.json" :direction :output :if-exists :supersede)
    (format out "{~:{~s:[~s,~s],~%~}~%" (butlast by-zip))
    (format out "~:{~s:[~s,~s]~}}~%" (last by-zip))))

(let ((by-zip
       (mappend
        (lambda (row)
          (remove nil
                  (mapcar (op (when (first (fourth _1))
                                (list (car row) (first (fourth _1)) (second _1))))
                          (cdr row))))
        state-by-zip)))
  (with-open-file (out "_data/state_rep_zips.json" :direction :output :if-exists :supersede)
    (format out "[~:{[~s,[~s,~s]],~%~}" (butlast by-zip))
    (format out "~:{[~s,[~s,~s]]~}]~%" (last by-zip))))
