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

(defvar state-district-to-representative-alist
  (hash-table-alist state-district-to-representative))


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

(defun zip-hash (zip)
  (when-let (pair (gethash zip zip-to-state-district))
    (destructuring-bind (state district) pair
      (let ((hash (make-hash-table :test 'equalp))
            (state-district (concatenate 'string state district)))
        (setf (gethash "sen" hash)
              (gethash state state-to-senator))
        (setf (gethash "rep" hash)
              (gethash state-district state-district-to-representative))
        (setf (gethash "state" hash)
              (gethash state-district state-district-to-state-representative))
        hash))))


;;; Write out the results.
#+nil
(mapc (lambda (zip)
        (with-open-file (out (make-pathname :directory
                                            `(:relative "zip-data" ,(take 2 zip))
                                            :name zip
                                            :type "json")
                             :direction :output :if-exists :supersede)
          (encode-json (zip-hash zip) out)))
      (hash-table-keys zip-to-state-district))


;;; Deiners
(defvar deniers
  (cl-csv:read-csv
   (file-to-string "_submodules/non-git/climate-deniers-2019-final-rep-level-details.csv")))

(defun drop-periods (string)
  (regex-replace-all "\\." string ""))

(defvar denier-file-by-full-name (make-hash-table :test #'equalp)
  "Collect denier file names by their full names as listed in zip contact.
This is populated as a side effect of `match-denier' and is then used
by `zip-hash'.")

(defun denier-filename (denier)
  (regex-replace-all "[\\s]" (drop-periods (string-downcase (fifth denier))) "-"))

(defun reps-for-state (state)
  (mappend #'cdr (remove-if-not [{string= state} {subseq _ 0 2} #'first]
                                state-district-to-representative-alist)))

(defun match-denier (denier)
  (let ((state (string-upcase (second denier))))
    (find-if (lambda (contact) ; Every denier name fragment matches contact info.
               (every {search _ (string-downcase (drop-periods (car contact)))}
                      (nest (split-sequence #\Space)
                            (drop-periods)
                            (string-downcase)
                            (fifth denier))))
             (ecase (intern (string-upcase (third denier)))
               (representative (reps-for-state state))
               (senator (gethash state state-to-senator))))))

(defun denier-to-markdown (denier &optional (stream t))
  (format stream "---~%  layout: denier~%  tags: denier~%")
  (mapc (lambda (pair)
          (destructuring-bind (name content) pair
            (when content
              (format stream "~&  ~a: ~s~%" name content))))
        (let ((contact (match-denier denier)))
          ;; Populate `denier-file-by-full-name'.
          #+nil
          (when contact
            (setf (gethash (first contact) denier-file-by-full-name)
                  (denier-filename denier)))
          `(("title" ,(third denier))
            ("name" ,(fifth denier))
            ("state" ,(first denier))
            ("quote" ,(nth 7 denier))
            ("quote-ref" ,(nth 8 denier))
            ("quote-ref-url" ,(nth 9 denier))
            ("dirty-money" ,(nth 10 denier))
            ("phone" ,(second contact))
            ("email" ,(third contact))
            ("contact" ,(fourth contact)))))
  (format stream "~&---~%"))

#+nil
(mapcar
 (lambda (denier)
   (with-open-file (out (make-pathname :directory '(:relative "deniers")
                                       :name (denier-filename denier)
                                       :type "md")
                        :direction :output :if-exists :supersede)
     (denier-to-markdown denier out)))
 (cdr deniers))

(defun add-denier-filename (contact)
  (append contact (list (gethash (first contact) denier-file-by-full-name))))

#+nil
(progn ; Add denier filenames to contacts for senators and representatives.
  (maphash (lambda (key value)
             (setf (gethash key state-to-senator)
                   (mapcar #'add-denier-filename value)))
           state-to-senator)
  (maphash (lambda (key value)
             (setf (gethash key state-district-to-representative)
                   (mapcar #'add-denier-filename value)))
           state-district-to-representative))
