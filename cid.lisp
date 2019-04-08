(defpackage #:cl-cid
  (:nicknames #:cid)
  (:use #:cl)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:*cmap*
           #:*cmap-root-pathname*
           #:list-cmap
           #:load-cmap
           #:code-cid))
(in-package #:cl-cid)

(defstruct cmap
  header table)

(defparameter *cmap* nil)
(defparameter *cmap-root-pathname*
  (make-pathname :directory "/usr/local/share/adobe/resources/mapping/"))

(defun list-cmap ()
  (mapcar (lambda (p) (car (last (pathname-directory p))))
          (directory (merge-pathnames (make-pathname :name :wild :type nil)
                                      *cmap-root-pathname*))))

(defun load-cmap (name)
  (let ((cmap (make-cmap))
        (map-pathname (merge-pathnames (make-pathname :name "cid2code"
                                                      :type "txt"
                                                      :directory `(:relative ,name))
                                       *cmap-root-pathname*)))
    (with-open-file (in map-pathname :direction :input)
      (loop
        :for line := (read-line in nil :eof)
        :with linum := 0
        :until (eq line :eof)
        :unless (char= (char line 0) #\#)
        :do (let ((fields (apply #'vector (split-sequence #\tab line :test #'char=))))
              (print fields)
              (if (= linum 0)
                  (setf (cmap-header cmap)
                        (map 'vector (lambda (name) (intern name :keyword)) fields))
                  (push (map 'vector (lambda (v)
                                       (if (string= v "*")
                                           nil
                                           (parse-integer v :radix 16)))
                             fields)
                        (cmap-table cmap)))
              (incf linum))
        :finally (setf (cmap-table cmap)
                       (coerce (cmap-table cmap) 'vector))))
    cmap))

(defun code-cid (code))
