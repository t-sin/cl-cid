(defpackage #:cl-cid
  (:nicknames #:cid)
  (:use #:cl)
  (:import-from #:split-sequence
                #:split-sequence)
  (:export #:*cmap*
           #:*cmap-root-pathname*
           #:cmap-table
           #:cmap-header
           #:list-cmap
           #:load-cmap
           #:code-cid
           #:cid-code))
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

(defun to-keyword (s)
  (intern s :keyword))

(defun parse-hex (s)
  (flet ((hex-digit-p (c) (digit-char-p c 16)))
    (parse-integer (remove-if-not #'hex-digit-p s) :radix 16)))

(defun parse-field (s)
  (cond ((string= s "*") nil)
        ((find #\, s :test #'char=)
         (map 'vector #'parse-hex (split-sequence #\, s)))
        (t (parse-hex s))))

(defun load-cmap (name)
  (let ((cmap (make-cmap :table (make-array 10000 :adjustable t :fill-pointer 0)))
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
              (if (= linum 0)
                  (setf (cmap-header cmap) (map 'vector #'to-keyword fields))
                  (vector-push-extend (map 'vector #'parse-field fields) (cmap-table cmap)))
              (incf linum))))
    (setf *cmap* cmap)
    (length (cmap-table *cmap*))))

(defun code-cid (code &optional (enc :|UniJISX0213-UTF32|))
  (let* ((column (position enc (cmap-header *cmap*)))
         (row (find code (cmap-table *cmap*)
                    :test #'equal
                    :key (lambda (r) (aref r column)))))
    (when row (aref row 0))))

(defun cid-code (cid &optional (enc :|UniJISX0213-UTF32|))
  (let ((column (position enc (cmap-header *cmap*)))
        (row (find cid (cmap-table *cmap*)
                   :test #'=
                   :key (lambda (r) (aref r 0)))))
    (when row (aref row column))))
