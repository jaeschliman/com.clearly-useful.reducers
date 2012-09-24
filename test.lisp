(defpackage #:com.clearly-useful.reducers.test
  (:use #:cl
	#:com.clearly-useful.protocols
	#:com.clearly-useful.generic-collection-interface
	#:com.clearly-useful.reducers))

(in-package #:com.clearly-useful.reducers.test)

;;;;three examples showing the difference in performance
;;;;using cl map/reduce, reducer map/fold-left, and map/fold
;;;;on implementations where multithreading is enabled,
;;;;test-fold will parallelize.

;;obviously this one is slowest as map is constructing
;;an intermediate vector
(defun test-without (&optional (n 1))
  (let ((dat (make-array (floor 1e6) :initial-element 1)))
    (time (loop repeat n
	       do (reduce #'+ (map 'vector #'1+ dat))))))

;;same shape of code, but no intermediate vector
(defun test-with (&optional (n 1))
  (let ((dat (make-array (floor 1e6) :initial-element 1)))
    (time (loop repeat n 
	       do (fold-left #'+ (r/map #'1+ dat))))))

;;same shape of code, but run in parallel when possible
(defun test-fold (&optional (n 1))
  (let ((dat (make-array (floor 1e6) :initial-element 1)))
    (time (loop repeat n 
	     do (fold #'+ (r/map #'1+ dat))))))


;(test-without) ;obviously works
(test-with 1)
(test-fold 1)



;;;; trivial proof of concept for reducing over a file.
;;;; meant as an example

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct %file-lines path))

(extend-type %file-lines
  reduceable
  (coll-reduce (self fn seed)
	       (with-open-file (s (%file-lines-path self)
				  :direction :input)
		 (loop with r = seed
		      for val = (read-line s nil nil)
		      while val
		      do (setf r (funcall fn r val))
		      finally (return r)))))

(defun file-lines (path)
  (make-%file-lines :path path))

(defun println (s)
  (write-string s)
  (terpri))

(defun discard (&rest args)
  (declare (ignore args)))

(defun do-transform (fn coll)
  (fold-left #'discard (funcall fn coll)))

(defun test-readfile ()
  (let ((lines (file-lines #P"test.lisp"))
	(f (r/map #'println)))
    (do-transform f lines)))



