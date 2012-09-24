;;;; package.lisp

(defpackage #:com.clearly-useful.reducers
  (:use #:cl
	#:com.clearly-useful.generic-collection-interface)
  (:export
   
   #:r/map
   #:r/mapcat
   #:r/filter
   #:r/remove
   #:r/flatten
   #:r/flatten-if
   #:r/take-while
   #:r/take
   #:r/drop))

