;;;; com.clearly-useful.reducers.lisp

(in-package #:com.clearly-useful.reducers)


;;; adapted from cl-reducers. adapted for optional docstring
;;; and discriminating nil from no argument
;;; would be good to add compiler macros too.
(defmacro defcurried (name args doc &optional body)
  (flet ((do-curried (name args doc body)
	   (let ((cargs (butlast args))
		 (larg (last args))
		 (x (gensym))
		 (given (gensym)))
	     `(defun ,name  (,@cargs &optional (,@larg nil ,given)) 
		,doc
		(if ,given
		    ,body
		    (lambda (,x) (,name ,@cargs ,x)))))))
    (if (stringp doc)
	(do-curried name args doc body)
	(do-curried name args nil (if body
				      (cons doc body)
				      doc)))))


;;;;;;;;;;;;;; f*

(defcurried r/map (f collection)
  (folder collection (mapping f)))

(defcurried r/filter (pred collection)
  (folder collection (filtering pred)))

(defcurried r/mapcat (f collection)
  (folder collection (mapcatting f)))

(defcurried r/remove (pred collection)
  (r/filter (complement pred) collection))

(defun sequential? (v)
  (macrolet ((of-type ((&rest types) v)
	       `(or ,@(loop for type in types
			   collect `(typep ,v ',type)))))
    (of-type (cons vector array reduceable indexable seq seqable) v)))

(defcurried r/flatten (collection)
  (folder collection
	  (lambda (f1)
	    (lambda (&optional (ret nil given) v)
	      (if (not given)
		  (funcall f1)
		  (if (sequential? v)
		      (fold-left f1 (r/flatten v) :initial-value ret)
		      (funcall f1 ret v)))))))

(defcurried r/flatten-if (pred collection)
  (folder collection
	  (lambda (f1)
	    (lambda (&optional (ret nil given) v)
	      (if (not given)
		  (funcall f1)
		  (if (and (sequential? v) (funcall pred v))
		      (fold-left f1 (r/flatten v) :initial-value ret)
		      (funcall f1 ret v)))))))
;;;;



(defcurried r/take-while (pred coll)
  (reducer coll
	   (lambda (f1)
	     (lambda (ret val)
	       (if (funcall pred val)
		   (funcall f1 ret val)
		   (reduced ret))))))


(defcurried r/take (n coll)
  (reducer coll
	   (lambda (f1)
	     (let ((i n))
	       (lambda (ret val)
		 (decf i)
		 (if (< -1 i)
		     (funcall f1 ret val)
		     (reduced ret)))))))

(defcurried r/drop (n coll)
  (reducer coll
	   (lambda (f1)
	     (let ((i n))
	       (lambda (ret val)
		 (decf i)
		 (if (< -1 i)
		     ret
		     (funcall f1 ret val)))))))

