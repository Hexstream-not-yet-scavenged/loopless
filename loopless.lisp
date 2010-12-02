(in-package #:cl-user)

(defpackage #:loopless
  (:nicknames #:ll)
  (:use #:cl)
  #-loopless-noconflict
  (:export #:with-collectors
	   #:collect
	   #:ncollect
	   #:collecting
	   #:compose)
  (:export #:mapal
	   #:mapalist
	   #:mapacon
	   #:mapal*
	   #:mapalist*
	   #:mapacon*
	   #:mappl
	   #:mapplist
	   #:mappcon
	   #:mapv
	   #:mapvector
	   #:mapvcon
	   #:mapt
	   #:maptimes
	   #:maptcon
	   #:mapt*
	   #:maptimes*
	   #:maptcon*

	   #:for*
	   #:step*
	   
	   #:doalist
	   #:doalist*
	   #:doplist
	   #:docons
	   #:dovector
	   #:dotimes*
	   #:while*))

(in-package #:loopless)

;;;; So, I'm sure some will note the "irony" of using LOOP in the
;;;; implementation of a library called Loopless which sole reason for
;;;; existing is using LOOP less, so I thought I'd address this: the
;;;; reason I'm using LOOP at some places here is that it makes the
;;;; implementation that much simpler and I'm also guessing that much
;;;; work has been invested in optimizing LOOP to death for common
;;;; cases so it makes sense to leverage that.

;;; COMPOSE, COLLECTING and WITH-COLLECTORS implementations
;;; originally from cl-utilities (with enhancements).

(defmacro with-collectors ((&rest collectors) &body body)
  (multiple-value-bind (names collectors ncollectors tails)
      (let (names the-collectors ncollectors tails)
	(dolist (collector collectors)
	  (destructuring-bind (collector ncollector)
	      (etypecase collector
		((and symbol (not null))
		   (list collector
			 (if (symbol-package collector)
			     (intern (format nil "N~A" collector)))))
		((cons symbol (cons symbol null)) collector))
	    (let ((name (or collector
			    ncollector
			    (error "collector and ncollector can't both be nil!"))))
	      (push name names)
	      (push collector the-collectors)
	      (push ncollector ncollectors)
	      (push (gensym (format nil "~A-TAIL" name)) tails))))
	(values (nreverse names)
		(nreverse the-collectors)
		(nreverse ncollectors)
		(nreverse tails)))
    `(let ,(mapcan #'list names tails)
       (labels ,(mapcan
		 (lambda (name collector ncollector tail)
		   (nconc (if collector
			      (list `(,collector
				      (thing)
				      (if ,name
					  (setf (cdr ,tail)
						(setf ,tail (list thing)))
					  (setf ,name
						(setf ,tail (list thing))))
				      thing)))
			  (if ncollector
			      (list `(,ncollector
				      (list)
				      (if ,name
					  (setf (cdr ,tail) list)
					  (setf ,name list))
				      (if list
					  (setf ,tail (last list)))
				      nil)))))
		 names collectors ncollectors tails)
	 (declare (ignorable
		   ,@(mapcan (lambda (collector ncollector)
			       (nconc (if collector
					  (list `#',collector))
				      (if ncollector
					  (list `#',ncollector))))
			     collectors ncollectors)))
	 ,@body)
       (values ,@names))))

;; These should only be called inside of COLLECTING macros, but we
;; define them here to provide informative error messages and to make
;; it easier for SLIME (et al.) to get documentation for the COLLECT
;; and NCOLLECT functions when they're used in the COLLECTING macro.
(defun collect (thing)
  (error "Can't collect ~S outside the context of the COLLECTING macro"
	 thing))

(defun ncollect (list)
  (error "Can't ncollect ~S outside the context of the COLLECTING macro"
	 list))

(defmacro collecting (&body body)
  `(with-collectors (collect)
     ,@body))

(defun compose (&rest functions)
  (cond ((not functions) (lambda (first-arg &rest ignored-rest)
			   (declare (ignore ignored-rest))
			   first-arg))
	((not (cdr functions)) (car functions))
	(t (let ((last-function (car functions))
		 (first-function (car (last functions)))
		 (butlast-position (1- (length functions))))
	     (lambda (&rest args)
	       (funcall last-function
			(reduce #'funcall functions
				:from-end t
				:initial-value (apply first-function args)
				:start 1
				:end butlast-position)))))))

#+nil
(define-compiler-macro compose (&rest functions)
  (labels ((sharp-quoted-p (x)
	     (and (listp x)
		  (eql (first x) 'function)
		  (symbolp (second x)))))
    `(lambda (x) ,(reduce #'(lambda (fun arg)
			      (if (sharp-quoted-p fun)
				  (list (second fun) arg)
				  (list 'funcall fun arg)))
			  functions
			  :initial-value 'x
			  :from-end t))))

;; Laughably bad-style unidiomatic implementation.
;; But hey, it works!
(defmacro define-mapcar-like-family ((nil-acc list-acc nconc-acc)
				     var
				     contributed-args-per-element
				     end-test-form
				     fill-args-form
				     step-vars-form)
  `(progn
     ,@(mapcar
	(let ((more-vars (intern (format nil "MORE-~AS" var)))
	      (vars (intern (format nil "~AS" var)))
	      (loop (intern "LOOP")))
	  (lambda (name accumulation-type)
	    `(defun ,name (function ,var &rest ,more-vars)
	       (let* ((,vars (cons ,var ,more-vars))
		      (args (make-list
			     ,(if (= contributed-args-per-element 1)
				  `(length ,vars)
				  `(* (length ,vars)
				      ,contributed-args-per-element)))))
		 ,(let ((core `(prog ()
				  ,loop
				  (if ,end-test-form
				      (return-from ,name ,(if accumulation-type
							      'collect
							      var)))
				  ,fill-args-form
				  ,(let ((core `(apply function args)))
				     (if accumulation-type
					 (list (ecase accumulation-type
						 (list 'collect)
						 (nconc 'ncollect))
					       core)
					 core))
				  ,step-vars-form
				  (go ,loop))))
		    (if accumulation-type
			`(collecting ,core)
			core))))))
	(list nil-acc list-acc nconc-acc)
	'(nil list nconc))))

(define-mapcar-like-family (mapal mapalist mapacon) alist
  2
  (notevery #'identity alists)
  (let ((args args))
    (dolist (alist alists)
      (let ((entry (car alist)))
	(setf (car args) (car entry)
	      (second args) (cdr entry)
	      args (cddr args)))))
  (map-into alists #'cdr alists))
(define-mapcar-like-family (mapal* mapalist* mapacon*) alist
  2
  (notevery #'identity alists)
  (let ((args args))
    (dolist (alist alists)
      (let ((entry (car alist)))
	(setf (car args) (car entry)
	      (second args) (second entry)
	      args (cddr args)))))
  (map-into alists #'cdr alists))
(define-mapcar-like-family (mappl mapplist mappcon) plist
  2
  (notevery #'identity plists)
  (let ((args args))
    (dolist (plist plists)
      (setf (car args) (first plist)
	    (second args) (second plist)
	    args (cddr args))))
  (map-into plists #'cddr plists))

(defun mapv (function vector &rest more-vectors)
  (apply #'map nil function vector more-vectors))

(defun mapvector (function vector &rest more-vectors)
  (apply #'map 'list function vector more-vectors))

(defun mapvcon (function vector &rest more-vectors)
  (collecting
    (let ((vectors (cons vector more-vectors)))
      (dotimes (i (apply #'max
			 (mapcar #'length vectors)))
	(ncollect (apply function (mapcar (lambda (vector)
					    (aref vector i))
					  vectors)))))))

(defun mapt (function count)
  (let ((function (coerce function 'function)))
    (dotimes (i count)
      (funcall function i))))

(defun maptimes (function count)
  (collecting
    (let ((function (coerce function 'function)))
      (dotimes (i count)
	(collect (funcall function i))))))

(defun maptcon (function count)
  (collecting
    (let ((function (coerce function 'function)))
      (dotimes (i count)
	(ncollect (funcall function i))))))

(defun mapt* (function count)
  (let ((function (coerce function 'function)))
    (dotimes (ignored count)
      (funcall function))))

(defun maptimes* (function count)
  (collecting
    (let ((function (coerce function 'function)))
      (dotimes (ignored count)
	(collect (funcall function))))))

(defun maptcon* (function count)
  (collecting
    (let ((function (coerce function 'function)))
      (dotimes (ignored count)
	(ncollect (funcall function))))))


(defmacro for* (bindings &body body)
  (let ((vars-inits-steps
	 (flet ((ck (symbol) ; Coerce to Keyword
		  (if symbol
		      (if (keywordp symbol)
			  symbol
			  (intern (symbol-name symbol) '#:keyword)))))
	   (mapcar
	    (lambda (binding)
	      (let ((var (car binding)))
		(cons var
		      (let ((stepping (cdr binding)))
			(flet ((from (function)
				 (list (second stepping)
				       `(,function ,var ,(if (eq (ck (third stepping)) :by)
							     (fourth stepping)
							     1)))))
			  (ecase (ck (car stepping))
			    ((:from :upfrom) (from '+))
			    (:downfrom (from '-))
			    (:= (list (second stepping)
				      (if (eq (ck (third stepping)) :then)
					  (fourth stepping)
					  (second stepping))))))))))
	    bindings))))
    (let ((vars (mapcar #'first vars-inits-steps))
	  (inits (mapcar #'second vars-inits-steps))
	  (steps (mapcar #'third vars-inits-steps)))
      (let ((first-time-p (gensym "FIRST-TIME-P")))
	`(let* ((,first-time-p t)
		,@vars)
	   (flet ((step* ()
		    (if ,first-time-p
			(setf ,first-time-p nil
			      ,@(mapcan #'list vars inits))
			(setf ,@(mapcan #'list vars steps)))
		    nil))
	     ,@body))))))


(defmacro doalist ((key value alist &optional result) &body body)
  (let ((assoc (gensym "ASSOC")))
    `(dolist (,assoc ,alist ,result)
      (let ((,key (car ,assoc))
	    (,value (cdr ,assoc)))
	(prog () ,@body)))))

(defmacro doalist* ((key value alist &optional result) &body body)
  (let ((assoc (gensym "ASSOC")))
    `(dolist (,assoc ,alist ,result)
       (let ((,key (first ,assoc))
	     (,value (second ,assoc)))
	 (prog () ,@body)))))

(defmacro doplist ((key value plist &optional result) &body body)
  `(block nil
     (mappl (lambda (,key ,value)
	      (prog () ,@body))
	    ,plist)
	  ,result))

(defmacro docons ((var list &optional result) &body body)
  `(block nil
     (mapl (lambda (,var)
	     (prog () ,@body))
	   ,list)
     ,result))

(defmacro dovector ((var vector &optional result) &body body)
  `(block nil
     (map 'nil (lambda (,var)
		 (prog () ,@body))
	  ,vector)
     ,result))

(defmacro dotimes* ((count &optional result) &body body)
  (let ((ignored-var (gensym "IGNORED-VAR")))
    `(dotimes (,ignored-var ,count ,@(if result (list result)))
       ,@body)))

(defmacro while* (test &body body)
  `(prog ()
    loop-start
      (if (not ,test)
	  (return))
      (prog () ,@body)
      (go loop-start)))
