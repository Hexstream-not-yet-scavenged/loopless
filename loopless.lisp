(in-package #:cl-user)

(defpackage #:loopless
  (:nicknames #:ll)
  (:use #:cl)
  (:export #:destructuring-lambda
	   
	   #:mapal
	   #:mapalist
	   #:mapacon
	   #:mapal*
	   #:mapalist*
	   #:mapacon*
	   #:mappl
	   #:mapplist
	   #:mappcon

	   #:for*
	   #:step*
	   #:collecting ; Todo: avoid potential package conflicts
	   #:collect    ;       with cl-utilities and such.
	   #:ncollect
	   #:with-collectors
	   #:compose
	   
	   #:doalist
	   #:doalist*
	   #:doplist
	   #:dovector
	   #:dotimes*
	   #:while*))

(in-package #:loopless)

;;;; So, I'm sure some will note the "irony" of using LOOP in the
;;;; implementation of a library called LOOPless which sole reason for
;;;; existing is using LOOP less, so I thought I'd address this: the
;;;; reason I'm using LOOP at some places here is that it makes the
;;;; implementation that much simpler and I'm also guessing that much
;;;; work has been invested in optimizing LOOP to death for common
;;;; cases so it makes sense to leverage that.

;;; WITH-UNIQUE-NAMES, ONCE-ONLY, COMPOSE, COLLECTING and
;;; WITH-COLLECTORS implementations from cl-utilities
;;; (with minor adaptations).

;; Not exported but used below.
(defmacro with-unique-names ((&rest bindings) &body body)
  "Executes a series of forms with each var bound to a fresh,
uninterned symbol. See http://www.cliki.net/WITH-UNIQUE-NAMES"
  (flet ((binding-parts (binding)
	   "Return (values var prefix) from a WITH-UNIQUE-NAMES binding
form. If PREFIX is not given in the binding, NIL is returned to
indicate that the default should be used."
	   (if (consp binding)
	       (values (first binding) (second binding))
	       (values binding nil))))
    `(let ,(mapcar #'(lambda (binding)
		       (multiple-value-bind (var prefix)
			   (binding-parts binding)
			 (check-type var symbol)
			 `(,var (gensym ,(format nil "~A"
						 (or prefix var))))))
	    bindings)
       ,@body)))
  
(defmacro once-only (names &body body)
  ;; Check the NAMES list for validity.
  (let ((bad-name (find-if-not #'symbolp names)))
    (when bad-name
      (error "ONCE-ONLY expected a symbol but got ~S" bad-name)))
  ;; Do not touch this code unless you really know what you're doing.
  (let ((gensyms (loop for name in names collect (gensym (string name)))))
    `(let (,@(loop for g in gensyms
		   for name in names
		   collect `(,g (gensym ,(string name)))))
       `(let (,,@(loop for g in gensyms for n in names
		       collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms
			 collect `(,n ,g)))
	     ,@body)))))

(flet ((split-required-and-other-args (args)
	 (let* ((other-args (member-if (lambda (thing)
					 (and (symbolp thing)
					      (char= (char (symbol-name thing) 0)
						     #\&)))
				       args))
		(required-args (ldiff args other-args)))
	   (values required-args other-args))))
  (defmacro destructuring-lambda (args &body body)
    "Just like LAMBDA, except the required parameters can be
destructuring patterns as in DESTRUCTURING-BIND."
    (multiple-value-bind (required-args other-args) (split-required-and-other-args args)
      (multiple-value-bind (required-args-final destructures)
	  (let (raf destructures)
	    (dolist (arg required-args)
	      (etypecase arg
		(symbol (push arg raf))
		(cons (let ((gensym (gensym "ARG")))
			(push gensym raf)
			(push (list arg gensym) destructures)))))
	    (values (nreverse raf) (nreverse destructures)))
	`(lambda ,(append required-args-final other-args)
	   ,@(reduce (lambda (destructure form)
		       (list `(destructuring-bind ,@destructure ,@form)))
		     destructures
		     :from-end t :initial-value body))))))


;;; The following functions are analogous to MAPC/MAPCAR/MAPCAN
;;; and MAPL/MAPLIST/MAPCON, but for alists and plists.
;;;
;;; ┌───┬────────────────────────────┬────────┬───────────┬──────────┐
;;; │PKG│         Result acc. method │ nil    │ list      │ nconc    │
;;; ├───┼────────────────────────────┼────────┼───────────┼──────────┤
;;; │   │      Structure mapped over │        │           │          │
;;; │   │                            │        │           │          │
;;; │CL │         elements of a list │ MAPC   │ MAPCAR    │ MAPCAN   │
;;; │CL │           conses of a list │ MAPL   │ MAPLIST   │ MAPCON   │
;;; │   │                            │        │           │          │
;;; │LL │ (key . value)s of an alist │ MAPAL  │ MAPALIST  │ MAPACON  │
;;; │LL │   (key value)s of an alist │ MAPAL* │ MAPALIST* │ MAPACON* │
;;; │LL │    (key value)s of a plist │ MAPPL  │ MAPPLIST  │ MAPPCON  │
;;; └───┴────────────────────────────┴────────┴───────────┴──────────┘
;;;
;;; Note that the names of the new functions
;;; are constructed in a consistent way, based on MAPL/MAPLIST/MAPCON:
;;; MAP, then
;;; A or P, depending on what we're dealing with (alist or plist), then
;;; L, LIST or CON depending on accumulation method (nil, list or nconc).
;;; And finally, a star if the alist has its values
;;; in the second element instead of the cdr. The former is less common.

(defun mapal (function alist &rest more-alists)
  "Analogous to MAPL but for alists. The FUNCTION is passed two
arguments for each alist entry: the key and value. MAPAL returns nil."
  (if more-alists
      (error "mapal only supports one alist argument for now."))
  (loop for (key . value) in alist
	do (funcall function key value)))

(defun mapalist (function alist &rest more-alists)
  "Analogous to MAPLIST but for alists. The FUNCTION is passed two
arguments for each alist entry: the key and value. MAPALIST returns a
list composed of each value returned by FUNCTION."
  (if more-alists
      (error "mapalist only supports one alist argument for now."))
  (loop for (key . value) in alist
	collect (funcall function key value)))

(defun mapacon (function alist &rest more-alists)
  "Analogous to MAPCON but for alists. The FUNCTION is passed two
arguments for each alist entry: the key and value. MAPACON returns a
list composed of the concatenation of each list returned by FUNCTION."
  (if more-alists
      (error "mapacan only supports one alist argument for now."))
  (loop for (key . value) in alist
	nconc (funcall function key value)))

(defun mapal* (function alist &rest more-alists)
  "Just like MAPAL except the value is in the second element of each
entry, not the cdr."
  (if more-alists
      (error "mapal* only supports one alist argument for now."))
  (loop for (key value) in alist
	do (funcall function key value)))

(defun mapalist* (function alist &rest more-alists)
  "Just like MAPALIST except the value is in the second element of each
entry, not the cdr."
  (if more-alists
      (error "mapalist only supports one alist argument for now."))
  (loop for (key value) in alist
	collect (funcall function key value)))

(defun mapacon* (function alist &rest more-alists)
  "Just like MAPACON except the value is in the second element of each
entry, not the cdr."
  (if more-alists
      (error "mapacon only supports one alist argument for now."))
  (loop for (key value) in alist
	nconc (funcall function key value)))

(defun mappl (function list &rest more-lists)
  "Analogous to MAPL but for plists. The FUNCTION is passed two
arguments for each plist entry: the key and value. MAPPL returns nil."
  (if more-lists
      (error "mappl only supports one list argument for now."))
  (loop for (key value) on list by #'cddr
	do (funcall function key value)))

(defun mapplist (function list &rest more-lists)
  "Analogous to MAPLIST but for plists. The FUNCTION is passed two
arguments for each plist entry: the key and value. MAPPLIST returns a
list composed of each value returned by FUNCTION."
  (if more-lists
      (error "mapplist only supports one list argument for now."))
  (loop for (key value) on list by #'cddr
	collect (funcall function key value)))

(defun mappcon (function list &rest more-lists)
  "Analogous to MAPCON but for plists. The FUNCTION is passed two
arguments for each plist entry: the key and value. MAPACON returns a
list composed of the concatenation of each list returned by FUNCTION."
  (if more-lists
      (error "mappcon only supports one list argument for now."))
  (loop for (key value) on list by #'cddr
	nconc (funcall function key value)))


(defmacro for* (bindings &body body)
  "FOR* steps variables with support for a small but useful subset of
LOOP's FOR clause, which you can combine with any looping construct
whatsoever. Simply wrap the looping construct with FOR* and then call
the local function STEP* at the start of each loop body iteration. The
first time STEP* is called, the variables are initialized to their
first value. On subsequent calls, the values are updated. The updates
are always made in sequence, analogously to LET* and SETF. It is not
semantically meaningful to read the variables before STEP* has been
called at least once.

FOR* deals with stepping of variables only.
It never initiates a termination of looping.
This is to avoid interfering with the semantics
of the looping construct we're wrapping.

Each binding is (VAR . STEPPING).
Here are the supported STEPPING clauses:

for-as-arithmetic: you can use FROM, UPFROM, DOWNFROM and BY like you
would with LOOP: \"FROM start\" or \"UPFROM start\" for incrementing
from start, \"DOWNFROM start\" for decrementing. Use \"BY step\" to
control the stepping (default is 1).

for-as-equals: you can use = and THEN like you would with LOOP:

Example:
\(mapcar (for* ((i downfrom (expt 2 5) by 3)
		(oddp = (oddp i))
		(j from 0)
		(firstp = t then nil)
		(k downfrom 0 by 2))
	   (lambda (element)
	     (step*)
	     `((element ,element) (i ,i) (oddp ,oddp)
	       (j ,j) (firstp ,firstp) (k ,k)
	       ((+ i j k) ,(+ i j k)))))
	 '(a b c))
==> (((ELEMENT A) (I 32) (ODDP NIL) (J 0) (FIRSTP T) (K 0) ((+ I J K) 32))
     ((ELEMENT B) (I 29) (ODDP T) (J 1) (FIRSTP NIL) (K -2) ((+ I J K) 28))
     ((ELEMENT C) (I 26) (ODDP NIL) (J 2) (FIRSTP NIL) (K -4) ((+ I J K) 24)))"
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
      (with-unique-names (first-time-p)
	`(let* ((,first-time-p t)
		,@vars)
	   (flet ((step* ()
		    (if ,first-time-p
			(setf ,first-time-p nil
			      ,@(mapcan #'list vars inits))
			(setf ,@(mapcan #'list vars steps)))
		    nil))
	     ,@body))))))


(defmacro collecting (&body body)
  "Collect things into a list forwards. Within the body of this macro,
the COLLECT function will collect its argument into the list returned
by COLLECTING, then return that argument."
  (with-unique-names (collector tail)
    `(let (,collector ,tail)
       (labels ((collect (thing)
		  (if ,collector
		      (setf (cdr ,tail)
			    (setf ,tail (list thing)))
		      (setf ,collector
			    (setf ,tail (list thing))))
		  thing)
		(ncollect (list)
		  (if ,collector
		      (setf (cdr ,tail) list)
		      (setf ,collector list))
		  (if list
		      (setf ,tail (last list)))
		  nil))
	 ,@body)
       ,collector)))

;; These should only be called inside of COLLECTING macros, but we
;; define them here to provide informative error messages and to make
;; it easier for SLIME (et al.) to get documentation for the COLLECT
;; and NCOLLECT functions when they're used in the COLLECTING macro.
(defun collect (thing)
  "Collect THING in the context established by the COLLECTING macro.
Return THING."
  (error "Can't collect ~S outside the context of the COLLECTING macro"
	 thing))

(defun ncollect (list)
  "Destructively collect LIST in the context established by the
COLLECTING macro as if by NCONC. Return NIL. This is equivalent
to (mapc #'collect list) except faster (because no consing) and LIST
must be prepared to have its tail destructively modified the next time
COLLECT or NCOLLECT is called.

Calling this function NCOLLECT has at least two advantages:
1. The N prefix reminds you that you must pass a fresh list
that isn't referenced anywhere else because of the destructive behavior
\(its tail will be modified the next time COLLECT or NCOLLECT is called).
2. NCOllect starts the same as NCOnc.
Other candidates would have been COLLECT-LIST (a bit long)
and NCONCING (potential conflict with ITERATE)."
  (error "Can't ncollect ~S outside the context of the COLLECTING macro"
	 list))


(flet ((check-collectors (collectors)
	 "Check that all of the COLLECTORS are symbols. If not, raise an error."
	 (let ((bad-collector (find-if-not #'symbolp collectors)))
	   (when bad-collector
	     (error 'type-error
		    :datum bad-collector
		    :expected-type 'symbol))))
       (gensyms-alist (collectors)
	 "Return an alist mapping the symbols in COLLECTORS to gensyms"
	 (mapcar #'cons collectors
		 (mapcar (compose #'gensym
				  #'(lambda (x)
				      (format nil "~A-TAIL-" x)))
			 collectors))))
  (defmacro with-collectors ((&rest collectors) &body body)
    "Collect some things into lists forwards. The names in COLLECTORS
are defined as local functions which each collect into a separate
list.  Returns as many values as there are collectors, in the order
they were given."
    (check-collectors collectors)
    (let ((gensyms-alist (gensyms-alist collectors)))
      `(let ,(loop for collector in collectors
		   for tail = (cdr (assoc collector gensyms-alist))
		   nconc (list collector tail))
	 (labels ,(loop for collector in collectors
			for tail = (cdr (assoc collector gensyms-alist))
			collect `(,collector (thing)
					     (if ,collector
						 (setf (cdr ,tail)
						       (setf ,tail (list thing)))
						 (setf ,collector
						       (setf ,tail (list thing))))))
	   ,@body)
	 (values ,@collectors)))))

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function."
  #'(lambda (x)
      (reduce #'funcall functions
	      :initial-value x
	      :from-end t)))

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


(defmacro doalist ((key value alist &optional result) &body body)
  "Analogous to DOLIST, but for alists.
Throws an error if one of the items in the list is not a list (cons or
nil). If an entry is NIL, then both key and value will be NIL (or
should this throw an error?)."
  (with-unique-names (assoc)
    `(dolist (,assoc ,alist ,result)
      (let ((,key (car ,assoc))
	    (,value (cdr ,assoc)))
	,@body))))

(defmacro doalist* ((key value alist &optional result) &body body)
  "Just like DOALIST except the value is in the second element of each
entry, not the cdr."
  (with-unique-names (assoc)
    `(dolist (,assoc ,alist ,result)
       (let ((,key (first ,assoc))
	     (,value (second ,assoc)))
	 ,@body))))

(defmacro doplist ((key value plist &optional result) &body body)
  "Analogous to DOLIST, but for plists."
  `(loop for (,key ,value) on ,plist by #'cddr
	 do (progn ,@body)
	 finally (return ,result)))

(defmacro dovector ((var vector &optional result) &body body)
  "Analogous to DOLIST, but for vectors."
  `(loop for ,var across ,vector
	 do (progn ,@body)
	 ,@(if result
	       `(finally (return (let ((,var nil))
				   ,result))))))

(defmacro dotimes* ((count &optional result) &body body)
  "Just like DOTIMES, but without a var argument.
This saves you from having to name the variable and makes explicit the
fact you won't need its value. Of marginal value, but offers a nice
alternative to (loop repeat count do body finally result)."
  (with-unique-names (ignored-var)
    `(dotimes (,ignored-var ,count ,@(if result (list result)))
       ,@body)))

(defmacro while* (test &body body)
  "TEST is evaluated. If the result is true, then BODY is executed as
an implicit PROGN, else the loop terminates. This cycle repeats as
long as TEST evaluates to true."
  `(loop while ,test do (progn ,@body)))
