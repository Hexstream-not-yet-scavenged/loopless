(defpackage #:loopless-examples
  (:use #:cl #:loopless))

;;;; This file contains examples of how to convert your LOOPs.
;;;; The examples are roughly in increasing order of complexity.

;;;; To make the distinction between what Common Lisp already provides
;;;; and what is part of LOOPless easy, I will prefix symbols that are
;;;; part of the latter with LL, the package nickname of LOOPless.


;;;; -- Reading forms from a stream with WHILE* ----------------------


(with-input-from-string (stream "some (simple \"forms\") #(to read)")
  (loop for form = (read stream nil nil)
	while form
	collect form))

;; ==> (SOME (SIMPLE "forms") #(TO READ))

(with-input-from-string (stream "some (simple \"forms\") #(to read)")
  (LL:collecting
    (let (form)
      (LL:while* (setf form (read stream nil nil))
	(LL:collect form)))))

;; This may not look much better than the LOOP way at first,
;; but notice that each of COLLECTING/COLLECT, LET, WHILE* and SETF
;; does only one thing and can be used with the rest of the language.

;; Also, LET and SETF are conveniently already part of the language.


;;;; -- Using for-as-arithmetic outside of LOOP with FOR* ------------


(loop for element in '(a b c)
      for i downfrom 10 by 3
      collect (list element i (* i 10)))

;; ==> ((A 10 100) (B 7 70) (C 4 40))

(mapcar (LL:for* ((i downfrom 10 by 3))
	  (lambda (element)
	    (LL:step*)
	    (list element i (* i 10))))
	'(a b c))

;; There is a frequent need to have arithmetic stepping of a variable.
;; Thanks to FOR*, you can combine LOOP's intuitive syntax
;; for this with any looping construct whatsoever.


;;;; -- Using MAPALIST to map over an alist much like MAPCAR ---------


(loop for (key . value) in '((a . 1) (b . 2) (c . 3))
      collect (list key (- value)))

;; ==> ((A -1) (B -2) (C -3))

(LL:mapalist (lambda (key value)
	       (list key (- value)))
	     '((a . 1) (b . 2) (c . 3)))

;; Once you know MAPL/MAPLIST/MAPCON,
;;      learning MAPAL/MAPALIST/MAPACON,
;;               MAPAL*/MAPALIST*/MAPACON*,
;;               MAPPL/MAPPLIST/MAPPCON,
;;               MAPV/MAPVECTOR/MAPVCON,
;;               MAPT/MAPTIMES/MAPTCON and
;;               MAPT*/MAPTIMES*/MAPTCON* is trivial.
;;
;; There is a nice clean summary table of these functions
;; in loopless.lisp, right before the definition of MAPAL.


;;;; -- Using MAPCAN when you need to both collect and nconc ---------


(loop for element in '(a 24 x y 86 "test" (nested stuff))
      if (symbolp element)
	collect element
      else if (numberp element)
	     collect (- element)
      else if (listp element)
	     nconc (copy-seq element))

;; ==> (A -24 X Y -86 NESTED STUFF)

(mapcan (lambda (element)
	  (typecase element
	    (symbol (list element))
	    (number (list (- element)))
	    (list (copy-seq element))))
	'(a 24 x y 86 "test" (nested stuff)))

;; Here I'm not actually using anything new provided by LOOPless.
;; Sometimes the standard already provides suitable alternatives to LOOP.


;;;; -- Using with-collectors to collect into different lists  -------


(loop for element across #(a 24 b "test" 86 "this" c)
      if (symbolp element)
	collect element into symbols
      else if (numberp element)
	     collect element into numbers
      else if (stringp element)
	     collect element into strings
      finally (return (values symbols numbers strings)))

;; ==> (A B C)
;;     (24 86)
;;     ("test" "this")

(LL:with-collectors (symbols numbers strings)
  (LL:dovector (element #(a 24 b "test" 86 "this" c))
    (etypecase element
      (symbol (symbols element))
      (number (numbers element))
      (string (strings element)))))

;; Notice how we can use ETYPECASE
;; because we don't have to conform to LOOP syntax.

;; It's true that ITERATE doesn't have that problem,
;; but with LOOPless you'll need a LOOP-like construct
;; so rarely that LOOP's quirks are less of a problem.
;; Conversely, ITERATE's marginal advantages
;; over LOOP are less compelling.


;;;; -- Using FOR* for intuitive stepping of multiple variables ------


(loop for element in '(a b c)
      for i downfrom (expt 2 5) by 3
      for oddp = (oddp i)
      for j from 0
      for firstp = t then nil
      for k downfrom 0 by 2
      collect `((element ,element) (i ,i) (oddp ,oddp)
		(j ,j) (firstp ,firstp) (k ,k)
		((+ i j k) ,(+ i j k))))

;; ==> (((ELEMENT A) (I 32) (ODDP NIL) (J 0) (FIRSTP T) (K 0) ((+ I J K) 32))
;;	((ELEMENT B) (I 29) (ODDP T) (J 1) (FIRSTP NIL) (K -2) ((+ I J K) 28))
;;	((ELEMENT C) (I 26) (ODDP NIL) (J 2) (FIRSTP NIL) (K -4) ((+ I J K) 24)))

(mapcar (LL:for* ((i downfrom (expt 2 5) by 3)
		  (oddp = (oddp i))
		  (j from 0)
		  (firstp = t then nil)
		  (k downfrom 0 by 2))
	  (lambda (element)
	    (LL:step*)
	    `((element ,element) (i ,i) (oddp ,oddp)
	      (j ,j) (firstp ,firstp) (k ,k)
	      ((+ i j k) ,(+ i j k)))))
	'(a b c))


;;;; -- Using DESTRUCTURING-LAMBDA to destructure required args ------


(loop for (key (symbol count))
	on '(:repeat (a 1) :these (b 3) :somehow (c 2)) by #'cddr
      nconc (loop repeat count nconc (list key symbol)))

;; ==> (:REPEAT A :THESE B :THESE B :THESE B :SOMEHOW C :SOMEHOW C)

(LL:mappcon
 (LL:destructuring-lambda (key (symbol count))
   (LL:collecting
     (LL:dotimes* (count)
       (LL:collect key)
       (LL:collect symbol))))
 '(:repeat (a 1) :these (b 3) :somehow (c 2)))

;; When using functions in the "MAP* family", there's a frequent need
;; for a lambda which immediately destructures one or more of its args.

;; Using an explicit lambda is cumbersome because you have to name
;; the variable and then immediately pass it to DESTRUCTURING-BIND.
;; Moreover, writing it out uses an additional indentation level.

;; Speaking of indentation, when you're MISERable an easy trick for
;; great space savings is to format the mapping function like I did here.
;; These kinds of functions stay very readable when the function arg
;; is an explicit lambda despite being formatted this way,
;; because the additional indentation level this results in
;; ensures that the subsequent arg is isolated and easily spotted.
;; This paragraph is sloppily written.
