(defpackage #:loopless-examples
  (:use #:cl #:loopless))

;;;; This file contains examples of how to convert your LOOPs.
;;;; The examples are roughly in increasing order of complexity.

;;;; To make the distinction between what Common Lisp already provides
;;;; and what is part of LOOPless easy, I will prefix symbols that are
;;;; part of the latter with LL, the package nickname of LOOPless.


;;;; -----------------------------------------------------------------

(loop for element in '(a b c)
      for i downfrom 10 by 3
      collect (list element i (* i 10)))

;; ==> ((A 10 100) (B 7 70) (C 4 40))

(mapcar (LL:for* ((i downfrom 10 by 3))
	  (lambda (element)
	    (LL:step*)
	    (list element i (* i 10))))
	'(a b c))

;;;; -----------------------------------------------------------------


(loop for (key . value) in '((a . 1) (b . 2) (c . 3))
      collect (list key (- value)))

;; ==> ((A -1) (B -2) (C -3))

(LL:mapalist (lambda (key value)
	       (list key (- value)))
	     '((a . 1) (b . 2) (c . 3)))


;;;; -----------------------------------------------------------------


(loop for element in '(a 24 x y 86 "test" (nested stuff))
      if (symbolp element)
	collect element
      else if (numberp element)
	     collect (- element)
      else if (listp element)
	     nconc (copy-seq element))

;; ==> (A -24 X Y -86 NESTED STUFF)

;; Here I'm not actually using anything new provided by LOOPless.
(mapcan (lambda (element)
	  (typecase element
	    (symbol (list element))
	    (number (list (- element)))
	    (list (copy-seq element))))
	'(a 24 x y 86 "test" (nested stuff)))


;;;; -----------------------------------------------------------------


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

;;;; -----------------------------------------------------------------


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


;;;; -----------------------------------------------------------------


(loop for (key (symbol count))
	on '(:repeat (a 1) :these (b 3) :somehow (c 2)) by #'cddr
      nconc (loop repeat count nconc (list key symbol)))

;; ==> (:REPEAT A :THESE B :THESE B :THESE B :SOMEHOW C :SOMEHOW C)

(LL:mappcon (LL:destructuring-lambda (key (symbol count))
	      (LL:collecting
		(LL:dotimes* (count)
		  (LL:collect key)
		  (LL:collect symbol))))
	    '(:repeat (a 1) :these (b 3) :somehow (c 2)))
