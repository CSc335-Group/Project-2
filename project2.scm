;;; Project 2
;;; Group members: Baishaki Debi, Yi Lin
;;; Email Addresses: bdebi000@citymail.cuny.edu, ylin026@citymail.cuny.edu
;;; Spring 2022


;;; ALL INPUTTED PROPOSITIONS MUST BE CREATED USING THE CONSTRUCTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; LOAD CONSTRUCTORS/SELECTORS/CLASSIFIERS

(load "infix_doc.scm")
;;;(load "prefix_doc.scm")

;;; --------------------------------------------------------------------------------------------------------------


;;; PART 1: FRONT-END
;;; --------------------------------------------------------------------------------------------------------------
;;; CONVERT-OR FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs a OR (v) proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (-) and ANDs (^)
;;; NOTE: (p v q) is logically equivalent to -(-p ^ -q)

(define (convert-or e)
  (let ((first-op (first-operand e)) (second-op (second-operand e)))
    (let ((and-exp (make-and (make-not first-op) (make-not second-op))))
      (make-not and-exp))))

;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; CONVERT-IMPLY FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs an IMPLY (=>) proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (-) and ANDs (^)
;;; NOTE: (p => q) is logically equivalent to (-p v q) is logically equivalent to -(p ^ -q)

(define (convert-imply e)
  (let ((first-op (first-operand e)) (second-op (second-operand e)))
    (let ((or-exp (make-or (make-not first-op) second-op)))
      (convert-or or-exp))))

;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; SIMPLIFIER FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs a proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (-) and ANDs (^)

(define (simplifier e)
  (cond ((symbol? e) e)
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (cond ((or? e) (convert-or (make-or (simplifier first-op) (simplifier second-op))))
                 ((imply? e) (convert-imply (make-imply (simplifier first-op) (simplifier second-op))))
                 (else (make-and (simplifier first-op) (simplifier second-op))))))
        (else (let ((first-op (first-operand e)))
                (make-not (simplifier first-op))))))

;;; STRUCTURAL INDUCTION PROOF:
;;; BASIS:
;;; A symbol contains no operators so the result is vacuously true

;;; INDUCTION HYPOTHESIS (IH):
;;; Assume that for each of the components of the proposition currently under consideration, there is a logically
;;; equivalent proposition using just AND (^) and NOT (-) operators

;;; INDUCTION STEP (IS):
;;; Consider a proposition P. There are 4 cases:

;;; 1. P = (R ^ S); R and S are components
;;; By the IH, there is a proposition R' that is logically equivalent to R, where R' is expressed using only the
;;; AND (^) and NOT (-) operators. Similarly, there is a proposition S' that is logically equivalent to S, where S'
;;; is expressed using only the AND (^) and NOT (-) operators.
;;; (R ^ S) is then logically equivalent to (R' ^ S') which is already a proposition using only AND (^) and NOT (-)
;;; operators. We are done with this case.

;;; 2. P = (R v S)
;;; Recall that (R v S) is logically equivalent to -(-R ^ -S) which is equivalent to -(-R' ^ -S'), where by the IH,
;;; R' and S' are logically equivalent propositions to R and S, respectively, using only AND (^) and NOT (-) operators.
;;; We have just found a logically equivalent proposition to P so we are done with this case.

;;; 3. P = -R
;;; To reiterate, by the IH, there is a proposition R' that is logically equivalent to R, where R' is expressed
;;; using only the AND (^) and NOT (-) operators. So, -R is logically equivalent to -R'.
;;; Since -R' is a proposition using only the AND (^) and NOT (-) operators, we are done with this case.

;;; 4. P = (R => S)
;;; Recall that (R => S) is logically equivalent to (-R v S) which in turn is logically equivalent to -(R ^ -S).
;;; Then, -(R ^ -S) is logically equivalent to -(R' ^ -S'), where by the IH, R' and S' are logically equivalent
;;; propositions to R and S, respectively, using only AND (^) and NOT (-) operators. Since we have found a logically
;;; equivalent proposition of P using just AND (^) and NOT (-) operators, we are done with this case.
;;; ------------------------------------------------------------------------------------------------------------------


;;; PART 2: BACK-END
;;; ------------------------------------------------------------------------------------------------------------------
;;; interpreter
;;;(pair? (cdr '((x #t) (y #t))))

;;;(eq? 'x (car '(x #t)))

;;;(define (tst x)
;;;  (eq? x (car '(y #t))))

(define (lookup x alist)
  (cond ((eq? (caar alist) x) (cadar alist)) ;; if x is in current pair
        ((eq? (cdr alist) '()) '()) ;; if x is not in the list
        (else (lookup x (cdr alist)))))

;;; imply function
(define (imply p q)
  (or (not p) q))

;;; value the expression
(define (value e alist)
  (cond ((symbol? e) (lookup e alist))
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (cond ((and? e) (and (value first-op alist) (value second-op alist)))
                 ((or? e) (or (value first-op alist) (value second-op alist)))
                 ((imply? e) (imply (value first-op alist) (value second-op alist))))))
        (else
         (let ((first-op (first-operand e)))
           (not (value first-op alist))))))

;;; ------------------------------------------------------------------------------------------------------------------


;;; PART 3: CONNECTING FRONT-END & BACK-END
;;; ------------------------------------------------------------------------------------------------------------------
;;; WRAPPER FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs a proposition e that was created using the constructors and an association list, alist,
;;;                contains values for each symbol in the proposition
;;; Post-Condition: returns the truth value of the proposition using the values in alist for the symbols

(define (wrapper e alist)
  (value (simplifier e) alist))


;;; TEST INPUTS
(define l1 '((x #f) (y #f) (z #t)))

(define e1 (make-or 'x 'y))
(define e2 (make-imply 'x 'z))
(define e3 (make-and 'x 'y))
(define e4 (make-not 'x))
(define e5 (make-not (make-and 'x 'y)))
(define e6 (make-not e1))
(define e7 (make-and e1 e2))

(value (simplifier e7) l1)
(value (simplifier e2) l1)

;;; ------------------------------------------------------------------------------------------------------------------


;;; USER TOOLS
;;; ------------------------------------------------------------------------------------------------------------------
;;; ALIST-COMPLETE FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs a proposition e that was created using the constructors and an association list alist
;;; Post-Condition: returns #t if all symbols in e are in alist. #f, otherwise.

(define (aList-complete e alist)
  (cond ((symbol? e)
         (if (eq? (lookup e alist) '()) #f #t))
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (if (and (eq? (aList-complete first-op alist) #t) (eq? (aList-complete second-op alist) #t)) #t #f)))
        (else (let ((first-op (first-operand e)))
                (aList-complete first-op alist)))))

;;; STRUCTURAL INDUCTION PROOF:
;;; BASIS:
;;; If the proposition is just a symbol, then we can directly check alist using lookup function. If lookup function
;;; doesn't find it in alist, return #f (symbol is not in alist). Otherwise, #t (symbol is in alist)

;;; INDUCTION HYPOTHESIS (IH):
;;; Assume that for the components of the proposition currently under consideration, that we already know if
;;; all the symbols in the components are in the alist or not

;;; INDUCTION STEP (IS):
;;; Consider a proposition P. There are 2 cases:

;;; 1. P is a proposition with 2 operands (in other words, P is either an AND (^), OR (v), or IMPLY (=>)
;;; proposition). R and S are components of P. By the IH, we know whether the symbols in proposition R are all
;;; in alist or not. Similarly, by the IH, we know whether the symbols in proposition S are all in alist or not.
;;; Then, all that's left is to confirm that both components do indeed have all the symbols in the alist (recursive
;;; call returned #t for both components). If the check passes, return #t. Otherwise, return #f.

;;; 2. P is a proposition with 1 operand R (in other words, P is a NOT (-) proposition).
;;; To reiterate, by the IH, we know whether the symbols in proposition R are all in alist or not. Then, all that's
;;; left is to just return that result (#t if all symbols are in alist, #f otherwise). 

;;; TEST INPUTS:
;;;(define l1 '((x #f) (y #f) (z #t)))

;;;(aList-complete '((((x -) ^ (y -)) -) ^ ((x ^ (z -)) -)) l1)
;;;(aList-complete '((((x -) ^ (p -)) -) ^ ((x ^ (z -)) -)) l1)

;;;(aList-complete '((x ^ (z -)) -) l1)
;;;(aList-complete '((x ^ (q -)) -) l1)
;;; ------------------------------------------------------------------------------------------------------------------