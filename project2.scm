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
;;; Assume that for each component in the proposition currently under consideration, we have already
;;; re-written the component into a logically equivalent proposition using just AND (^) and NOT (-) operators

;;; INDUCTION STEP (IS):
;;; Consider a proposition P. There are 4 cases:

;;; 1. P = (R ^ S); R and S are components
;;; By the IH, R' is the re-written proposition that is logically equivalent to R, where R' is expressed using only
;;; the AND (^) and NOT (-) operators. Similarly, S' is the re-written proposition that is logically equivalent to S,
;;; where S' is expressed using only the AND (^) and NOT (-) operators.
;;; (R ^ S) is then logically equivalent to (R' ^ S') so all we need to do is call the make-and constructor on R' and
;;; S' to re-write (R ^ S).

;;; 2. P = (R v S)
;;; (R v S) is logically equivalent to (R' v S'), where by the IH, R' and S' are re-written propositions that are
;;; logically equivalent to R and S, respectively, using only AND (^) and NOT (-) operators.
;;; Now, all that's needed to be done is to call convert-or function that will convert (R' v S') to the logically
;;; equivalent proposition -(-R' ^ -S').

;;; 3. P = -R
;;; To reiterate, by the IH, R' is the re-written proposition that is logically equivalent to R, where R' is
;;; expressed using only the AND (^) and NOT (-) operators. So, -R is logically equivalent to -R'.
;;; Then, all that's left to do is to call the make-not constructor on R'.

;;; 4. P = (R => S)
;;; (R => S) is logically equivalent to (R' => S'), where by the IH, R' and S' are re-written propositions that are
;;; logically equivalent to R and S, respectively, using only AND (^) and NOT (-) operators. All that's left now to
;;; do is to call convert-imply function on (R' => S') to convert it to the logically equivalent proposition
;;; -(R' ^ -S').
;;; ------------------------------------------------------------------------------------------------------------------

;;; TEST INPUTS
(define e1 (make-or 'x 'y))
(define e2 (make-imply 'x 'z))
(define e3 (make-and 'x 'y))
(define e4 (make-not 'x))
(define e5 (make-not (make-and 'x 'y)))
(define e6 (make-not e1))
(define e7 (make-and e1 e2))

;;; (simplifier e1) => (((x -) ^ (y -)) -)
;;; (simplifier e2) => ((x ^ (z -)) -)
;;; (simplifier e5) => ((x ^ y) -)


;;; PART 2: BACK-END
;;; ------------------------------------------------------------------------------------------------------------------


;;; LOOKUP FUNCTION

;;; SPECIFICATION
;;; Pre-condition: input a symbol x, and a association list alist
;;; Post-condition: if x is in the association list, then it will output its value,
;;;   if x is not iin the  association list, then it will return a null list '()

;;; DESIGN IDEA
;;; iterative procedure
;;; Check every pair of of association list, say current pair = (car alist), for the termination, if x is the first element of current pair,
;;; then it will terminates and return the value of second elements of current pair, and if we check through the list, the x does not appear
;;; in any pair of the association list

;;; orignal ALIST
;;; ------------------------------------------
;;; already checked pairs | not checked yet
;;; ------------------------------------------
;;;                     (alist head ....     )

(define (lookup x alist)
  (cond ((eq? (caar alist) x) (cadar alist)) ;; if x is in current pair
        ((eq? (cdr alist) '()) '()) ;; if x is not in the list
        (else (lookup x (cdr alist)))))

;;; PROOF
;;; we let original accociation list to be ALIST, 
;;; guess invariant: x appears in any pair of original ALIST iff x appears in alist

;;; Strong enough? when the program start, alist = ALIST, then our GI is true.
;;; Weak enough? when the program terminates, as we mentioned in design idea, there are two cases cause the program terminates
;;;   1. x was fould in current head pair of alist, x must appears in the same pair of ALIST, then our GI is true
;;;   2. alist contains only a element of a pair and x is not appear in the pair, x is not in the orignal ALIST, our GI is true
;;; Preservable? We assume that GI is true before each iterative call, while the program does not terminates, that means x is not in current head pair,
;;;   and alist does contains at least 2 pairs, then for the next call new alist becomes (cdr alist), then our GI: x appears in any pair of ALIST iff x
;;;   appears in alist still maintained, since previous head pair does not contains x.

;;; Q.E.D

;;; TEST DATA
;;; (lookup 'x '((x #f) (y #t) (z #f))) => #f
;;; (lookup 'y '((x #f) (y #t) (z #f))) => #t
;;; (lookup 'z '((x #f) (y #t) (z #f))) => #f
;;; (lookup 'w '((x #f) (y #f) (z #f))) => ()



;;; IMPLY FUNCTION

;;; SPECIFICATION
;;; Pre-condition: input 2 boolean value p and q
;;; Post-condition: returns the boolean value of p implies q

;;; USE p=>q <=> -pvq
(define (imply p q)
  (or (not p) q))

;;; TEST INPUTS
;;; (imply #t #t) => #t
;;; (imply #t #f) => #f
;;; (imply #f #t) => #t
;;; (imply #f #f) => #t


;;; VALUE FUNCTION

;;; SPECIFICATION:
;;; Pre-Condition: inputs a proposition e that was created using the constructors and a valid association list alist
;;;   (every symbol x appears in e can be foulded with its associated value in alist)
;;; Post-Condition: it will returns the value of e that each symbol of e is evaluated to its aoociated value in alist

;;; CODE:
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

;;; STRUCTURAL INDUCTION PROOF:

;;; Let P to be the least class that contains all the infix propersition

;;; BASIS:
;;; If the proposition is just a symbol, then we can directly check alist using lookup function. Since we assume our alist to be valid
;;; that is, every symbol x appears in e can be founded with its associated value in alist, so (lookup e alist) will return the boolean
;;; value of e when e is a symbol which is correct.

;;; INDUCTION HYPOTHESIS (IH)
;;; For any porposition p \in P, assume the value of all the components of p are true

;;; INDUCTION STEP:
;;; Here we have 2 cases:
;;;  1. p is a infix proposition with 2 operands (in other words, p is either an AND (^), OR (v), or IMPLY (=>)
;;;     proposition). r and s are components of p that p = (r op s). By the IH, we know the value of r and s are all true. 
;;;     Then, since we have all the corrected value of r and s, then the value of p is going to be (operator (value of r) (value of s))
;;;     which is true
;;;  2. p is an infix proposition with only one operand (p is NOT (-)). let r to be a component of p that p = -r. By the IH,
;;;     we know the value of r is true. then the value of p is going to be (not (value of r)) which is correct!


;;; Q.E.D

;;; TEST INPUTS
;;; (value '((x v y) ^ (x => z)) '((x #t) (y #f) (z #t))) => #t
;;;          (T v F) ^ (T => T) = T
;;; (value '((x v y) -) '((x #f) (y #t))) => #f
;;;         (- (F v T)) = F


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

;;; (value e7 l1) => #f
;;; (wrapper e7 l1) => #f
;;; (value e2 l1) => #t
;;; (wrapper e2 l1) => #t

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

;;;(aList-complete '((((x -) ^ (y -)) -) ^ ((x ^ (z -)) -)) l1) => #t
;;;(aList-complete '((((x -) ^ (p -)) -) ^ ((x ^ (z -)) -)) l1) => #f

;;;(aList-complete '((x ^ (z -)) -) l1) => #t
;;;(aList-complete '((x ^ (q -)) -) l1) => #f
;;; ------------------------------------------------------------------------------------------------------------------