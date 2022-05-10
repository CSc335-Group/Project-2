;;; Project 2
;;; Group member: Baishaki Debi, Yi Lin
;;; Email Addresses: bdebi000@citymail.cuny.edu, ylin026@citymail.cuny.edu
;;; Spring 2022


;;; PART 1


;;; CONSTRUCTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-AND FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the AND (^) operator on the inputed expressions

;;; CODE
(define (make-and e1 e2)
  (list e1 '^ e2))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-OR FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the OR (v) operator on the inputed expressions

;;; CODE
(define (make-or e1 e2)
  (list e1 'v e2))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-NOT FUNCTION
;;; Pre-condition: inputs an expressions representing a proposition
;;; Post-condition: returns the expression obtained from performing the NOT (-) operator on the inputed expression

;;; CODE
(define (make-not e)
  (list e '-))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-IMPLY FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the IMPLY (=>) operator on the inputed
;;;                 expressions

;;; CODE
(define (make-imply e1 e2)
  (list e1 '=> e2))

;;; --------------------------------------------------------------------------------------------------------------


;;; SELECTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; FIRST-OPERAND FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns the expression representing the first operand of the inputed expression

;;; CODE
(define (first-operand e)
  (car e))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; SECOND-OPERAND FUNCTION
;;; Pre-condition: inputs an expression representing either a AND, OR, IMPLY proposition
;;; Post-condition: returns the expression representing the second operand of the inputed expression

;;; CODE
(define (second-operand e)
  (caddr e))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; OPERATOR FUNCTION
;;; Pre-condition: inputs an expression representing either a AND, OR, IMPLY proposition
;;; Post-condition: returns the operator of the inputed expression

;;; CODE
(define (operator e)
  (cadr e))

;;; --------------------------------------------------------------------------------------------------------------


;;; CLASSIFIERS
;;; --------------------------------------------------------------------------------------------------------------
;;; AND? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an AND expression, #f otherwise

;;; CODE
(define (and? e)
  (eq? (operator e) '^))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; OR? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an OR expression, #f otherwise

;;; CODE
(define (or? e)
  (eq? (operator e) 'v))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; NOT? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an NOT expression, #f otherwise

;;; CODE
(define (not? e)
  (eq? (operator e) '-))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; IMPLY? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an IMPLY expression, #f otherwise

;;; CODE
(define (imply? e)
  (eq? (operator e) '=>))

;;; --------------------------------------------------------------------------------------------------------------


;;; trying out some code for part 1
;;; idea: we're given an expression that could either be a not, and, or, imply expression
;;; based on what type of expression it is we can use the appropriate conversions:
;;; (p v q) equivalent to -(-p ^ -q)
;;; (p => q) equivalent to (-p v q) equivalent to -(p ^ -q)

(define (convert-or e)
  (let ((first-op (first-operand e)) (second-op (second-operand e)))
    (let ((and-exp (make-and (make-not first-op) (make-not second-op))))
      (make-not and-exp))))

(define (convert-imply e)
  (let ((first-op (first-operand e)) (second-op (second-operand e)))
    (let ((or-exp (make-or (make-not first-op) second-op)))
      (convert-or or-exp))))

(define (check-exp e)
  (or (equal? e #t) (equal? e #f)))

(define (convert-exp e)
  (cond ((check-exp e) e)
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
            (cond ((and (check-exp first-op) (check-exp second-op))
                   (cond ((or? e) (convert-or e))
                         ((imply? e) (convert-imply e))
                         (else e)))
                  (else (cond ((or? e) (convert-or (make-or (convert-exp first-op) (convert-exp second-op))))
                              ((imply? e) (convert-imply (make-imply (convert-exp first-op) (convert-exp second-op))))
                              (else (make-and (convert-exp first-op) (convert-exp second-op))))))))
        (else (let ((first-op (first-operand e)))
                (cond ((check-exp first-op) e)
                      (else (make-not (convert-exp first-op))))))))

(define e1 (make-or #t #f))
(define e2 (make-imply #t #f))
(define e3 (make-and #t #f))
(define e4 (make-not #t))
(define e5 (make-not (make-and #t #f)))
(define e6 (make-not e1))
(define e7 (make-and e1 e2))
;(convert-exp e1)
;(convert-exp e2)
;(convert-exp e3)
;(convert-exp e4)
;(convert-exp e5)
;(convert-exp e6)
(convert-exp e7)
    








    