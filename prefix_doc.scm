;;; Prefix Constructors, Selectors, & Classifiers


;;; CONSTRUCTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-AND FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the AND (^) operator on the inputed expressions

;;; CODE
(define (make-and e1 e2)
  (list '^ e1 e2))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-OR FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the OR (v) operator on the inputed expressions

;;; CODE
(define (make-or e1 e2)
  (list 'v e1 e2))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-NOT FUNCTION
;;; Pre-condition: inputs an expressions representing a proposition
;;; Post-condition: returns the expression obtained from performing the NOT (-) operator on the inputed expression

;;; CODE
(define (make-not e)
  (cond ((and (not (symbol? e)) (not? e)) (first-operand e))
        (else (list '- e))))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-IMPLY FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the IMPLY (=>) operator on the inputed
;;;                 expressions

;;; CODE
(define (make-imply e1 e2)
  (list '=> e1 e2))

;;; --------------------------------------------------------------------------------------------------------------


;;; SELECTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; FIRST-OPERAND FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns the expression representing the first operand of the inputed expression

;;; CODE
(define (first-operand e)
  (cadr e))

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
  (cond ((symbol? e) '())
        (else (car e))))

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
