;;; Project 2
;;; Group member: Baishaki Debi, Yi Lin
;;; Email Addresses: bdebi000@citymail.cuny.edu, ylin026@citymail.cuny.edu
;;; Spring 2022

;;; LOAD CONSTRUCTORS/SELECTORS/CLASSIFIERS
(load "infix_doc.scm")
;;;(load "prefix_doc.scm")

;;; PART 1
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


(define (convert-exp e)
  (cond ((symbol? e) e)
        ((not (not? e)) ;; if e is not not expr
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (cond ((or? e) (convert-or (make-or (convert-exp first-op) (convert-exp second-op))))
                 ((imply? e) (convert-imply (make-imply (convert-exp first-op) (convert-exp second-op))))
                 (else (make-and (convert-exp first-op) (convert-exp second-op))))))
        (else (let ((first-op (first-operand e)))
                (make-not (convert-exp first-op))))))

(display "\nconvert-exp Tests\n")
(display "-----------------------------------------------------------------------\n")
(define e1 (make-or 'x 'y))
(define e2 (make-imply 'x 'z))
(define e3 (make-and 'x 'y))
(define e4 (make-not 'x))
(define e5 (make-not (make-and 'x 'y)))
(define e6 (make-not e1))
(define e7 (make-and e1 e2))
(convert-exp e1)
(convert-exp e2)
(convert-exp e3)
(convert-exp e4)
(convert-exp e5)
(convert-exp e6)
(convert-exp e7)
(display "-----------------------------------------------------------------------\n")

;;; ------------------------------------------------------------------------------------------------------------------
;;; part 2
;;; interpreter
;;;(pair? (cdr '((x #t) (y #t))))

;;;(eq? 'x (car '(x #t)))

;;;(define (tst x)
;;;  (eq? x (car '(y #t))))



(define (lookup x alist)
  (cond ((eq? (caar alist) x) (cadar alist)) ;; if x is in current pair
        ((eq? (cdr alist) '()) '()) ;; if x is not in the list
        (else (lookup x (cdr alist)))))

(define l1 '((x #f) (y #f) (z #t)))
;(lookup 'x l1)
;(lookup 'y l1)
;(lookup 'z l1)

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

(display "\nvalue Tests\n")
(display "-----------------------------------------------------------------------\n")

(value '((((x -) ^ (y -)) -) ^ ((x ^ (z -)) -)) l1)
(value '((x ^ (z -)) -) l1)

(value (convert-exp e7) l1)
(value (convert-exp e2) l1)

(display "-----------------------------------------------------------------------\n")

;;; ------------------------------------------------------------------------------------------------------------------
;;; USER TOOLS

;;; Specification: function takes in a proposition and an association list and checks if every symbol in the proposition
;;; appears in the association list. returns #t if all elements are in the assoc. list, #f otherwise

(define (aList-complete e alist)
  (cond ((symbol? e)
         (if (eq? (lookup e alist) '()) #f #t))
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (if (and (eq? (aList-complete first-op alist) #t) (eq? (aList-complete second-op alist) #t)) #t #f)))
        (else (let ((first-op (first-operand e)))
                (aList-complete first-op alist)))))

(display "\naList-complete Tests\n")
(display "-----------------------------------------------------------------------\n")

(aList-complete '((((x -) ^ (y -)) -) ^ ((x ^ (z -)) -)) l1)
(aList-complete '((((x -) ^ (p -)) -) ^ ((x ^ (z -)) -)) l1)

(aList-complete '((x ^ (z -)) -) l1)
(aList-complete '((x ^ (q -)) -) l1)

(display "-----------------------------------------------------------------------\n")
