;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sorting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
Design functions that implement this sorting algorithm.
For each function, write down if it is generative recursion
or structural recursion. Also write down the running time
of each function using Big Oh notation.
|#

;; elements_odd: List-of-Number -> List-of-Odd-Number
;; Picks the odd-positioned numbers in the list
(check-expect (elements_odd '()) '())
(check-expect (elements_odd (list 1)) (list 1))
(check-expect (elements_odd (list 2 4 6 8 10 12)) (list 2 6 10))

;; Strategy: Structural Decomposition
(define (elements_odd L1)
  (cond
    [(empty? L1)         '()]
    [(empty? (rest L1))  (list (first L1))]
    [else
     (cons (first L1) (elements_odd (rest (rest L1))))]))



; An [Two-at-a-Time-List-of X] is one of:
;  - '()
;  - (cons X '())
;  - (cons X (cons X [Two-at-a-Time-List-of X]))

;; elements_even: List-of-Number -> List-of-Even-Numbers
;; Picks the even numbers in the list
(check-expect (elements_even '()) '())
(check-expect (elements_even (list 2)) '())
(check-expect (elements_even (list 1 2 3 4 5 6)) (list 2 4 6))
;; Strategy: Structural Decomposition
(define (elements_even L1)
  (if (empty? L1) '()
      (if (empty? (rest L1)) '()
          (cons (first(rest L1))
                (elements_even (rest (rest L1)))))))

;; [List of Natural] -> [List-of-Odd-Natural] [List-of-Even-Natural]
;; Divides a list into two:
;; 1.odd numbered list
;; 2.even numbered list
(check-expect( divide '(1 2 3 4 5 6 7 8 9)) '((1 3 5 7 9)(2 4 6 8)))
;; Strategy: Function Composition
(define (divide L1)
  (cons (elements_odd L1) (cons (elements_even L1) `())))

;; List List->List
;; Concatanates two different lists.
;; Examples:
(check-expect (list_concat '() '()) '())
(check-expect (list_concat '(1 2 3) '()) '(1 2 3))
(check-expect (list_concat '() '(1 2 3)) '(1 2 3))
(check-expect (list_concat '(1 2 3) '(6 7 8) ) '(1 2 3 6 7 8))
(check-expect (list_concat '(1 2 3) '(4 5)) '(1 2 3 4 5))
;;stategy: structural decomposition
#| template :
(define list_concat L1 L2)
        (... (empty L1) L2...
        ... (empty? L2) L1...
        ... (L1) L2...)
|#
(define (list_concat L1 L2)
  (if (empty? L1) L2
      (if (empty? L2) L1
          (if (< (first L1) (first L2))
              (cons (first L1) (list_concat (rest L1) L2))
              (cons (first L2) (list_concat (rest L2) L1))))))

;; List of Natural-> List of Natural
;; Sorts the given elements in ascending order
;; Example:
(check-expect (sort_fn '()) '())
;; list with odd number of elements
(check-expect ( sort_fn '( 5 4 3 2 1 9 7 6 8))'(1 2 3 4 5 6 7 8 9))
;; list with even number of elements
(check-expect ( sort_fn '( 2 3 1 5 4 7 8 6))'(1 2 3 4 5 6 7 8 ))
;; Strategy: Function Composition
(define (sort_fn L1)
  (if (empty? L1) L1
      (if (empty? (rest L1)) L1
          (list_concat
           (sort_fn (elements_even L1))
           (sort_fn (elements_odd L1))))))
