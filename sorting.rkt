;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sorting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Here is an algorithm for sorting lists of numbers

  - if the list has 0 elements or 1 element,
    it is sorted; return it.

  - if the list has 2 or more elements, divide
    the list into two halves and recursively sort
    them. Note that you can divide the elements in
    half multiple ways; it does not need to be the
    first half and second half; it may even be easier
    to take the odd numbered and even-numbered elements.

  - combine the two sorted sublists into a single
    one by merging them

|#

#;
(define (list-length l1)
  (list-ref l1 (/ (length l1) 2)))
#;
(define (even? num)
  (cond
    [(= (modulo num 2) 0) #true]
    [else #false]))

(define (smaller-than x l)
  (filter (lambda (y) (< y x)) l))
(define (larger-than x l)
  (filter (lambda (y) (> y x)) l))



(define (merge-sort l1)
  (cond
    [(or (empty? l1) (empty? (rest l1))) l1]
    [else (quick-sort (append (smaller-than (second l1) l1) (list (second l1)))
                      (larger-than (second l1) l1))]))
#|
Here is an algorithm for merging the two lists:

  - if either list is empty, return the other one

  - if both are not empty, pick the list with the
    smaller first element and break it up into
    it's first element and the rest of the list.
    Recur with the entire list whose first element is
    larger and the rest of the list whose first element
    is smaller. Then cons the first element onto the
    result of the recursive call.

|#
; quick-sort: List-of-Number List-of-Number -> List-of-Number
; Merge two lists together and have them sorted
; Strategy: Structural Decomposition
(check-expect (quick-sort '() '()) '())
(check-expect (quick-sort (list 1 3 5) '()) (list 1 3 5))
(check-expect (quick-sort (list 1 3 5) (list 2 4 6)) (list 1 2 3 4 5 6))
(check-expect (quick-sort (list 1 2 3 5 6 7) (list 4)) (list 1 2 3 4 5 6 7))
(define (quick-sort l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (if (< (first l1) (first l2))
              (cons (first l1) (quick-sort (rest l1) l2))
              (cons (first l2) (quick-sort l1 (rest l2))))]))
#|

Design functions that implement this sorting algorithm.
For each function, write down if it is generative recursion
or structural recursion. Also write down the running time
of each function using Big Oh notation.

|#

