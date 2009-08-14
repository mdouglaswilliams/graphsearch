#lang scheme/base
;; Towers of Hanoi Representation
;;
;; Each state in the Towers of Hanoi problem is represented as a list 
;; of three sublists.  Each sublist represents the disks on the 
;; corresponding needle in the state, For example:
;;
;;        |        |        |    
;;       =|=       |        |    
;;      ==|==      |        |    
;;     ===|===     |        |    
;;   -----------------------------
;;
;; is rerepresented as
;;
;;   ((1 2 3)()())
;;
;; We define the following Scheme functions to construct and manipulate
;; these states:
;;
;; new-towers-state  constructs a new state given the contents of the
;;                   three needles; the same as the function list
;; left              returns the contents of the left needle; the same
;;                   as the function car
;; middle            returns the contents of the middle needle; the
;;                   same as the function cadr
;; right             returns the contents of the right needle: the 
;;                   same as the function caddr
;; stack             returns a new stack with the given disk on top of
;;                   a stack of disks; the same as the function cons
;; rest              returns the rest of the disks after the top on on
;;                   stack of disks; the same as the function cdr
;; empty?            predicate function that returns #t if a stack of
;;                   disks is empty; the same as the function null?
;; smaller?          predicate function that returns #t if the first
;;                   disk is smaller than the second disk; the same as
;;                   the function <

;(require (planet "graphsearch.ss" ("williams" "graphsearch.plt" 1 0)))
(require "../graphsearch.ss")

(define new-towers-state list)
(define left car)
(define middle cadr)
(define right caddr)
(define stack cons)
(define top car)
(define rest cdr)
(define empty? null?)
(define smaller? <)

;; Rule set
;;
;; These are the rule to generate the search graph for the Towers of
;; Hnoi problem.  There are six rules corresponding to legal moves.
;;
;;   move-left-to-middle
;;   move-left-to-right
;;   move-middle-to-left
;;   move-middle-to-right
;;   move-right-to-left
;;   move-right-to-middle
;;
;; The preconditions for each of these rules prevents an application of
;; the rules that would produce illegal states.

(define-ruleset towers-rules)

;;; Move the top disk from the left needle to the middle needle
(define-rule (move-left-to-middle towers-rules) (state)
  (and (not (empty? (left state)))
       (or (empty? (middle state))
           (smaller? (top (left state))
                     (top (middle state)))))
  ==>
  (new-towers-state
   (rest (left state))
   (stack (top (left state)) (middle state))
   (right state)))

;;; Move the top disk from the left needle to the right needle
(define-rule (move-left-to-right towers-rules) (state)
  (and (not (empty? (left state)))
       (or (empty? (right state))
           (smaller? (top (left state))
                     (top (right state)))))
  ==>
  (new-towers-state
   (rest (left state))
   (middle state)
   (stack (top (left state)) (right state))))

;;; Move the top disk from the middle needle to the left needle
(define-rule (move-middle-to-left towers-rules) (state)
  (and (not (empty? (middle state)))
       (or (empty? (left state))
           (smaller? (top (middle state))
                     (top (left state)))))
  ==>
  (new-towers-state
   (stack (top (middle state)) (left state))
   (rest (middle state))
   (right state)))

;;; Move the top disk from the middle needle to the right needle
(define-rule (move-middle-to-right towers-rules) (state)
  (and (not (empty? (middle state)))
       (or (empty? (right state))
           (smaller? (top (middle state))
                     (top (right state)))))
  ==>
  (new-towers-state
   (left state)
   (rest (middle state))
   (stack (top (middle state)) (right state))))

;;; Move the top disk from the right needle to the left needle
(define-rule (move-right-to-left towers-rules) (state)
  (and (not (empty? (right state)))
       (or (empty? (left state))
           (smaller? (top (right state))
                     (top (left state)))))
  ==>
  (new-towers-state
   (stack (top (right state)) (left state))
   (middle state)
   (rest (right state))))

;;; Move the top disk from the right needle to the middle needle
(define-rule (move-right-to-middle towers-rules) (state)
  (and (not (empty? (right state)))
       (or (empty? (middle state))
           (smaller? (top (right state))
                     (top (middle state)))))
  ==>
  (new-towers-state
   (left state)
   (stack (top (right state)) (middle state))
   (rest (right state))))

;; Top-level Search Function
;;
;; This is the top-level search routine for the Towers of Hanoi
;; problem.  It calls the graph-search procedure with the appropriate
;; arguments, prints the states in the solution, and prints the
;; statistics for the search graph.
(define (towers-search initial-state goal-state merge-method)
  (define (towers-value state cost)
    cost)
  (define (goal? state)
    (equal? state goal-state))
  (let ((solution 
         (graph-search initial-state goal? 'towers-rules
                       towers-value #t merge-method)))
    (if solution
        (begin
          (printf "Solution:~n")
          (for-each
           (lambda (state)
             (printf "~a~n" state))
           solution))
        (printf "No solution found~n")))
  (graph-statistics))

(towers-search '((1 2 3 4)()()) '(()()(1 2 3 4)) 'prepend)

(towers-search '((1 2 3 4)()()) '(()()(1 2 3 4)) 'append)