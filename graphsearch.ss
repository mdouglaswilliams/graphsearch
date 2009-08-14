;; Graph Search
;; M. Douglas Williams

(module graphsearch mzscheme
  
  (provide
   define-ruleset
   define-rule
   graph-search
   graph-statistics)
  
  ;; Ruleset
  
  (define-struct ruleset (name rules))
  
  ;; A rulesset is a structure (make-struct n l) where n is a synbol and
  ;; l is the empty list '().
  ;;
  ;; Slots:
  ;;  name         name of the rule set (symbol)
  ;;  rules        list of rules
  
  ;; name->ruleset: symbol ->  ruleset
  ;; name->ruleset: symbol x ruleset
  ;; get or set the ruleset with a given name.  A hash-table associating
  ;; the symbol naming the ruleset to the ruleset object representing it.
  
  (define name->ruleset
    (let ((hash-table (make-hash-table)))
      (case-lambda
        ((name)
         (hash-table-get hash-table name))
        ((name ruleset)
         (hash-table-put! hash-table name ruleset)
         (void)))))
  
  ;; define-ruleset: (define-ruleset name)
  ;; defines a ruleset with the given name.
  
  (define-syntax define-ruleset
    (syntax-rules ()
      ((_ name)
       (let ((ruleset (make-ruleset 'name '())))
         (name->ruleset 'name ruleset)
         (void)))))
  
  ;; Rule
  
  (define-struct rule (name precondition action))
  
  ;; A rule is a structure (make-rule n p a) where n is a symbol and p
  ;; and a are functions of one argument.
  ;;
  ;; Slot:
  ;;  name         symbol naming the rule
  ;;  precondition function representing the precondition
  ;;  action       function representing the action
  
  ;; define-rule: (define-rule (rule-name rule-set-name) arguments
  ;;                precondition ==> action)
  ;; defines a rule with the given name in the rule-set with the given
  ;; rule-setname.  The rule has the precondition and action procedures
  ;; specified with the given arguments.
  
  (define-syntax define-rule
    (syntax-rules (==>)
      ((_ (rule-name ruleset-name) arguments
          precondition
          ==>
          action)
       (let ((ruleset (name->ruleset 'ruleset-name))
             (rule (make-rule 'rule-name
                              (lambda arguments precondition)
                              (lambda arguments action))))
         (set-ruleset-rules! ruleset
                             (append (ruleset-rules ruleset)
                                     (list rule)))
         (void)))))
  
  ;; applicable?: rule x any -> boolean
  ;; returns #t if the rule is applicable to the given state and #f
  ;; otherwise.
  
  (define (applicable? rule state)
    ((rule-precondition rule) state))
  
  ;; action: rule x any -> any
  ;; returns the state resulting from applying the rule to the given
  ;; state.
  
  (define (action rule state)
    ((rule-action rule) state))
  
  ;;; Node
  
  (define-struct node (state cost value status parent successors))
  
  ;; A node is a structure (make-node s c v st p l) where s is a state;
  ;; c and v are (non-negative) numbers; s is one of #f, 'open, or
  ;; 'closed; p is a node or #f; and l is the empty list '().
  ;;
  ;; Slots:
  ;;  state        the state for this node
  ;;  cost         the lowest known cost to this node
  ;;  value        the (cached) heuristic value for this node
  ;;  status       the status of this node:
  ;;                 #f     new node
  ;;                 open   node has not been expanded
  ;;                 closed node has been expanded
  ;;  parent       the parent of this node in the search tree (or #f)
  ;;  successors   list of successors of this node in the search graph
  
  ;; *nodes*: list
  ;; is a list of the nodes in the graph.
  
  (define *nodes* '())
  
  ;; find-node: state -> node or #f
  ;; returns the existing node for the given state or #f.
  
  (define (find-node state)
    (let loop ((nodes *nodes*))
      (if (null? nodes)
          #f
          (let ((node (car nodes)))
            (if (equal? (node-state node) state)
                node
                (loop (cdr nodes)))))))
  
  ;; get-node: state -> node
  ;; returns the node for the given state, creating a new one if
  ;; necessary.
  
  (define (get-node state)
    (let ((node (find-node state)))
      (when (not node)
        (set! node (make-node state 0 0 #f #f '()))
        (set! *nodes* (cons node *nodes*)))
      node))
  
  ;; get-successors: node x ruleset -> list of nodes
  ;; returns a list of the nodes that are successors of the given node
  ;; using the specified ruleset.
  
  (define (get-successors node ruleset)
    (let ((state (node-state node)))
      (let loop ((rules (ruleset-rules ruleset)))
        (if (null? rules)
            '()
            (let ((rule (car rules)))
              (if (applicable? rule state)
                  (cons (get-node (action rule state))
                        (loop (cdr rules)))
                  (loop (cdr rules))))))))
  
  ;; ancestor-states: node -> list of any
  ;; returns a list of the ancestor states of the node (including the
  ;; node itself) in the search tree.
  
  (define (ancestor-states node)
    (if (not node)
        '()
        (append (ancestor-states (node-parent node))
                 (list (node-state node)))))
  
  ;; merge-item: any x list x function -> list
  ;; returns a list with the item added in order according to the given
  ;; predicate function.
  
  (define (merge-item item sorted-list pred)
    (cond ((null? sorted-list)
           (cons item '()))
          ((pred item (car sorted-list))
           (cons item sorted-list))
          (else
           (cons (car item)
                 (merge-item item (cdr sorted-list) pred)))))
  
  ;; sort!: list x function -> list
  ;; returns the list with its items in order according to the given
  ;; predicate function.
  ;;
  ;; Example:
  ;;  (sort '(6 4 1 7 3 8 7 9 2 5) <) -> (1 2 3 4 5 6 7 7 8 9)
  
  (define (sort! list pred)
    (if (null? list)
        '()
        (merge-item (car list) (cdr list) pred)))
  
  ;; merge!: list x list x function -> list
  ;; returns a list with the items of the two sorted lists in order
  ;; according to the given predicate function.
  ;;
  ;; Example:
  ;;   (merge! '(1 3 5 7) '(2 4 6 7 8 9) <) -> (1 2 3 4 5 6 7 7 8 9)
  
;  (define (merge! sorted-list1 sorted-list2 pred)
;    (cond ((null? sorted-list1)
;           sorted-list2)
;          ((null? sorted-list2)
;           sorted-list1)
;          (else
;           (if (pred (car sorted-list1) (car sorted-list2))
;               (begin
;                 (set-cdr! sorted-list1
;                           (merge! (cdr sorted-list1) sorted-list2 pred))
;                 sorted-list1)
;               (begin
;                 (set-cdr! sorted-list2
;                           (merge! sorted-list1 (cdr sorted-list2) pred))
;                 sorted-list2)))))
  
  (define (merge sorted-list1 sorted-list2 pred)
    (cond ((null? sorted-list1)
           sorted-list2)
          ((null? sorted-list2)
           sorted-list1)
          (else
           (if (pred (car sorted-list1) (car sorted-list2))
               (cons (car sorted-list1) (merge (cdr sorted-list1) sorted-list2 pred))
               (cons (car sorted-list2) (merge sorted-list1 (cdr sorted-list2) pred))))))
  
  ;; graph-search: any x function x symbol x function x boolean x symbol
  ;;               -> void
  ;; returns a list of states from the initial state to a goal state using
  ;; the specified rule set.  The search is guided by the given heuristic
  ;; function (value) as well as the given sort and merge methods.  The
  ;; nodes in the search graph are retained in *nodes* for analysis.
  
  (define (graph-search initial-state goal? ruleset-name
                        value sort-new-nodes? merge-method)
    (define (check-new-path node successor)
      (let ((new-cost (add1 (node-cost node))))
        (when (< new-cost (node-cost successor))
          (set-node-parent! successor node)
          (set-node-cost! successor new-cost)
          (set-node-value! successor
                           (value (node-state successor)
                                  (node-cost successor)))
          (when (eq? (node-status successor) 'closed)
            (for-each
             (lambda (successor-successor)
               (check-new-path successor successor-successor))
             (node-successors successor))))))
    (set! *nodes* '())
    (call/cc
     (lambda (return)
       (let ((ruleset (name->ruleset ruleset-name))
             (s (get-node initial-state))
             (open '()))
         ;; Add s as the only member of OPEN.
         (set! open (list s))
         (set-node-status! s 'open)
         ;; Recursive internal procedure to iterate over the nodes on
         ;; OPEN.
         (let loop ()
           (if (null? open)
               (return #f))
           (let ((n #f)
                 (successors '())
                 (new-nodes '()))
             ;; Remove the first node from OPEN and assign it to n.
             (set! n (car open))
             (set! open (cdr open))
             ;; Move node n to closed.
             (set-node-status! n 'closed)
             ;; If n is a goal state, exit with success.
             (if (goal? (node-state n))
                 (return (ancestor-states n)))
             ;; Generate the successor nodes of n.
             (set! successors (get-successors n ruleset))
             (set-node-successors! n successors)
             ;; Process each successor.
             (for-each
              (lambda (successor)
                (if (not (node-status successor))
                    (begin
                      (set-node-status! successor 'open)
                      (set-node-parent! successor n)
                      (set-node-cost! successor (add1 (node-cost n)))
                      (set-node-value! successor
                                       (value (node-state successor)
                                              (node-cost successor)))
                      (set! new-nodes
                            (append new-nodes (list successor))))
                    (check-new-path n successor)))
              successors)
             ;;
             (if sort-new-nodes?
                 (set! new-nodes
                       (sort! new-nodes 
                              (lambda (n1 n2)
                                (<= (node-value n1) (node-value n2))))))
             (case merge-method
               ((prepend)
                (set! open (append new-nodes open)))
               ((append)
                (set! open (append open new-nodes)))
               ((merge)
                (set! open
                      (merge new-nodes open
                             (lambda (n1 n2)
                               (<= (node-value n1) (node-value n2)))))))
             (loop)))))))
  
  ;; graph-statistics: void
  ;; prints some simple statistics on the search graph generates by a
  ;; previous graph-search call.
  
  (define (graph-statistics)
    (let ((n-nodes 0) 
          (n-closed 0) )
      (for-each  
       (lambda (node)
         (set! n-nodes (add1 n-nodes))
         (when (eq? (node-status node) 'closed)
           (set! n-closed (add1 n-closed))))
       *nodes*)
      (printf "Total nodes generated: ~a~n" n-nodes)
      (printf "Nodes expanded (including the goal): ~a~n" n-closed)
      (void)))
  
)