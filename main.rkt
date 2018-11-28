#lang br/quicklang
(require brag/support "parser.rkt")


(module+ reader (provide read-syntax))
(provide specification pipe load search lookup 
         [rename-out [mb #%module-begin]]
         [struct-out node])


;; Node structure: Number List<Number>
(struct node (name childs) #:transparent)

;; Graph will be a hash-table containing all the nodes
(define graph #f)

;; Lookup : Number -> Node
(define (lookup name)
  (hash-ref graph name))

;; Set graph to be the hash-table w/ all the created nodes
(define (load pairs)
  (set! graph (make-hash pairs)))


;; Node -> List
(define (search n seen)
  (define nowseen (cons (node-name n) seen))
  (define new-nodes
    (filter-not
     (λ (c) (member c nowseen))
    (node-childs n)))
  (foldr
   (λ (next-child lst)
     (remove-duplicates
      (append (search (lookup next-child) lst) lst)))
   nowseen
   new-nodes))

;; Creats list of groups
;; List<names> -> List<List<names>>
(define (find-groups groups)
  ;; Check if a name is in a previously discovered group
  (define (in-group? name)
    (ormap (λ (g) (member name g)) groups))
  ;; Nodes that havn't been categorized yet
  (define to-search (filter-not in-group? (hash-keys graph)))
  (if (empty? to-search)
      groups ;; sorted all nodes, done
      (let* [(new-group (search (lookup (first to-search)) '()))]
        (find-groups (cons new-group groups)))))
             
  
  


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexer specification
(define-lex-abbrev digits (:+ (char-set "1234567890")))
(define l
  (lexer
   [whitespace (token 'WHITESPACE #:skip? #t)]
   [digits (token 'ID (string->number lexeme))]
   ["<->" (token 'ASSOC lexeme)]
   ["," (token 'COMMA lexeme)]))

;; builds the tokenizer
(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (λ () (l ip)))

;; read()
(define (read-syntax path port)
  (define ast (parse path (make-tokenizer port path)))
  (strip-bindings
   #`(module plumb-mod plumb/main #,ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract Syntax Tree Transformers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Each 'pipe' term in the AST corresponds to a node definition
;; This macro creates a pair of the node's name and a node structure
;; This allows it to easily be put into a hash-table
(define-macro (pipe ID CHILD ...)
  #'(cons ID (node ID (list CHILD ...))))

;; Each 'specification' term in the AST is the entire puzzle
;; Turn the NODEs into a list, then pass into the load function
(define-macro (specification NODE ...)
  #'(load (list NODE ...)))

;; This module-begin solves part 1
(define-macro (p1-mb TREE)
  #'(#%module-begin
     TREE
     (display (length (search (lookup 0) '())))))

;; This module-begin solves part 2
(define-macro (mb TREE)
  #'(#%module-begin
     TREE
     (display (length (find-groups '())))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
