;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Add the missing ones

(struct int  (num)                 #:transparent)  ;; a constant number, e.g., (int 17)\
(struct var  (string)              #:transparent)
(struct add  (e1 e2)               #:transparent)  ;; add two expressions
(struct mult  (e1 e2)              #:transparent)
(struct neg  (e)                   #:transparent)
(struct islthan  (e1 e2)           #:transparent)
(struct ifzero  (e1 e2 e3)         #:transparent)
(struct ifgthan  (e1 e2 e3 e4)     #:transparent)
(struct mlet  (s e1 e2)            #:transparent)
(struct apair  (e1 e2)             #:transparent)
(struct first  (e)                 #:transparent)
(struct second  (e)                #:transparent)
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct munit   ()                 #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)                #:transparent) ;; if e1 is unit then 1 else 0
(struct closure (env fun)          #:transparent) ;; a closure is not in "source" programs; it is what functions evaluate to

;; Problem 1

(define (racketlist->numexlist xs)
   (cond [(null? xs)(munit)]
         [#t (apair (car xs) (racketlist->numexlist (cdr xs)))]
  ))

(define (numexlist->racketlist xs)
  (cond [(munit? xs)'()]
        [ #t (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]
        )
  )

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car(car env)) str) (cdr(car env))]
        [#t (envlookup(cdr env) str )]
        )
  )


;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env  (var-string e))]
        [(int? e)
        (int (int-num e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "NUMEX addition applied to non-number")))]
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (* (int-num v1) 
                       (int-num v2)))
               (error "NUMEX multiply applied to non-number")))]
        [(neg? e) 
         (let ([v1 (eval-under-env (neg-e e) env)])
           (if (int? v1)
               (int (- (int-num v1) ))
               (error "NUMEX negation applied to non-number")))]
        [(islthan? e) 
         (let ([v1 (eval-under-env (islthan-e1 e) env)]
               [v2 (eval-under-env (islthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if(< (int-num v1) 
                       (int-num v2)) (int 1)(int 0))
               (error "NUMEX lessthan applied to non-number")))]
        [(ifgthan? e) 
         (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
               [v2 (eval-under-env (ifgthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
              (if(> (int-num v1) 
                       (int-num v2)) (eval-under-env(ifgthan-e3 e) env) (eval-under-env(ifgthan-e4 e)env))
               (error "NUMEX greaterthan applied to non-number")))]
        [(ifzero? e) 
         ( let ([v1 (eval-under-env (ifzero-e1 e) env)])
           (if (int? v1)
               (if (equal? (int-num v1) 0) (eval-under-env(ifzero-e2 e) env) (eval-under-env(ifzero-e3 e) env))
               (error "NUMEX ifzero applied to non-number")))]
       [(call? e)
         (let([fclosure (eval-under-env (call-funexp e) env)]
              [value (eval-under-env (call-actual e) env)])
           (let ([fdeclare (closure-fun fclosure)])
           (if (closure? fclosure)
               (eval-under-env(fun-body fdeclare) (cons(cons(fun-formal fdeclare) value)(cons (cons(fun-nameopt fdeclare) fclosure)(closure-env fclosure))))
               (error "call error"))))]    
        [(fun? e)
           (closure env e)]
        [(mlet? e) 
          (let ([v1 (eval-under-env (mlet-e1 e) env)])
           (if (and (string? (mlet-s e))
                    )
               (eval-under-env (mlet-e2 e) (cons  (cons (mlet-s e) v1 ) env))
               (error "NUMEX ifzero applied to non-number")))]
        [(apair? e)
         (let (
               [v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(first? e)
         (let ([p (eval-under-env (first-e e) env)])
            (if (apair? p)
           (apair-e1 p)(error "not a pair")))]
        [(second? e)
         (let ([p (eval-under-env (second-e e) env)])
            (if (apair? p)
           (apair-e2 p)(error "not a pair")))]
        [(munit? e)
           (munit)]
        [(closure? e)
           e]
        [(ismunit? e) 
         (let ([v (eval-under-env (ismunit-e e) env)])
           (if (equal? v (munit))(int 1)(int 0)))]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
   (if(munit? e1) e2 e3))

(define (mlet* pairList finalExp)(cond [(null? pairList) (mlet "finalExpResult" (munit) finalExp)]
                                       [#t (mlet (car (car pairList)) (cdr (car pairList)) (mlet* (cdr pairList) finalExp))]))


(define (ifeq e1 e2 e3 e4)
   (let ([v1 e1]
         [v2 e2])
  (ifgthan v1 v2 e4 (ifgthan v2 v1 e4 e3))
  ))

;; Problem 4

(define numex-map (fun "final" "func" (fun "map" "list" (ifeq (ismunit (var "list")) (int 1) (munit)
                                                           (apair (call (var "func") (first (var "list"))) (call (var "map") (second(var "list"))))))))

(define numex-mapAddN
  (fun "mapAddN" "m" (call numex-map(fun null "x" (add (var "m") (var "x"))))  
   ))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([freekon? (lambda (e)
  (cond [(var? e) 
         (set (var-string e))]
        
        [(int? e)
        (set)]
        
        [(add? e) 
         (let ([fv1 (freekon? (add-e1 e))]
               [fv2 (freekon? (add-e2 e))])
           (set-union fv1 fv2))]
        
        [(mult? e) 
         (let ([fv1 (freekon? (mult-e1 e))]
               [fv2 (freekon? (mult-e2 e))])
           (set-union fv1 fv2))]
        
        [(neg? e) 
         (let ([fv1 (freekon? (neg-e e))])
           fv1)]
        
        [(islthan? e) 
         (let ([fv1 (freekon? (islthan-e1 e))]
               [fv2 (freekon? (islthan-e2 e))])
           (set-union fv1 fv2 ))]
        
        [(ifgthan? e) 
         (let ([fv1 (freekon? (ifgthan-e1 e))]
               [fv2 (freekon? (ifgthan-e2 e))]
               [fv3 (freekon? (ifgthan-e3 e))]
               [fv4 (freekon? (ifgthan-e4 e))])
           (set-union(set-union(set-union fv1 fv2) fv3) fv4))]
        
        [(ifzero? e) 
         ( let([fv1 (freekon? (ifzero-e1 e))]
               [fv2 (freekon? (ifzero-e2 e))]
               [fv3 (freekon? (ifzero-e3 e))])
            (set-union(set-union fv1 fv2) fv3))]
        
       [(call? e)
         (let([fv1 (freekon? (call-funexp e))]
              [fv2 (freekon? (call-actual e))])
            (set-union fv1 fv2))]
            
        [(fun? e)
           (let ([fv1 (freekon? (fun-body e))])
             (set-remove (set-remove fv1 (fun-formal e)) (fun-nameopt e)))]
        
        [(mlet? e) 
          (let([fv1 (freekon? (mlet-e2 e))])
            (set-remove fv1 (mlet-s e)) )]
        
        [(apair? e)
         (let([fv1 (freekon? (apair-e1 e))]
              [fv2 (freekon? (apair-e2 e))])
            (set-union fv1 fv2))]
        
        [(first? e)
         (let([fv1 (freekon? (first-e e))])
           fv1)]
        
        [(second? e)
         (let ([fv1 (freekon? (second-e e))])
           fv1)]

        [(munit? e)
           (set)]

        [(ismunit? e) 
         (let ([fv1 (freekon? (ismunit-e e))])
           fv1)]

        [#t (error (format "bad NUMEX expression: ~v" e))]))])
    
    (cond [(var? e) 
         (var (var-string e))]
          
        [(int? e)
        (int (int-num e))]
        
        [(add? e) 
         (let ([v1 (compute-free-vars (add-e1 e))]
               [v2 (compute-free-vars (add-e2 e))])
           (add v1 v2))]
        
        [(mult? e) 
         (let ([v1 (compute-free-vars (mult-e1 e))]
               [v2 (compute-free-vars (mult-e2 e))])
           (mult v1 v2))]
        
        [(neg? e) 
         (let ([v1 (compute-free-vars (neg-e e))])
          (neg v1))]
        
        [(islthan? e) 
         (let ([v1 (compute-free-vars (islthan-e1 e))]
               [v2 (compute-free-vars (islthan-e2 e))])
           (islthan v1 v2 ))]
        
        [(ifgthan? e) 
         (let ([v1 (compute-free-vars (ifgthan-e1 e))]
               [v2 (compute-free-vars (ifgthan-e2 e))]
               [v3 (compute-free-vars (ifgthan-e3 e))]
               [v4 (compute-free-vars (ifgthan-e4 e))])
           (ifgthan v1 v2 v3 v4))]
        
        [(ifzero? e) 
         ( let([v1 (compute-free-vars (ifzero-e1 e))]
               [v2 (compute-free-vars (ifzero-e2 e))]
               [v3 (compute-free-vars (ifzero-e3 e))])
            (ifzero v1 v2 v3))]
        
       [(call? e)
         (let([v1 (compute-free-vars (call-funexp e))]
              [v2 (compute-free-vars (call-actual e))])
            (call v1 v2))]
            
        [(fun? e)
           (let ([v1 (compute-free-vars (fun-body e))])
             (fun-challenge (fun-nameopt e) (fun-formal e) v1 (freekon? e)))]
        
        [(mlet? e) 
          (let([v1 (compute-free-vars (mlet-e1 e))]
               [v2 (compute-free-vars (mlet-e2 e))])
            (mlet (mlet-s e) v1 v2 ))]
        
        [(apair? e)
         (let([v1 (compute-free-vars (apair-e1 e))]
              [v2 (compute-free-vars (apair-e2 e))])
            (apair v1 v2))]
        
        [(first? e)
         (let([v1 (compute-free-vars (first-e e))])
           (first v1))]
        
        [(second? e)
         (let ([v1 (compute-free-vars (second-e e))])
           (second v1))]

        [(munit? e)
           (munit)]

        [(ismunit? e) 
         (let ([v1 (compute-free-vars (ismunit-e e))])
           (ismunit v1))])))
                     

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env  (var-string e))]
        [(int? e)
        (int (int-num e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "NUMEX addition applied to non-number")))]
        [(mult? e) 
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (* (int-num v1) 
                       (int-num v2)))
               (error "NUMEX multiply applied to non-number")))]
        [(neg? e) 
         (let ([v1 (eval-under-env-c (neg-e e) env)])
           (if (int? v1)
               (int (- (int-num v1) ))
               (error "NUMEX negation applied to non-number")))]
        [(islthan? e) 
         (let ([v1 (eval-under-env-c (islthan-e1 e) env)]
               [v2 (eval-under-env-c (islthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if(< (int-num v1) 
                       (int-num v2)) (int 1)(int 0))
               (error "NUMEX lessthan applied to non-number")))]
        [(ifgthan? e) 
         (let ([v1 (eval-under-env-c (ifgthan-e1 e) env)]
               [v2 (eval-under-env-c (ifgthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
              (if(> (int-num v1) 
                       (int-num v2)) (eval-under-env-c(ifgthan-e3 e) env) (eval-under-env-c(ifgthan-e4 e)env))
               (error "NUMEX greaterthan applied to non-number")))]
        [(ifzero? e) 
         ( let ([v1 (eval-under-env-c (ifzero-e1 e) env)])
           (if (int? v1)
               (if (equal? (int-num v1) 0) (eval-under-env-c(ifzero-e2 e) env) (eval-under-env-c(ifzero-e3 e) env))
               (error "NUMEX ifzero applied to non-number")))]
       [(call? e)
         (let([fclosure (eval-under-env-c (call-funexp e) env)]
              [value (eval-under-env-c (call-actual e) env)])
           (let ([fdeclare (closure-fun fclosure)])
           (if (closure? fclosure)
               (eval-under-env-c(fun-challenge-body fdeclare) (cons(cons(fun-challenge-formal fdeclare) value)(cons (cons(fun-challenge-nameopt fdeclare) fclosure)(closure-env fclosure))))
               (error "call error"))))]
       
        [(fun-challenge? e)
           (closure (freesaz (fun-challenge-freevars e) env) e)]
        [(mlet? e) 
          (let ([v1 (eval-under-env-c (mlet-e1 e) env)])
           (if (string? (mlet-s e))
               (eval-under-env-c (mlet-e2 e) (cons (cons (mlet-s e) v1) env))
               (error "NUMEX ifzero applied to non-number")))]
        [(apair? e)
         (let (
               [v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(first? e)
         (let ([p (eval-under-env-c (first-e e) env)])
            (if (apair? p)
           (apair-e1 p)(error "not a pair")))]
        [(second? e)
         (let ([p (eval-under-env-c (second-e e) env)])
            (if (apair? p)
           (apair-e2 p)(error "not a pair")))]
        [(munit? e)
           (munit)]
        [(ismunit? e) 
         (let ([v (eval-under-env-c (ismunit-e e) env)])
           (if (equal? v (munit))(int 1)(int 0)))]
        [#t (error (format "bad NUMEX expression: ~v" e))]))


;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

(define (freesaz freeset env)
  (if (set-empty? freeset)
   '()
   (cons (cons (set-first freeset) (envlookup env (set-first freeset))) (freesaz (set-rest freeset) env))))
       
