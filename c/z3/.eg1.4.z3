(declare-sort Loc)
(declare-sort Rec)
(declare-sort St)
(declare-fun value (St Loc) Rec)
(define-sort Set () (Array Loc Bool))
;; Let's try the approach of defining domain using a function.
(declare-fun dom (St) Set)
;; Set functions
(declare-const empty Set)
(assert (= empty ((as const Set) false)))
(define-fun in ((l Loc)(s Set)) Bool
  (= (select s l) true))
(define-fun singleton ((l Loc)) Set
  (store empty l true))
(define-fun remove ((s Set)(l Loc)) Set
  (store s l false))
(define-fun add ((s Set)(l Loc)) Set
  (store s l true))
(declare-sort Id)
(declare-sort Str)
;; Declare record attributes
(declare-fun id (Rec) Id)
(declare-fun cbal (Rec) Int)
(declare-fun sbal (Rec) Int)
;; ------ Basic Axioms ---------
;; 1. No two locations point to the same record in any state. 
;; 2. Id is a primary key, hence unique.
(assert (forall ((st St))
          (forall ((l1 Loc)(l2 Loc)) 
            (=> (and (in l1 (dom st)) 
                     (in l2 (dom st))
                     (= (id (value st l1)) 
                        (id (value st l2)))) 
                (and (= l1 l2) )))))
;; Define st2 = st1[l -> r] 
(define-fun bind ((st2 St) (st1 St) (l Loc) (r Rec)) Bool
  (and (= (dom st2) (add (dom st1) l))
       (= (value st2 l) r)
       (forall ((l1 Loc)) 
          (=> (and (not (= l1 l)) (in l1 (dom st1))) 
              (and (in l1 (dom st2)) 
                   (= (value st2 l1) (value st1 l1)))))))
;; Define I
(define-fun I ((st St)) Bool
  (forall ((l Loc)) 
    (=> (in l (dom st))
        (and (>= (cbal (value st l)) 0)
             (>= (sbal (value st l)) 0)))))
;; Define G1
(declare-fun G1 (St St) Bool)
(assert (forall ((st1 St)(st2 St)) 
          (= (G1 st1 st2)
             (exists ((l Loc)(r1 Rec)(r2 Rec)) 
                (and (in l (dom st1)) 
                     (in l (dom st2))
                     (= (value st1 l) r1)
                     (= (id r2) (id r1))
                     (= (sbal r2) (sbal r1)) 
                     (>= (cbal r2) 0)
                     (bind st2 st1 l r2))))))
;; G2 is same as G1.
;; Define G3
(declare-fun G3 (St St) Bool)
(assert (forall ((st1 St)(st2 St)) 
          (= (G3 st1 st2) 
             (exists ((l Loc)(r1 Rec)(r2 Rec)) 
                (and (in l (dom st1)) 
                     (in l (dom st2))
                     (= (value st1 l) r1)
                     (= (id r2) (id r1))
                     (>= (sbal r2) 0) 
                     (>= (cbal r2) 0)
                     (bind st2 st1 l r2) )))))
;; Define R = G1 ∪ G2 ∪ G3
(declare-fun R (St St) Bool)
(assert (forall ((st1 St)(st2 St)) 
          (= (R st1 st2) 
             (or (G1 st1 st2) (G3 st1 st2)))))
;; Define Ir for SI 
(define-fun Ir ((stl St)(stg1 St)(stg2 St)) Bool
  (= stg1 stg2))
;; Define Ic for SI
(define-fun Ic ((stl St)(stg1 St)(stg2 St)) Bool
  (forall ((l Loc)) 
    (=> (and (in l (dom stl)) (in l (dom stg1)))
        (and (in l (dom stg2)) 
             (= (value stg1 l) (value stg2 l)) )))) 
;; Define Rl
(define-fun Rl ((stl St)(stg1 St)(stg2 St)) Bool
  (and (Ir stl stg1 stg2) (R stg1 stg2)))
;; Define Rc
(define-fun Rc ((stl St)(stg1 St)(stg2 St)) Bool
  (and (Ic stl stg1 stg2) (R stg1 stg2)))
;; Define σl>>σg
(declare-fun flush (St St) St)
(assert (forall ((stl St)(stg St))
          (= (dom (flush stl stg)) 
             (union (dom stl) (dom stg)))))
(assert (forall ((stl St)(stg St)) 
          (forall ((l Loc)) 
            (ite (in l (dom stl)) 
                  (= (value (flush stl stg) l) 
                     (value stl l)) 
                  (ite (in l (dom stg)) 
                       (= (value (flush stl stg) l) 
                          (value stg l))
                       true)))))
;; Define Q(stl,stg)
(define-fun Q ((stl St)(stg St)) Bool
  (G1 stl (flush stl stg)))
(declare-const stableQ Bool)
(assert (= stableQ
           (forall ((stl St)(stg1 St)(stg2 St)) 
              (=> (and (Q stl stg1) (Rc stl stg1 stg2)) 
                  (Q stl stg2)))))
;; Checking the stability of Q
;; (assert (not stableQ))

;; Define what is means to update user balance for user whose id is 
;; u_id
(declare-fun updated (St St St) Bool)
(assert (forall ((stg St) (st1 St)(st2 St)) 
          (=> (updated stg st1 st2)
              (= (dom st1) (dom st2)))))
; (assert (forall ((stg St) (st1 St)(st2 St)) 
;           (=> (updated stg st1 st2)
;               (forall ((l Loc)) 
;                 (ite (and (in l (dom (flush st1 stg))) 
;                           (= (id (select (flush st1 stg) l)) u_id))
;                      (and (in l (dom st2)) 
;                           (exists ((r Rec)) 
;                              (and (= (id r) u_id)
;                                   (= (sbal r) (sbal (select (flush st1 stg) l)))
;                                   (= (cbal r) (+ (cbal user) amt)) 
;                                   (= (select st2 l) r))))
;                      (ite (in l (dom st1)) 
;                           (and (in l (dom st2))
;                                (= (select st2 l) (select st1 l))) 
;                           (not (in l (dom st2)))))))))

;; Define Q1
(define-fun Q1 ((stl St)(stg St)) Bool
  (forall ((stl1 St )) 
    (=> (updated stg stl stl1) (Q stl1 stg))))
;; TIMES OUT
(check-sat)
(get-model)
