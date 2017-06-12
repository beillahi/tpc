(declare-sort RecId)
(define-sort St () (Array RecId Bool))
;; Set functions
(declare-const empty St)
(assert (= empty ((as const St) false)))
(define-fun in ((r RecId)(st St)) Bool
  (= (select st r) true))
(define-fun singleton ((r RecId)) St
  (store empty r true))
(define-fun remove ((s St)(r RecId)) St
  (store s r false))
(define-fun add ((s St)(r RecId)) St
  (store s r true))
(declare-sort Id)
(declare-sort Str)
;; Declare record attributes
(declare-fun id (RecId) Id)
;(declare-fun name (RecId) Str)
(declare-fun cbal (RecId) Int)
(declare-fun sbal (RecId) Int)
;; Id is declared as a primary key, hence its uniqueness is guaranteed
;; by the system.
(assert (forall ((st St))
          (forall ((r1 RecId)(r2 RecId)) 
            (=> (and (in r1 st) (in r2 st) (= (id r1) (id r2))) 
                (= r1 r2)))))
;; Define I
(define-fun I ((st St)) Bool
  (forall ((r RecId)) 
    (=> (in r st)
        (and (>= (cbal r) 0)
             (>= (sbal r) 0)))))
;; ∃(ρ,ρ'). id(ρ')=id(ρ) ∧ sbal(ρ')=sbal(ρ) ∧ cbal(ρ')≥0 
;;            ∧ σ' = (σ - {ρ}) ∪ {ρ'}
;; Define G1
(declare-fun G1 (St St) Bool)
(assert (forall ((st1 St)(st2 St)) 
          (= (G1 st1 st2) 
             (exists ((r1 RecId)(r2 RecId)) 
                (and (= (id r2) (id r1))
                     (= (sbal r2) (sbal r1)) 
                     (>= (cbal r2) 0)
                     (= st2 (add (remove st1 r1) r2)) )))))
;; G2 is same as G1.
;; Define G3
(declare-fun G3 (St St) Bool)
(assert (forall ((st1 St)(st2 St)) 
          (= (G3 st1 st2) 
             (exists ((r1 RecId)(r2 RecId)) 
                (and (= (id r2) (id r1))
                     (>= (sbal r2) 0) 
                     (>= (cbal r2) 0)
                     (= st2 (add (remove st1 r1) r2)) )))))
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
  (forall ((r RecId)) 
    (=> (and (in r stl) (in r stg1))
        (in r stg2)))) 
;; Define Rl
(define-fun Rl ((stl St)(stg1 St)(stg2 St)) Bool
  (and (Ir stl stg1 stg2) (R stg1 stg2)))
;; Define Rc
(define-fun Rc ((stl St)(stg1 St)(stg2 St)) Bool
  (and (Ic stl stg1 stg2) (R stg1 stg2)))
;; Unfortunately, with the current state-as-set encoding, we cannot
;; define the flush operation (σl>>σg). This is because if ρ∈σl and
;; ρ∈σg, then is ρ deleted or modified? We cannot mark
;; isDelete(ρ)=true, because this goes against the axiom that if ρ∈σg,
;; then  isDelete(ρ)=false. Even if we assume only updates, the flush
;; operation needs to define new RecIds to denote the updated version
;; of the old records.


(check-sat)
(get-model)
