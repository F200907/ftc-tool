(set-logic ALL)
(set-option :produce-unsat-cores true) ; enable generation of unsat cores

(declare-datatypes ((State 0)) (((mk-state (x Int)))))
(define-fun id ((s1 State) (s2 State)) Bool (and (= (x s1) (x s2))))
(define-fun sb_x ((s1 State) (s2 State) (val Int)) Bool (and (= (x s2) val)))

(declare-const state1 State)
(declare-const state2 State)
(declare-const state3 State)
(declare-const state4 State)

(assert (not (=> (>= (x state1) 0) (=> (id state1 state2) (and (=> (not (or (< (x state1) 0) (= (x state1) 0))) (=> (sb_x state2 state3 (- (x state2) 1)) (=> (and (not (< (x state3) 0)) (= (x state4) 0)) (= (x state4) 0)))) (=> (not (not (or (< (x state1) 0) (= (x state1) 0)))) (=> (id state2 state3) (= (x state3) 0))))))))

; (assert (>= (x state1) 0))

; (assert (not (=> (id state1 state2) (and (=> (not (or (< (x state2) 0) (= (x state2) 0))) (=> (sb_x state2 state3 (- (x state2) 1)) (=> (and (not (< (x state3) 0)) (= (x state3) 0)) (= (x state3) 0)))) (=> (not (not (or (< (x state2) 0) (= (x state2) 0)))) (=> (id state2 state3) (= (x state3) 0)))))))

(check-sat)
(get-model)
