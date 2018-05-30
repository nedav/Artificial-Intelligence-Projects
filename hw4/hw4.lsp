; Overall Solution: Use DPLL to find a solution. DPLL works by repeatedly assigning a variable in delta to either true or false 
;					and then performing resolution on delta. The resolution is in two parts. First, any clauses containing literals
;					in which the assigned variable is true are removed; if one literal of a disjunction is true, the entire disjunction will be true.
;					Second, any occurrences of literals in which the assigned variable is false are removed; if one literal of a disjunction
;					is false, the literal will have no affect on the truth value of the disjunction overall.
;					DPLL determines whether delta is satisfiable with a certain assignment based on the results of this repeated resolution. 
;					If at any point delta contains an empty clause, the assignment must not be a model because the variable assignments
;					do not satisfy the clause (if it were satisfied at any point, the clause would have been removed). If all possible
;					assignments are unsatisfiable, delta is unsatisfiable.
;					If at any point delta is empty, it must be satisfiable because all clauses have been satisfied and thus removed.
; Given an integer n and a CNF delta, return a list of n integers representing a model of delta if delta is satisfiable 
; and nil otherwise.
(defun sat? (n delta)
	(dpll delta nil (order-vars n)))

; Given an integer n, return a list (1 ... n).
(defun order-vars (n)
	(cond ((= n 1) (list 1))
		  (t (append (order-vars (- n 1)) (list n)))))

; Given a CNF delta, an assignment assign, and an order of variables order, return a model if delta is satisfiable and nil otherwise by
; using DPLL.
; Solution: If there are no clauses in delta, return the assignment. If there are any unassigned variables that weren't assigned
;			due to being removed, assign them to be true
;			If there is a clause in delta that is empty, return false as this indicates that there is no solution
; 			Get the next variable from the list. Try assigning it to true and recursing on it with the result of resolution of delta. 
; 			If this fails, try assigning it to false and recurse on it with the result of resolution of delta.
(defun dpll (delta assign order)
	(cond ((null delta) (append assign order))
		  ((not (no-nils delta)) nil)
		  (t (let* ((next-var (first order)))
		  		(or (dpll (eval-cnf delta next-var) (append assign (list next-var)) (rest order))
		  			(dpll (eval-cnf delta (- next-var)) (append assign (list (- next-var))) (rest order)))))))

; Given a CNF delta, return t if all elements are not nil, otherwise return nil.
(defun no-nils (delta)
	(cond ((null delta) t)
		  ((null (first delta)) nil)
		  (t (no-nils (rest delta)))))

; Given a CNF delta and a literal, return the result of removing any clauses of delta containing the unnegated literal and 
; removing the negated literal from any clauses it occurs in.
; Solution: If the literal is in the first clause, remove the clause and check the rest of the clauses.
;			Otherwise, remove any negations of the literal in the first clause and check the rest of the clauses.
(defun eval-cnf (delta literal)
	(cond ((null delta) nil)
		  (t (if (find-literal (first delta) literal)
		  	     (eval-cnf (rest delta) literal)
		  		 (cons (find-negation-literal (first delta) literal) (eval-cnf (rest delta) literal))))))

; Given a clause and a literal, return t if the literal is found in the clause and nil otherwise.
; Solution: Keep on recursing until either the literal is found or the end of the clause is reached, in which case return nil as no
;			literal has been found.
(defun find-literal (clause literal)
	(cond ((null clause) nil)
		  ((= (first clause) literal) t)
		  (t (find-literal (rest clause) literal))))

; Given a clause and a literal, return the clause with any occurences of the negated literal deleted.
; Solution: If the negation of the literal is in the first clause, remove it from the clause and check the rest of the clause.
;			Otherwise, don't change anything.
(defun find-negation-literal (clause literal)
	(cond ((null clause) nil)
		  ((= (first clause) (- literal)) (find-negation-literal (rest clause) literal))
		  (t (cons (first clause) (find-negation-literal (rest clause) literal)))))