; Problem 1
; Given a tree, the function returns a list of the terminal nodes of the tree in the order they would be visited by a left-to-right depth-first search
; Solution: If the tree is nil, return nil. This case is used for when the recursive call (DFS rest tree) is nil.
;			If the tree is a single atom, simply return it in a list
;			Otherwise the tree is a list of children nodes. First recurse on the left-most child (the first element of the tree) to get the
;			deepest, left-most node, and append it to the result of recursing on the rest of the tree.
(defun DFS (TREE)
	(cond ((null TREE) nil)
		  ((atom TREE) (list TREE))
		  (t (append (DFS (first TREE)) (DFS (rest TREE))))))

; Problem 2
; Test cases I came up with:
;

; Given a tree and the maximum depth of the tree, the function returns a list of the terminal nodes of the tree in the order 
; they would be visited by a left-to-right depth-first iterative deepening search
; Solution: Check whether the depth is 0. If it is, get the result of doing a depth-first limited search of depth 0 on the tree.
;			Otherwise, append the results of doing depth-first limited searchs of depths 0 to D-1 to the result of 
;			doing a depth-first limited search of depth D.
(defun DFID (TREE D)
	(cond ((= D 0) (DFL TREE 0))
		  (t (append (DFID TREE (- D 1)) (DFL TREE D)))))

; Given a tree and a maximum depth of the tree, the function returns a list of terminal nodes of the tree in the order
; they would be visited by a left-to-right limited depth-first search
; Solution: Check whether the tree is empty. If so, return nil.
;			Check whether the tree is a single atom. If so, there are no other nodes to visit, so return it in a list
;			Check whether the depth is 0. If so, because we already checked if the tree is a single atom, there are no nodes to return,
;			so return nil.
;			Otherwise, append the results of doing a limited DFS search up until a depth of D-1 on the first child to the results of doing a 
;			limited DFS on the rest of the children. 
(defun DFL (TREE D)
		(cond ((null TREE) nil)
			  ((atom TREE) (list TREE))
			  ((= D 0) nil)
			  (t (append (DFL (first TREE) (- D 1)) (DFL (rest TREE) D)))))

; Problem 3
; Given a state S, returns t if S is equal to (3 3 nil) and nil otherwise. 
(defun final-state (s)
	(equal s (list 3 3 nil)))

; Given a state S, a number of missionaries to move M, and a number of cannibals to move C, the function returns 
; the resulting state of applying the action to the state or nil, if the action cannot be applied.
; Solution: Check whether the action results in a greater number of cannibals than the number of missionaries (which must be nonzero) 
;			on the state's side of the river; if so, return nil.
;			Check whether the action results in a greater number of cannibals than the number of missionaries (which must be nonzero)
;			on the other side of the river; if so, return nil
;			Check whether the number of missionaries/cannibals exceeds the number in the state; if so, return nil
;			Otherwise, the resulting state is the m missionaries plus the (3 - s_m) missionaries on the other side of the river
; 			and the c cannibals plus the (3 - s_c) cannibals on the other side of the river.
(defun next-state (s m c)
	(let* ((s_m (first s)) (s_c (second s)) (side (third s)))
		(cond ((and (> (- s_c c) (- s_m m)) (not (= (- s_m m) 0))) nil)
		  	  ((and (> (+ c (- 3 s_c)) (+ m (- 3 s_m))) (not (= (+ m (- 3 s_m)) 0))) nil)
			  ((or (> m s_m) (> c s_c)) nil)
			  (t (list (list (+ m (- 3 s_m)) (+ c (- 3 s_c)) (not side)))))))

; Given a state S, the function returns a list of all the states that can be reached by applying legal operators to the current state.
; Solution: Append the results of applying all legal moves on the state, namely moving 2 missionaries, 2 cannibals, 1 missionary and 1
;			cannibal, 1 missionary, and 1 cannibal. Note that if one of these moves is not legal (e.g. violates the number of cannibals 
;			and missionaries on the same side constraint or there aren't enough cannibals/missionaries in the state to perform such a move),
;			next-state will return nil, so it won't be included in the list of legal successor states.
(defun succ-fn (s)
	(append (next-state s 2 0) (next-state s 0 2) (next-state s 1 1) (next-state s 1 0) (next-state s 0 1)))

; Given a list of successor states STATES of a certain state, a path from the initial state to the certain state, and a depth DEPTH,
; the function returns the path from the initial state to the goal state, found by a Limited DFS of DEPTH on each element of STATES.
; Solution: Check whether there are no states. If so, there are no solutions to be found, so return nil.
;			Check whether the first child is a goal state. If so, add it to the path and return the path
;			Check whether the depth is 0. If so, because we already checked whether the first child is a final state,
;			we have not found a final state so return nil.
;			Otherwise, run DFS on the first state until depth-1
;			If the result of this is not nil, it must be a solution, so return it.
;			Otherwise, no solution was found, so check the other states.
(defun mult-dfs (states path depth)
	(cond ((null states) nil)
		  ((final-state (first states)) (append path (list (first states))))
    	  ((equal depth 0) nil)
		  (t (let* ((solution (mult-dfs (succ-fn (first states)) (append path (list (first states))) (- depth 1))))
		  	(cond ((not (null solution)) solution)
		  		  (t (mult-dfs (rest states) path depth)))))))

; Given a state S, a path from the initial state to S (PATH), and a depth (DEPTH), the function returns the path 
; from the intial state to a goal state (if there is no such path, nil is returned) found by a Limited DFS at a depth DEPTH 
; Solution: Check whether the state is a final state. If so, return the path.
;			Check whether the depth is 0. If so, because we already checked whether the state is a final state,
;			we have not found a final state, so return nil.
;			Otherwise, run DFS on the children of the state.
(defun single-dfs (s path depth)
    (cond ((final-state s) path)
    	  ((equal depth 0) nil)
    	  (t (mult-dfs (succ-fn s) path depth))))

; Given an initial state S and a search depth DEPTH, the function returns a path from the initial state to a goal state, 
; found by a ID-DFS search, starting at depth DEPTH and incrementing until a solution is found. 
; Solution: Get the result of running DFS starting from the state s until depth (the path is initially just s).
;			If the result is not nil, it is a solution, so return it.
;			Otherwise, run ID-DFS at a depth incremented by 1.
(defun id-dfs (s depth)
 	(let* ((solution (single-dfs s (list s) depth)))
 		(cond ((not (null solution)) solution)
 			  (t (id-dfs s (+ depth 1))))))