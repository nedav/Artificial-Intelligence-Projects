;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

; Given a state s, return t if s is a goal state and nil otherwise.
; Logic: If s is a goal state, all boxes will be in goals, meaning there will be no squares consisting of just a box.
;		 Therefore, to determine if s is a goal state check that no squares consist of just a box.
;		 Note that you do not need to check specifically that the boxes are in goals because if the boxes are not alone in a square, they must be in a goal.
; Description: If s is nil, return t so as to not affect the logical "and". This is used to stop the recursion on the rest of s.
;			   Otherwise, check whether isBox returns false for the first row of squares by calling a helper function 
;			   and recursively check that isBox returns false for all other rows.
(defun goal-test (s)
	(cond ((null s) t)
		  (t (and (isNotBoxRow (first s)) (goal-test (rest s))))))

; Given a row of a state r, return t if every element of r does not contain a box and nil otherwise.
; Logic: This is a helper function for goal-test.
; Description: If the function has recursed on the rest of r until it is nil, isBox must not have returned nil for any element of r, 
;			   so there must be no squares with just boxes, so return t.
;			   Check whether the first element is a box. If it is, the state cannot be a goal-state, so return nil.
;			   Otherwise, recursively check the rest of the elements.
(defun isNotBoxRow (r)
	(cond ((null r) t)
		  ((isBox (first r)) nil)
		  (t (isNotBoxRow (rest r)))))

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.

; Given a state s, return a list of all states that can be reached from the given state in one move (up, down, left, or right)
; Description: Try each of the four moves, create a list of the resulting states, and remove any moves that are invalid (nil)
(defun next-states (s)
  	(cleanUpList (list (try-move s 'u) (try-move s 'd) (try-move s 'l) (try-move s 'r))))

; Given a state s, a row number r, and a column number c, return the integer content of s at square (r, c). If there is no such square, return 1 (the value of a wall).
; Logic: Get to the correct row by recursing on the rest of s and decrementing r by 1 until r is 0. 
;		 Then call a helper function to get the cth element of that row.
; Description: Check whether s is nil. This is used in cases where, after recursively decrementing r, 
;			   the rest of s is null before r=0 has been reached. This means that row r is outside the bounds of s, so return 1.
;			   Check whether r is 0. If so, that is the first row of s, so get the cth of it.
;			   Otherwise, keep on recursing on the rest of s and decremented r until the desired row is reached.
(defun get-square (s r c)
	(cond ((null s) 1)
		  ((= r 0) (get-cth-row (first s) c))
		  (t (get-square (rest s) (- r 1) c))))

; Given a row r and a column number c, return the cth element of r. If there is no such element, return 1 (the value of a wall).
; Logic: This is a helper function for get-square. Get the correct element by recursing on the rest of r and decrementing c by 1 until c is 0.
; Description: Check whether r is nil. This is used in cases where, after recursively decrementing c, 
;			   the rest of r is null before c=0 has been reached. This means that c is outside the bounds of r, so return 1.
;			   Check whether c is 0. If so, that is the first element of r, so return it.
;			   Otherwise, keep on recursing on the rest of r and decremented c until the desired element is reached.
(defun get-cth-row (r c)
	(cond ((null r) 1)
		  ((= c 0) (first r))
		  (t (get-cth-row (rest r) (- c 1)))))

; Given a state s, a row number r, a column number c, and an integer value v, return a new state such that square (r, c) has value v.
; Logic: Get the correct row by recursing on the rest of s and decrementing r by 1 until r is 0.
;		 Then call a helper function to set the cth element of the row.
; Description: Check whether s is nil. This is used in cases where, after recursively decrementing r, 
;			   the rest of s is null before r=0 has been reached. This means that r is outside the bounds of s, so return nil.
;			   Check whether r is 0. If so, that is the first row of s, so construct a new state where the first row has 
;			   the cth element set to c and the other rows remain the same.
;			   Otherwise, keep the current row in the state and set the rest of the state to the result of recursing on the rest of s 
;			   and decremented r until the desired row is reached and changed.
(defun set-square (s r c v)
	(cond ((null s) s)
		  ((= r 0) (cons (set-cth-row (first s) c v) (rest s)))
		  (t (cons (first s) (set-square (rest s) (- r 1) c v)))))

; Given a row r, a column number c, and an integer value v, return a new row such that element c has value v.
; Logic: Set the correct element by recursing on the rest of r and decrementing c by 1 until c is 0.
; Description: Check whether r is nil. This is used in cases where, after recursively decrementing c, 
;			   the rest of r is null before c=0 has been reached. This means that c is outside the bounds of r, so return nil.
;			   Check whether c is 0. If so, that is the first element of r, so constuct a new row where the first element is v 
;			   and the rest is the same as r.
;			   Otherwise, keep the current element and set the rest of the row to the result of recursing on the rest of r 
; 			   and decremented c until the desired element is reached and changed.
(defun set-cth-row (r c v)
	(cond ((null r) r)
		  ((= c 0) (cons v (rest r)))
		  (t (cons (first r) (set-cth-row (rest r) (- c 1) v)))))

; Given a state s and a direction d (u=up, d=down, l=left, r=right), return a new state that is the result of moving 
; the keeper in direction d. If the move is invalid, return nil.
; Logic: Based on the next square that is in direction d of the keeper, either do nothing (the move is invalid), 
;		 move the keeper, or move a box and a keeper.
; Description: Check where s is nil. This is used in cases where move-keeper is called after move-box returned a nil state.
;			   If the next square is a wall, the move is invalid, so return nil.
;			   If the next square is blank, move the keeper there.
;					If the keeper is on a blank, leave a blank behind, otherwise it must be on a goal, so leave a goal.
; 			   If the next square is a goal, move the keeper there.
;					If the keeper is on a blank, leave a blank behind, otherwise it must be on a goal, so leave a goal.
; 			   If the next square is a box or a box on a goal, move the box and then move the keeper.
(defun try-move (s d)
	(if (null s) nil 
	(let* ((k-pos (getKeeperPosition s 0)) (k_row (cadr k-pos)) (k_col (car k-pos)) (curr-square (get-square s k_row k_col))
		   (n-pos (get-next-pos k_row k_col d)) (n_row (car n-pos)) (n_col (cadr n-pos)) (next-square (get-square s n_row n_col)))
		(cond ((isWall next-square) nil)
			  ((isBlank next-square) (if (isKeeper curr-square) (set-next-square s k_row k_col d 0 3) (set-next-square s k_row k_col d 4 3)))
			  ((isStar next-square) (if (isKeeper curr-square) (set-next-square s k_row k_col d 0 6) (set-next-square s k_row k_col d 4 6)))
		      ((or (isBox next-square) (isBoxStar next-square)) (try-move (move-box s k_row k_col d) d))))))  

; Given a state s, the row position of the keeper k_row, the column position of the keeper k_col, and a direction d, 
; return a state that is the result of the box being pushed by the keeper
; Logic: Based on the next square that is in direction d of the box, either do nothing (the move is invalid) or move the box.
; Description: If the next square is a wall, the move is invalid.
;			   If the next square is another box, the move is invalid.
; 			   If the next square is blank, move the box there.
;					If the box is on a blank, leave a blank behind, otherwise it must be on a goal, so leave a goal.
; 			   If the next square is goal, move the box there.
;					If the box is on a blank, leave a blank behind, otherwise it must be on a goal, so leave a goal.
(defun move-box (s k_row k_col d)
	(let* ((k-square (get-square s k_row k_col))
		   (box-pos (get-next-pos k_row k_col d)) (b_row (car box-pos)) (b_col (cadr box-pos)) (b-square (get-square s b_row b_col))
		   (n-pos (get-next-pos b_row b_col d)) (n_row (car n-pos)) (n_col (cadr n-pos)) (next-square (get-square s n_row n_col)))
		(cond ((isWall next-square) nil)
			  ((isBox next-square) nil)
			  ((isBlank next-square) (if (isBox b-square) (set-next-square s b_row b_col d 0 2) (set-next-square s b_row b_col d 4 2)))
			  ((isStar next-square) (if (isBox b-square) (set-next-square s b_row b_col d 0 5) (set-next-square s b_row b_col d 4 5))))))

; Given a row number r, a column number c, and a direction d, return the coordinates of the next square in that direction in the form (row, column).
; Logic: This function is used to get the coordinates of the square a keeper or box wants to move to.
(defun get-next-pos (r c d)
	(cond ((equal d 'u) (list (- r 1) c))
		  ((equal d 'd) (list (+ r 1) c))
		  ((equal d 'l) (list r (- c 1)))
		  ((equal d 'r) (list r (+ c 1)))))

; Given a state s, a row number r, a column number c, and a direction d, return the value of the square in the direction d of (r, c).
; Logic: This function is used to get the content of the square that a keeper or box wants to move to
(defun get-next-square (s r c d)
	(cond ((equal d 'u) (get-square s (- r 1) c))
		  ((equal d 'd) (get-square s (+ r 1) c))
		  ((equal d 'l) (get-square s r (- c 1)))
		  ((equal d 'r) (get-square s r (+ c 1)))))

; Given a state s, a row number r, a column number c, and a direction d, a value for the current square curr-val, and 
; a value for the next square next-val, return a state in which (r, c) has curr-val and the square in the direction d of (r, c)
; has next-val.
; Logic: This function is used to "move" a keeper or box in a direction, where next-val is the keeper/box on a blank/goal and curr-val 
;		 is a blank/goal.
; Description: Set the value of the current square, and then set the value of the next square.
(defun set-next-square (s r c d curr-val next-val)
	(cond ((equal d 'u) (set-square (set-square s r c curr-val) (- r 1) c next-val))
		  ((equal d 'd) (set-square (set-square s r c curr-val) (+ r 1) c next-val))
		  ((equal d 'l) (set-square (set-square s r c curr-val) r (- c 1) next-val))
		  ((equal d 'r) (set-square (set-square s r c curr-val) r (+ c 1) next-val))))


; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.  

; Given a state, return 0.
(defun h0 (s) 0)

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.

; Given a state s, return the number of boxes that are not on goal positions in s
; Logic: Boxes which are not on goal positions are indicated with a "2". Simply count the number of 2s in s by counting the number of 
;		 squares for which isBox returns true.
; Solution: If s is nil, return 0 as to not affect the sum. This is used to stop the recursion.
;			Otherwise, count the number of boxes in the first row and add them to the number of boxes in the other rows.
; Is this heuristic admissible? Answer: Yes, because the optimal solution must consist of at least n moves, where n is the number of boxes
;								left to be pushed into goals. Because multiple boxes cannot be pushed at the same time, the keeper must at
;								least move once in order to push each box into a goal.
(defun h1 (s)
	(cond ((null s) 0)
		  (t (+ (count-boxes (first s)) (h1 (rest s))))))

; Given a row of a state, return the number of boxes in the row.
; Logic: This is a helper function for h1; see logic of h1 above.
; Solution: If r is nil, return 0 so as to not affect the sum. This is used to stop the recursion.
;			Check whether the first element is a box. If it is, add 1 to the count for the number of boxes in the rest of r.
;			Otherwise, count the number of boxes in the rest of r.
(defun count-boxes (r)
	(cond ((null r) 0)
		  ((isBox (first r)) (+ 1 (count-boxes (rest r))))
		  (t (count-boxes (rest r)))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.


; Given a state s, return the maximum Manhattan distance from the keeper to a box plus the number of other boxes.
; Description: Get the position of the keeper and a list of all the positions of the boxes (not in goals) in the state.
;			   If there are no boxes, return 0.
;			   Otherwise, calculate the Manhattan distances from the keeper to every box and return the max Manhattan distance
;			   plus the number of other boxes.
(defun h404447721 (s)
	(let* ((k-pos (getKeeperPosition s 0)) (k_row (cadr k-pos)) (k_col (car k-pos)) (boxes (find-all-boxes s 0)))
		(cond ((null boxes) 0)
			  (t (let* ((diffs (differences k_row k_col boxes)))
			  		(maxn diffs (first diffs)))))))

; Given a state s and a row number row, return a list with the positions (r, c) of every box in s.
; Description: Get the positions of all the boxes in the first row and append it to the result of getting all the positions of all the boxes
;			   in the other rows.
(defun find-all-boxes (s row)
	(cond ((null s) nil)
		  (t (append (find-all-boxes-column (first s) row 0) (find-all-boxes (rest s) (+ row 1))))))

; Given a row r, a row number row, and a column number col, return a list with the positions (r, c) of every box in r.
; Description: If the first element is a box, add its position to the list of positions of the rest of the column.
;			   Otherwise, get the list of positions of the rest of the column. 
(defun find-all-boxes-column (r row col)
	(cond ((null r) nil)
		  ((isBox (first r)) (cons (list row col) (find-all-boxes-column (rest r) row (+ col 1))))
	      (t (find-all-boxes-column (rest r) row (+ col 1)))))

; Given an x coordinate x1, a y coordinate y1, and a list of positions (r, c) of boxes, return a list of the Manhattan differences between 
; (x1, y1) and every (r, c).
; Description: Calculate the Manhattan distance (abs(x2 - x1) + abs(y2 - y1)) for the keeper and every box and add it to the list of 
;			   differences for the rest of the positions.
(defun differences (x1 y1 boxes)
	(if (null boxes) nil
		(let* ((x2 (first (first boxes))) (y2 (second (first boxes))) (xd (- x2 x1)) (yd (- y2 y1)))
			(cons (+ (absn xd) (absn yd)) (differences x1 y1 (rest boxes))))))

; Given a list l of integers and the current biggest element biggest, return the greatest element in the list.
; Description: If the first element is bigger than the biggest so far, make the first element the biggest and check the rest of the list.
;			   Otherwise, check the rest of the list with the current biggest.
(defun maxn (l biggest)
	(cond ((null l) biggest)
		  ((> (first l) biggest) (maxn (rest l) (first l)))
		  (t (maxn (rest l) biggest))))

; Given an integer a, return the absolute value of a.
(defun absn (a)
	(if (< a 0) (* -1 a) a))	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
