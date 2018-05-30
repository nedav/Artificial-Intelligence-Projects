; Given a number N and an ordered tree TREE, the function returns t if N appears in TREE and nil otherwise
; Solution: Check whether TREE is a number. If it is, simply return whether N is equal to it.
; 			If TREE is not a number, it is a list (L m R), so check if N is equal to m and return t if so.
;			If N is not equal to m, then it could either be in L or R. Because TREE is ordered, N must be in L if N < m or in R if m < N,
;			so recurse on L in the first case and recurse on R in the second case.  
(defun TREE-CONTAINS (N TREE)
	(cond ((numberp TREE) (equal N TREE))
		  ((equal N (second TREE)) t)						
		  ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
		  (t (TREE-CONTAINS N (third TREE)))))

; Given an ordered tree TREE, the function returns the largest number in TREE
; Solution: Check whether TREE is a number. If it is, simply return that number.
;			Otherwise, TREE is a list (L m R). To find the largest number in an ordered tree, recurse on the right subtree until a single
;			number is reached. Because all values in the right subtree are larger than m, the the number in the right-most subtree must be the largest.
(defun TREE-MAX (TREE)
	(cond ((numberp TREE) TREE)
		  (t (TREE-MAX (third TREE)))))

; Given an ordered tree TREE, the function returns a list of the numbers of TREE in in-order
; Solution: Check whether TREE is a number. If it is, return a list consisting of that number
;			Otherwise, TREE is a list (L m R). In-order lists the values in the left subtree, m, and then the right subtree. So, first recurse
;			and get the in-order of the left subtree, add (m), and then recurse and get the in-order of the right subtree. The three lists
;			are then appended together and returned.
(defun TREE-ORDER (TREE)
	(cond ((numberp TREE) (list TREE))
		  (t (append (TREE-ORDER (first TREE)) (list (second TREE)) (TREE-ORDER (third TREE))))))

; Given a list L, a non-negative starting index START, and a non-negative length LEN, the function returns the sublist starting at START and having length LEN.
; If no sublist exists, return nil.
; Solution: Check whether the the indices of the requested sublist surpass the list. If so, return nil. 
;			Check whether LEN is 0. If so, return nil.
;			If the starting index is 0, add the first element to the result of recursing on the next LEN-1 elements of the list.
;			Otherwise, recurse on the rest of the list, decrementing START, to reach the starting element.
(defun SUB-LIST (L START LEN)
	(cond ((> (+ START LEN) (length L)) nil)
		  ((equal LEN 0) nil)
		  ((equal START 0) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
		  (t (SUB-LIST (rest L) (- START 1) LEN))))

; Given a list L, the function returns a list with two elements: the first half of L and the second half of L, where the length of the second half 
; minus the length of the first half is 0 or 1.
; Solution: Assign LEN to be the length of L to avoid repetitive function calls. Determine how to split L by whether LEN is even or odd.
;			If it is even, return a list containing a sublist of the first LEN/2 elements and a sublist of the second LEN/2 elements. The 
;			elements are split as such because LEN/2 - LEN/2 = 0.
;			If it is odd, return a list containing a sublist of the first (LEN-1)/2 elements and a sublist of the second (LEN+1)/2
;			elements. The elements are split has such because (LEN+1)/2 - (LEN-1)/2 = 1.
(defun SPLIT-LIST (L)
	(let* ((LEN (length L)))
		(cond ((evenp LEN) (list (SUB-LIST L 0 (/ LEN 2)) (SUB-LIST L (/ LEN 2) (/ LEN 2))))
			  ((oddp LEN) (list (SUB-LIST L 0 (/ (- LEN 1) 2)) (SUB-LIST L (/ (- LEN 1) 2) (/ (+ LEN 1) 2)))))))

; Given a binary tree TREE, the function returns the height of TREE
; Solution: Check whether the TREE is an atom. If it is, the height is 0.
;			Otherwise, TREE is a list. To find the height, get the height of the left and right subtrees, which are saved in local variables
;			to avoid repetitive function calls, and choose the greater, as that will be the longest path. Adding 1 to the recursive calls
;			accounts for the current layer.
(defun BTREE-HEIGHT (TREE)
	(cond ((atom TREE) 0)
		  (t (let* ((LEFT (+ (BTREE-HEIGHT (first TREE)) 1)) (RIGHT (+ (BTREE-HEIGHT (second TREE)) 1)))
		  		(cond ((> LEFT RIGHT) LEFT)
		  			  (t RIGHT))))))

; Given a non-empty list of atoms LEAVES, the function returns a binary tree such that all the elements of LEAVES are elements and that,
; for every interal node of the tree, the number of leaves in the right branch minus the number of leaves in the left branch is 0 or 1.
; Solution: Check whether LEAVES has 1 or 2 elements; if it has 1, return that element, and if it has 2, return the 2 as a list.
;			Otherwise, use SPLIT-LIST to split LEAVES into halves and save it to a local variable to avoid redundant function calls.
;			Recursively get the elements of the binary tree by recursing on either half of LEAVES and put the results in a list together.
(defun LIST2BTREE (LEAVES)
	(cond ((equal (length LEAVES) 1) (first LEAVES))
		  ((equal (length LEAVES) 2) LEAVES)
		  (t (let* ((SPLIT (SPLIT-LIST LEAVES)))
		  		(list (LIST2BTREE (first SPLIT)) (LIST2BTREE (second SPLIT)))))))

; Given a binary tree TREE, the function returns a list of all the atoms in TREE
; Solution: Check whether TREE is just an atom. If it is, return it in a list
;			Otherwise, recursively get the list of atoms from both sides of TREE and append them together.
(defun BTREE2LIST (TREE)
	(cond ((atom TREE) (list TREE))
		  (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))))

; Given two Lisp expressions E1 and E2 whose atoms are all numbers, the function returns t if E1 and E2 are identical and nil otherwise
; Solution: Check whether the expressions are both nil. If they are, return true.
;			Check whether the expressions are both numbers. If they are, return whether they are the same.
;			Check whether the expressions are both lists. If they are, recursively check whether the first elements are the same AND the 
;			rest of the elements are the same.
;			Otherwise, the expressions cannot be the same, so return nil.
(defun IS-SAME (E1 E2)
	(cond ((and (null E1) (null E2)) t)
		  ((and (numberp E1) (numberp E2)) (= E1 E2))
		  ((and (listp E1) (listP E2)) (and (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))
		  (t nil)))