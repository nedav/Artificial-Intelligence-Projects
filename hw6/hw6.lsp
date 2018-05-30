;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw6.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).

; Given a node index n, a color index c, and the maximum number of colors k, return the variable
; that represents node n getting color c.
(defun node2var (n c k)
	(+ (* (- n 1) k) c))

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."

; Given a node index n, a color index c, and the maximum number of colors k, return a clause that represents
; (node n gets color c) or (n gets color c+1) ... or (n gets color k). For the clause to be true,
; at least one of "node n gets color x" must be satisfied, therefore satisfying the constraint that n must get 
; at least one color from the set {c,c+1,...,k}.
(defun at-least-one-color (n c k)
	(cond ((> c k) nil)				; Stop recursing when all k colors have been added
		  (t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))))

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."

; Given a node index n, a color index c, and the maximum number of colors k, return a list of clauses, where each clause prevents
; n from getting both a color x and a color y from the set {c,c+1,...,k}, for all x,y in the set.
; This satisfies the "at most one color" constraint as it prevents n from being assigned to 2 or more colors, thus restricting
; n to only be assigned to 0 or 1 colors.
(defun at-most-one-color (n c k)
	(cond ((= c k) nil)
		  ; Get a list of clauses that represent "n cannot get color c and a color from the set {c+1,...,k}"
		  ; and append it to clauses that represent "n cannot get both a color x and a color y 
		  ; from the set {c+1,...,k}, for all x,y in the set".
		  (t (append (at-most-one-color-clauses n c (+ c 1) k) (at-most-one-color n (+ c 1) k)))))

; Given a node index n, a color index c1, a color index c2, and a maximum number of colors k,
; return a list of clauses that represent "node n cannot get color c1 and a color from the set {c2,...,k}" 
; In logic, this is (not (n gets color c1 and n gets color x)) = (not n gets color c1 or not n gets color x), where x is in {c2,...,k}.
(defun at-most-one-color-clauses (n c1 c2 k)
	(cond ((> c2 k) nil)			; Stop recursing when all colors from c2 to k have been added
		  (t (cons (list (- (node2var n c1 k)) (- (node2var n c2 k))) (at-most-one-color-clauses n c1 (+ c2 1) k)))))

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."

; Given a node index n and the maximum number of colors k, return a list of clauses consisting of the clause that represents
; n getting at least one color from {1,2,...,k} and the clauses that represent n getting at most one color from 
; {1,2,...,k}, therefore constraining n to getting exactly one color from {1,2,...,k}.
; Formally, if x >= 1 and x <= 1, then x = 1.
(defun generate-node-clauses (n k)
	(cons (at-least-one-color n 1 k) (at-most-one-color n 1 k)))

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."

; Given an edge and the maximum number of colors k, return a list of clauses representing that the nodes at both ends of edge e 
; cannot have the same color from the set {1,2,...,k}.
(defun generate-edge-clauses (e k)
	(generate-edge e 1 k))

; Given an edge e, a color index c, and the maximum number of colors k, return a list of clauses, where each clause
; prevents the nodes in e from having the same color x, for all x in the set {1,2,...,k}.
; In logic, this is (not (n1 gets color x and n2 gets color x)) = (not n1 gets color x or not n2 gets color x). 
(defun generate-edge (e c k)
	(cond ((> c k) nil)
		  (t (cons (list (- (node2var (first e) c k)) (- (node2var (second e) c k))) (generate-edge e (+ c 1) k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun