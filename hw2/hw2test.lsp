(load "hw2.lsp")
(print "--Problem 1 Tests--")
(cond ((equal (DFS '((A (B)) C (D))) '(A B C D)) (print "Test 1 Success"))
      (t                   (print "Test 1 Failure")))

(cond ((equal (DFS 'A) '(A)) (print "Test 2 Success"))
      (t                   (print "Test 2 Failure")))

(cond ((equal (DFS '((A) (B))) '(A B)) (print "Test 3 Success"))
      (t                                (print "Test 3 Failure")))

(cond ((equal (DFS '((A) (B) (C) (D) (E))) '(A B C D E)) (print "Test 4 Success"))
      (t                                (print "Test 4 Failure")))

(cond ((equal (DFS '(A B C D E)) '(A B C D E)) (print "Test 5 Success"))
      (t                                (print "Test 5 Failure")))

(cond ((equal (DFS '(((((((A))) B))) (((C ((((D)))) E))))) '(A B C D E)) (print "Test 6 Success"))
      (t                                (print "Test 6 Failure")))

(cond ((equal (DFS '((W X) (Y Z))) '(W X Y Z)) (print "Test 7 Success"))
      (t                                (print "Test 7 Failure")))

(cond ((equal (DFS '(((A) B) C (D))) '(A B C D)) (print "Test 8 Success"))
      (t                   (print "Test 8 Failure")))


(print "--Problem 2 Tests--")

(cond ((equal (DFID  '((A (B)) C (D)) 3) '(C A C D A B C D)) (print "Test 1 Success"))
      (t                   (print "Test 1 Failure")))

(cond ((equal (DFID  '(A (B C) (D) (E (F G))) 3) '(A A B C D E A B C D E F G)) (print "Test 2 Success"))
      (t                   (print "Test 2 Failure")))

(cond ((equal (DFID  'A 0) '(A)) (print "Test 3 Success"))
      (t                   (print "Test 3 Failure")))

(cond ((equal (DFID  '(A) 0) NIL) (print "Test 4 Success"))
      (t                   (print "Test 4 Failure")))

(cond ((equal (DFID  '(A) 1) '(A)) (print "Test 5 Success"))
      (t                   (print "Test 5 Failure")))

(cond ((equal (DFID  '((w x) (y z)) 2) '(w x y z)) (print "Test 6 Success"))
      (t                   (print "Test 6 Failure")))

(cond ((equal (DFID  '((((A))) (((B)))) 4) '(A B)) (print "Test 7 Success"))
      (t                   (print "Test 7 Failure")))

(cond ((equal (DFID  '((((A)) B)) 4) '(B B A B)) (print "Test 8 Success"))
      (t                   (print "Test 8 Failure")))

(cond ((equal (DFID  '(A (B (C (D (E))))) 5) '(A A B A B C A B C D A B C D E)) (print "Test 9 Success"))
      (t                   (print "Test 9 Failure")))

(cond ((equal (DFID  '(V E R Y ((((G O O D J O B))))) 5) '(V E R Y V E R Y V E R Y V E R Y V E R Y G O O D J O B)) (print "Test 10 Success"))
      (t                   (print "Test 10 Failure")))
 
 (cond ((equal (DFID  '((((A (B)))) C) 5) '(C C C A C A B C)) (print "Test 11 Success"))
      (t                   (print "Test 11 Failure")))

(print "--Problem 3 Tests--")

(cond ((equal (next-state '(3 3 t) 1 0) NIL) (print "Test 1 Success"))
      (t                   (print "Test 1 Failure")))

(cond ((equal (next-state '(3 3 t) 0 1) '((0 1 NIL))) (print "Test 2 Success"))
      (t                   (print "Test 2 Failure")))

(cond ((equal (succ-fn '(3 3 t)) '((0 2 NIL) (1 1 NIL) (0 1 NIL))) (print "Test 3 Success"))
      (t                   (print "Test 3 Failure")))

(cond ((equal (succ-fn '(1 1 t)) '((3 3 NIL) (3 2 NIL))) (print "Test 4 Success"))
      (t                   (print "Test 4 Failure")))

(cond ((equal (ID-DFS '(3 3 T) 0) '((3 3 T) (0 2 NIL) (3 2 T) (0 3 NIL) (3 1 T) (2 2 NIL) (2 2 T) (3 1 NIL) (0 3 T) (3 2 NIL) (1 1 T) (3 3 NIL))) (print "Test 5 Success"))
      (t                   (print "Test 5 Failure")))

(print "---------All tests completed.---------")
