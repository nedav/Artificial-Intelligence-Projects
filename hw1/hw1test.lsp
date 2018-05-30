(load "hw1.lsp")
(print "--Problem 1 Tests--")
(cond ((TREE-CONTAINS 4 4) (print "Test 1 Success"))
      (t                   (print "Test 1 Failure")))

(cond ((TREE-CONTAINS 4 3) (print "Test 2 Failure"))
      (t                   (print "Test 2 Success")))

(cond ((TREE-CONTAINS 3 '((1 2 3) 7 8)) (print "Test 3 Success"))
      (t                                (print "Test 3 Failure")))

(cond ((TREE-CONTAINS 4 '((1 2 3) 7 8)) (print "Test 4 Failure"))
      (t                                (print "Test 4 Success")))

(print "--Problem 2 Tests--")
(cond ((= (TREE-MAX '((1 2 3) 7 8)) 8) (print "Test 1 Success"))
      (t                               (print "Test 1 Failure")))

(print "--Problem 3 Tests--")
(cond ((equal (TREE-ORDER 3) '(3)) (print "Test 1 Success"))
       (t                          (print "Test 1 Failure")))

(cond ((equal (TREE-ORDER '((1 2 3) 7 8)) '(1 2 3 7 8)) (print "Test 2 Success"))
       (t                                               (print "Test 2 Failure")))

(print "--Problem 4 Tests--")
(cond ((equal (SUB-LIST '(a b c d) 0 3) '(a b c)) (print "Test 1 Success"))
      (t                                        (print "Test 1 Failure")))

(cond ((equal (SUB-LIST '(a b c d) 3 1) '(d)) (print "Test 2 Success"))
      (t                                     (print "Test 2 Failure")))

(cond ((equal (SUB-LIST '(a b c d) 2 0)  NIL) (print "Test 3 Success"))
      (t                                      (print "Test 3 Failure")))

(print "--Problem 5 Tests--")

(cond ((equal (SPLIT-LIST '(a b c d)) '((a b) (c d))) (print "Test 1 Success"))
      (t                                                (print "Test 1 Failure")))

(cond ((equal (SPLIT-LIST '(a b c d e))  '((a b) (c d e))) (print "Test 2 Success"))
      (t                                                     (print "Test 2 Failure")))

(cond ((equal (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f))) (print "Test 3 Success"))
      (t                                                        (print "Test 3 Failure")))

(print "--Problem 6 Tests--")

(cond ((= (BTREE-HEIGHT 1) 0) (print "Test 1 Success"))
      (t                      (print "Test 1 Failure")))
(cond ((= (BTREE-HEIGHT '(1 2)) 1) (print "Test 2 Success"))
      (t                         (print "Test 2 Failure")))
(cond ((= (BTREE-HEIGHT '(1 (2 3))) 2) (print "Test 3 Success"))
      (t                               (print "Test 3 Failure")))
(cond ((= (BTREE-HEIGHT '((1 2) (3 4)))  2) (print "Test 4 Success"))
      (t                                    (print "Test 4 Failure")))
(cond ((= (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3) (print "Test 5 Success"))
      (t                                               (print "Test 5 Failure")))
(cond ((= (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3) (print "Test 6 Success"))
      (t                                                   (print "Test 6 Failure")))

(print "--Problem 7 Tests--")

(cond ((equal (LIST2BTREE '(1)) 1) (print "Test 1 Success"))
      (t                           (print "Test 1 Failure")))

(cond ((equal (LIST2BTREE '(1 2)) '(1 2)) (print "Test 2 Success"))
      (t                                  (print "Test 2 Failure")))

(cond ((equal (LIST2BTREE '(1 2 3)) '(1 (2 3))) (print "Test 3 Success"))
      (t                                        (print "Test 3 Failure")))
(cond ((equal (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4))) (print "Test 4 Success"))
      (t                                              (print "Test 4 Failure")))

(cond ((equal (LIST2BTREE '(1 2 3 4 5 6 7)) '((1 (2 3)) ((4 5) (6 7)))) (print "Test 5 Success"))
      (t                                                                (print "Test 5 Failure")))

(cond ((equal (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8)))) (print "Test 6 Success"))
      (t                                                                      (print "Test 6 Failure"))) 

(print "--Problem 8 Tests--")

(cond ((equal (BTREE2LIST 1) '(1)) (print "Test 1 Success"))
      (t                          (print "Test 1 Failure")))
(cond ((equal (BTREE2LIST '(1 2)) '(1 2)) (print "Test 2 Success"))
      (t                                  (print "Test 2 Failure")))
(cond ((equal (BTREE2LIST '(1 (2 3))) '(1 2 3)) (print "Test 3 Success"))
      (t                                        (print "Test 3 Failure")))
(cond ((equal (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4)) (print "Test 4 Success"))
      (t                                              (print "Test 4 Failure")))
(cond ((equal (BTREE2LIST '((1 (2 3)) ((4 5) (6 7)))) '(1 2 3 4 5 6 7)) (print "Test 5 Success"))
      (t                                                                (print "Test 5 Failure")))
(cond ((equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8)) (print "Test 6 Success"))
      (t                                                                      (print "Test 6 Failure")))
(print "--Problem 9 Tests--")
(cond ((IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) T (print "Test 1 Success"))
      (t                                         (print "Test 1 Failure")))
(cond ((IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) (print "Test 2 Failure"))
      (t                                     (print "Test 2 Success")))
(print "---------All tests completed.---------")
