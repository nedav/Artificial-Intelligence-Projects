(load "hw3.lsp")
(print "--Goal-test Tests--")
(cond ((equal (goal-test  '((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 3 0 1) (1 0 0 0 1) (1 1 1 1 1))) nil) (print "Test 1 Success"))
      (t                   (print "Test 1 Failure")))

(cond ((equal (goal-test  '((1 1 1 1 1) (1 0 0 4 1) (1 0 6 0 1) (1 0 3 0 1) (1 0 0 0 1) (1 1 1 1 1))) t) (print "Test 2 Success"))
      (t                   (print "Test 2 Failure")))

(cond ((equal (goal-test  '((1 1 1 1 1) (1 0 0 4 1) (1 0 0 0 1) (1 0 3 0 1) (1 0 0 0 1) (1 1 1 1 2))) nil) (print "Test 3 Success"))
      (t                   (print "Test 3 Failure")))

(print "--h0 Tests--")
(cond ((equal (h0  '((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 3 0 1) (1 0 0 0 1) (1 1 1 1 1))) 0) (print "Test 1 Success"))
      (t                   (print "Test 1 Failure")))

(print "--h1 Tests--")
(cond ((equal (h1 '((1 1 1 1 1) (2 0 0 4 1) (1 0 2 0 1) (1 0 3 0 1) (1 0 0 0 1) (1 2 1 1 2))) 4) (print "Test 1 Success"))
      (t                   (print "Test 1 Failure")))


(print "--Next-states Tests--")
(setq s1 '((1 1 1 1 1)
           (1 0 0 4 1)
           (1 0 2 0 1) 
           (1 0 3 0 1) 
           (1 0 0 0 1) 
           (1 1 1 1 1)))

(cond ((equal (next-states s1) '(((1 1 1 1 1) (1 0 2 4 1) (1 0 3 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1)) 
                                 ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 3 0 1) (1 1 1 1 1)) 
                                 ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 3 0 0 1) (1 0 0 0 1) (1 1 1 1 1))
                                 ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 0 3 1) (1 0 0 0 1) (1 1 1 1 1)))) (print "Test 1 Success"))
      (t                   (print "Test 1 Failure")))

(setq s2 '((1 1 1 1 1)
           (1 0 0 4 1) 
           (1 0 2 3 1) 
           (1 0 0 0 1) 
           (1 0 0 0 1) 
           (1 1 1 1 1))) 

(cond ((equal (next-states s2) '(((1 1 1 1 1) (1 0 0 6 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1))
                                  ((1 1 1 1 1) (1 0 0 4 1) (1 0 2 0 1) (1 0 0 3 1) (1 0 0 0 1) (1 1 1 1 1))
                                  ((1 1 1 1 1) (1 0 0 4 1) (1 2 3 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1)))) (print "Test 2 Success"))
      (t                   (print "Test 2 Failure")))

(setq s3 '((1 1 1 1 1) 
           (1 0 0 6 1) 
           (1 0 2 0 1) 
           (1 0 0 0 1) 
           (1 0 0 0 1) 
           (1 1 1 1 1)))

(cond ((equal (next-states s3) '(((1 1 1 1 1) (1 0 0 4 1) (1 0 2 3 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1))
                                 ((1 1 1 1 1) (1 0 3 4 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1)))) (print "Test 3 Success"))
      (t                   (print "Test 3 Failure")))

(setq s4 '((1 1 1 1 1) (1 4 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 0 5 3 1) (1 1 1 1 1)))  

(cond ((equal (next-states s4) '(((1 1 1 1 1) (1 4 2 0 1) (1 0 0 0 1) (1 0 0 3 1) (1 0 5 0 1) (1 1 1 1 1))
                                 ((1 1 1 1 1) (1 4 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 2 6 0 1) (1 1 1 1 1)))) (print "Test 4 Success"))
      (t                   (print "Test 4 Failure")))

(print "---------All tests completed.---------")