
(setq L 
	'(
		"cnfs/sat/cnf_10.cnf"
		"cnfs/sat/cnf_20.cnf"
		"cnfs/sat/cnf_30.cnf"
		"cnfs/sat/cnf_50.cnf"
		"cnfs/unsat/cnf_12.cnf"
		"cnfs/unsat/cnf_20.cnf"
		"cnfs/unsat/cnf_30.cnf"
		"cnfs/unsat/cnf_42.cnf"
		;"cnfs/new/cnf_100_200.cnf"
		;"cnfs/new/cnf_100_300.cnf"
		;"cnfs/new/cnf_100_420.cnf"
		;"cnfs/new/cnf_100_600.cnf"
		;"cnfs/new/cnf_200_1200.cnf"
		;"cnfs/new/cnf_200_400.cnf"
		;"cnfs/new/cnf_200_600.cnf"
		;"cnfs/new/cnf_200_820.cnf"
		;"cnfs/new/cnf_500_1000.cnf"
		;"cnfs/new/cnf_500_1500.cnf"
		;"cnfs/new/cnf_500_2100.cnf"
		;"cnfs/new/cnf_500_3000.cnf"
		;"cnfs/new/cnf_1000_2000.cnf"
		;"cnfs/new/cnf_1000_3000.cnf"
		;"cnfs/new/cnf_1000_4200.cnf"
		;"cnfs/new/cnf_1000_6000.cnf"
		;"cnfs/new/cnf_2000_8400.cnf"
		;"cnfs/new/cnf_3000_12600.cnf"
		;"cnfs/new/cnf_5000_20.cnf"
		;"cnfs/new/cnf_5000_21000.cnf"
		))


(defun test-h (l)
	(cond ((null l) t)
		  (t (format t (car l)) (time (print (solve-cnf (car l)))) (format t "----------~%") (test-h (cdr l)))))

(defun test ()
	(load "parse_cnf.lsp")
	(load "hw4.lsp")
	(test-h L))