;Nathan Tung (004-059-195)
;
; CS161 Fall 2013 HW6: Graph coloring to SAT conversion
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
;
(defun node2var (n c k)
	(+ (* (- n 1) k) c) ;calculate and return variable index
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
	(cond ((< k 1) nil) ;max number of colors should never be 0 or lower
		((< k c) nil) ;color index should never be less than max number of colors
		((= c k) (list (node2var n k k))) ;if we're done, return last variable index
		(t (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k))) ;otherwise, append current variable index to rest
	)
)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
(defun at-most-one-color (n c k)
	(cond ((= k c) (list (list (* k n) (* (* k n) -1))))
		;if there's only one literal, return conjuction of that literal and its negative (always true)
		(t (get-one-at-most (at-least-one-color n c k))) ;otherwise, find clause given list of all literals
	)
)

(defun get-one-at-most (s)
	(cond ((<= (length s) 1) nil) ;if s has only one element, we end here
		(t (append (permutation-one-at-most (* (first s) -1) (rest s)) (get-one-at-most (rest s))))
			;otherwise, try appending the permutations of the negated first and rest of list s
			;along with the permutations of the rest of list s
	)
)

(defun permutation-one-at-most(a b)
	(cond ((null b) nil) ;if rest of list is null, we're done
		(t (append (list (list a (* (first b) -1))) (permutation-one-at-most a (rest b))))
			;otherwise, make a list of 2-lists with literal a and negated literal from the front of list b
	)
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
(defun generate-node-clauses (n k)
	(append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k)) ;append at-least and at-most with c=1
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
(defun generate-edge-clauses (e k)
	(avoid-colors-clauses e 1 k) ;run helper function starting with c=1
)

(defun avoid-colors-clauses (e c k)
	(cond ((< k c) nil) ;if color ever exceeds color max, return nil
		(t (append (list (list (* (node2var (first e) c k) -1) (* (node2var (second e) c k) -1))) (avoid-colors-clauses e (+ c 1) k)))
			;otherwise, find the clause where color occurs for both x and y
			;negate that clause and append it to the rest of the function
	)
)


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
