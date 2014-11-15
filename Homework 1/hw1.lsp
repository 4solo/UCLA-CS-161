;Nathan Tung (004-059-195)

;Explanation: SUB-LIST attempts to get START variable to be 0
;so that it can beginning concatenating the first character for LEN amount before returning
(defun SUB-LIST (L START LEN)
	(cond ((NULL L) NIL) ;if the list is ever empty, return nil
		((= LEN 0) NIL) ;if our length has been achieved, return nil
		((/= START 0) (SUB-LIST (rest L) (- START 1) LEN)) ;if we're not starting at index one, truncate the first atom
		(t (cons (first L) (SUB-LIST(rest L) 0 (- LEN 1)))))) ;otherwise, place first atom onto front of the list of other atoms
			
;Explanation: SPLIT-LIST checks if L is even length, where it can split L exactly in two;
;otherwise, we determine the break point where the first half is 1 less than the second half
(defun SPLIT-LIST (L)
	(cond ((NULL L) NIL) ;if there's no list, return nothing
		((evenp (length L)) ;if there is a list with even number of atoms, break it into two halves
			(let* ((half (/ (length L) 2))) (list  (SUB-LIST L 0 half) (SUB-LIST L half half) )))
		(t ;otherwise, the right sublist has one more atom than the left sublist
			(let* ((half (/ (- (length L) 1) 2))) (list (SUB-LIST L 0 half) (SUB-LIST L half (+ half 1)) )))))

;Explanation: LIST2BTREE returns a list of either one or two atoms as a base case;
;for lists of more atoms, it splits the list and recursively calls itself to build a tree
(defun LIST2BTREE (LEAVES) 
	(cond ((= (length LEAVES) 1) ;if there is one leaf, return the single-atom list
			(first LEAVES))
		((= (length LEAVES) 2) ;if there are two leaves, return the two-atom list
			LEAVES)
		(t ;otherwise, we recursively concatenate the tree of the left side with the tree of the right side
			(let* ((right (cons (LIST2BTREE (second (SPLIT-LIST LEAVES))) NIL))) ;cons with nil to add outer parenthesis
			(cons (LIST2BTREE (first (SPLIT-LIST LEAVES))) right)))))

;Explanation: The idea here with BTREE2LIST is to go as deep as we can on the first of the list, recursively;
;once we locate an atom (which we turn into a single-atom list) or a two-atom list, we can append that with the
;rest of the list-converted TREE
(defun BTREE2LIST (TREE)
	(cond ((atom TREE) 
			(cons TREE NIL)) ;if the tree is just an atom, change it to a list
		((and (= (length TREE) 2) (atom (second TREE))) ;if the TREE has two atoms (length 2 with second as atom), return both as a list
			(list (first TREE) (second TREE)))
		(t ;otherwise, append atoms from left side to the rest of the list recursively
			(append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))))
			
;SAMPLE TEST CASES
;(SUB-LIST '(a b c d) 0 3)
;(SUB-LIST '(a b c d) 3 1)
;(SUB-LIST '(a b c d) 2 0)

;(SPLIT-LIST '(a b c d))
;(SPLIT-LIST '(a b c d e))
;(SPLIT-LIST '(a b c d e f))

;(LIST2BTREE '(1))
;(LIST2BTREE '(1 2))
;(LIST2BTREE '(1 2 3))
;(LIST2BTREE '(1 2 3 4))
;(LIST2BTREE '(1 2 3 4 5 6 7))
;(LIST2BTREE '(1 2 3 4 5 6 7 8))

;(BTREE2LIST 1)
;(BTREE2LIST '(1 2))
;(BTREE2LIST '(1 (2 3)))
;(BTREE2LIST '((1 2) (3 4)))
;(BTREE2LIST '((1 (2 3)) ((4 5) (6 7))))
;(BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))
