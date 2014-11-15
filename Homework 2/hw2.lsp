;Nathan Tung (004-059-195)

;Explanation: the idea here is to run DFS on the first part of the tree recursively, then on the rest. 
;Repeat until we hit the base cases, where either there is no TREE or there is only an atom (which we put into a list). Append the results.
(defun DFS (TREE)
	(cond ((null TREE) NIL) ;if there is no tree left, return NIL
		((atom TREE) (list TREE)) ;if there is an atom left, return it as a list
		((= (length TREE) 1) (DFS (first TREE))) ;if there's one list unit left in TREE, run DFS on its first unit
		(t (append (DFS (first TREE)) (DFS (rest TREE)))))) ;otherwise, append the result of the DFS'd first and rest

;Explanation: first we write limited DFS to traverse up to a certain depth.
;DFID then utilizes LDFS recursively to traverse through varying depths.
(defun DFID (TREE M)
	(cond ((null TREE) NIL) ;if there is no TREE, return NIL
		((< M 0) NIL)
		(t (append (DFID TREE (- M 1)) (LDFS TREE M))))) ;otherwise, use limited DFS to append last and current traversals
	
(defun LDFS (TREE L)
	(cond ((null TREE) NIL) ;if there is no TREE left, return NIL
		((< L 0) NIL) ;if L<0, we are done with traversing
		((atom TREE) (list TREE)) ;if there is an atom left, return it as a list
		((= (length TREE) 1) (LDFS (first TREE) (- L 1))) ;if there's one list unit left in TREE, run LDFS on its first unit
		(t (append (LDFS (first TREE) (- L 1)) (LDFS (rest TREE) L)))))
			;if TREE has more than two parts, append the LDFS result of the first with the LDFS result of the rest

			
;Missionary-Cannibal Problem (comments provide more details)

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(cond ((not (= (length s) 3)) NIL) ;if current state does not have 3 items, return NIL
		((not (= (first s) 3)) NIL) ;if number of missionaries is not 3, return NIL
		((not (= (second s) 3)) NIL) ;if number of cannibals is not 3, return NIL
		(t (not (third s))))) ;return the opposite of the boat's position (since NIL returns T)
	;same as using ...cond ((equal '(3 3 NIL)) t), but from a constraints point-of-view

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	(cond ((> (+ m c) 2) NIL) ;return NIL if we try to move more than 2 people
		((< (first s) m) NIL) ;return NIL if we try to move more missionaries than we have
		((< (second s) c) NIL) ;return NIL if we try to move more missionaries than we have
		((and (> (- (first s) m) 0) (< (- (first s) m)  (- (second s) c))) NIL)
			;return NIL if there is at least 1 missionary remaining here, and cannibals outnumber missionaries on this side
		((and (> (+ (- 3 (first s)) m) 0) (< (+ (- 3 (first s)) m) (+ (- 3 (second s)) c))) NIL)
			;return NIL if there is at least 1 missionary on the other side, and cannibals outnumber missionaries on this side
		(t (list (list (+ (- 3 (first s)) m) (+ (- 3 (second s)) c) (not (third s)))))))
			;otherwise, return the state of the opposite side


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
	(append (next-state s 1 0) (next-state s 0 1) (next-state s 1 1) (next-state s 2 0) (next-state s 0 2)))
		;append all possible new states, assuming next-state successfully verifies that they are valid


; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
	(cond ((= (length states) 0) NIL) ;if there are no states and we haven't found a match, return NIL
		((equal s (first states)) T) ;if s equals first element in states, then s is a member of states
		(t (on-path s (rest states))))) ;otherwise, try running on-path with the rest of states

		
; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). path is a	
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
	(cond ((= (length states) 0) NIL) ;return NIL if there are no more possible successor states to look at
		((mc-dfs (first states) path) (mc-dfs (first states) path))
			;run mc-dfs on the first state; if it exists, return it
		(t (mult-dfs (rest states) path))))
			;otherwise, get rid of that first state and call mult-dfs on the rest

			
; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
	(cond ((final-state s) (append path (list s))) ;if s is already goal state, return the path we have so far appended with goal state
		((not (on-path s path)) (mult-dfs (succ-fn s) (append path (list s))))
			;if current state s has not been visited in path, we can visit it, running dfs on it by first calling mult-dfs
		(t NIL))) ;if current state s has been visited, this branch does not lead to a solution, so return NIL
