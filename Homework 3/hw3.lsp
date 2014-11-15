; Nathan Tung
; 004059195
;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly effect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; effect your score).
;  
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
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
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
;
(defun goal-test (s)
  (cond ((null s) t) ;if the list is empty, all inner lists contained no loose blocks
	((equal (length s) 1) (partial-goal-test (first s))) ;check if the list inner list is a goal list
	(t (and (partial-goal-test (first s)) (goal-test (rest s))))) ;check if first inner list is goal, then run on rest
);end defun

; This helper function checks to make sure that a list of atoms does not have any unfinished boxes (or 2's)
(defun partial-goal-test (s)
	(cond ((null s) t) ;if the list is empty, we met no 2's
		((equal (length s) 1) (not (isBox (first s)))) ;check if the last number is 2
		(t (and (not (isBox (first s))) (partial-goal-test (rest s))))) ;check if the first element is 2, then run on rest of list
)

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
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'up) (try-move s 'right) (try-move s 'down) (try-move s 'left))) ;try moves in all four directions
	 )
    (cleanUpList result);end
   );end let
  );
  
(defun get-square (S r c)
	(cond ((null S) 1) ;if there is no state, return wall value
		((atom S) S) ;if the state is an atom (number), return that number
		((or (> r (maxRow S)) (> c (maxCol S))) 1) ;if row or column is beyond what the states lists, return wall value
		((or (< r 0) (< c 0)) 1) ;if row or column is negative, return wall value
		((> r 0) (get-square (rest S) (- r 1) c)) ;get rid of the first r rows
		((> c 0) (get-square (cons (rest (first S)) (rest S)) r (- c 1))) ;get rid of the first c atoms in the first inner list
		(t (get-square (first (first S)) r c))) ;otherwise, the element we want should be at the front
		;filter it through get-square one last time to make sure it's within bounds
)

(defun set-square (S r c v)
	(cond ((null S) nil)
	((> r 0) (cons (first S) (set-square (rest S) (- r 1) c v))) ;cons the first r rows unchanged with the modified rest of S
	((> c 0) (let* ((x (set-square (cons (rest (first S)) (rest S)) r (- c 1) v))) (cons (cons (first (first S)) (first x)) (rest x))))
		;cons the first element in first inner list back to its position after running set-square for decremented c
	(t (cons (cons v (rest (first S))) (rest S)))) ;otherwise, change the element at the very front and cons everything back
		;filter it through get-square one last time to make sure it's within bounds
)

(defun try-move (S D)
	(let* ((r (second (getKeeperPosition S 0))) (c (first (getKeeperPosition S 0)))) ;grab the row and column of the keeper
	(cond ((illegal-move S D r c) nil) ;if illegal move due to wall or immovable block, return nil
		(t (post-move-state S D r c)) ;otherwise, return the new state
	))
)

(defun illegal-move (S D r c)
	(cond
		((equal D 'up)
			(cond ((isWall (get-square S (- r 1) c)) t) ;top blocked by wall
				((and (isBox (get-square S (- r 1) c)) (or (isBox (get-square S (- r 2) c)) (isWall (get-square S (- r 2) c)))) t) ;top is immovable block
				(t nil))) ;otherwise, feel free to move
		((equal D 'left)
			(cond ((isWall (get-square S r (- c 1))) t) ;blocked by wall
				((and (isBox (get-square S r (- c 1))) (or (isBox (get-square S r (- c 2))) (isWall (get-square S r (- c 2))))) t) ;immovable block
				(t nil))) ;else, move
		((equal D 'down)
			(cond ((isWall (get-square S (+ r 1) c)) t) ;blocked by wall
				((and (isBox (get-square S (+ r 1) c)) (or (isBox (get-square S (+ r 2) c)) (isWall (get-square S (+ r 2) c)))) t) ;immovable block
				(t nil))) ;else, move
		((equal D 'right)
			(cond ((isWall (get-square S r (+ c 1))) t) ;blocked by wall
				((and (isBox (get-square S r (+ c 1))) (or (isBox (get-square S r (+ c 2))) (isWall (get-square S r (+ c 2))))) t) ;immovable block
				(t nil))) ;else, move
		(t t)) ;there is no other legal move
)

(defun block-move (S D r c) ;the position of the moved block (if any)
	(cond
		((equal D 'up)
			(cond ((or (isBox (get-square S (- r 1) c)) (isBoxStar (get-square S (- r 1) c))) (set-square S (- r 2) c box)) ;move the block if applicable
				(t S))) ;otherwise, make no changes
		((equal D 'left)
			(cond ((or (isBox (get-square S r (- c 1))) (isBoxStar (get-square S r (- c 1)))) (set-square S r (- c 2) box)) ;move the block if applicable
				(t S))) ;otherwise, make no changes
		((equal D 'down)
			(cond ((or (isBox (get-square S (+ r 1) c)) (isBoxStar (get-square S (+ r 1) c))) (set-square S (+ r 2) c box)) ;move the block if applicable
				(t S))) ;otherwise, make no changes
		((equal D 'right)
			(cond ((or (isBox (get-square S r (+ c 1))) (isBoxStar (get-square S r (+ c 1)))) (set-square S r (+ c 2) box)) ;move the block if applicable
				(t S))) ;otherwise, make no changes
	)
)

(defun keeper-move (S D r c) ;the position where the keeper ends up
	(cond
		((equal D 'up)
			(cond ((or (isStar (get-square S (- r 1) c)) (isBoxStar (get-square S (- r 1) c))) (set-square (block-move S D r c) (- r 1) c keeperstar))
				;if moving onto a star or box star, the position becomes keeper star
				(t (set-square (block-move S D r c) (- r 1) c keeper)))) ;otherwise, simply add keeper to the position
		((equal D 'left)
			(cond ((or (isStar (get-square S r (- c 1))) (isBoxStar (get-square S r (- c 1)))) (set-square (block-move S D r c) r (- c 1) keeperstar))
				;if moving onto a star or box star, the position becomes keeper star
				(t (set-square (block-move S D r c) r (- c 1) keeper)))) ;otherwise, simply add keeper to the position
		((equal D 'down)
			(cond ((or (isStar (get-square S (+ r 1) c)) (isBoxStar (get-square S (+ r 1) c))) (set-square (block-move S D r c) (+ r 1) c keeperstar))
				;if moving onto a star or box star, the position becomes keeper star
				(t (set-square (block-move S D r c) (+ r 1) c keeper)))) ;otherwise, simply add keeper to the position
		((equal D 'right)
			(cond ((or (isStar (get-square S r (+ c 1))) (isBoxStar (get-square S r (+ c 1)))) (set-square (block-move S D r c) r (+ c 1) keeperstar))
				;if moving onto a star or box star, the position becomes keeper star
				(t (set-square (block-move S D r c) r (+ c 1) keeper)))) ;otherwise, simply add keeper to the position
		)
)

(defun post-move-state (S D r c) ;the position where the keeper was
	(cond
		((isKeeperStar (get-square S r c)) (set-square (keeper-move S D r c) r c star)) ;if keeper was on star, change it to be star
		(t (set-square (keeper-move S D r c) r c blank)) ;otherwise, change it to be blank
))

(defun maxRow (s)
	(- (length s) 1) ;return max index of rows
)

(defun maxCol (s)
	(- (length (first s)) 1) ;return max index of column
)
			
; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0 ;simply return 0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; The heuristic IS admissible, since h1 (the number of boxes left to push) is ALWAYS less
; than the leftover number of moves necessary to reach the final goal state.
; In other words, it takes at least one step to move each box to its goal state.
(defun h1 (s)
	(count-loose-boxes s) ;count boxes that are not on goal position
  )
  
(defun count-loose-boxes (s)
	(cond ((null s) 0) ;if list is empty, there are no loose boxes
		((atom s) (cond ((isBox s) 1) ;if the atom is a loose box, return 1
			(t 0))) ;otherwise, return 0
		((equal (length s) 1) (count-loose-boxes (first s))) ;if there's one atom/list left, count that list
		(t (+ (count-loose-boxes (first s)) (count-loose-boxes (rest s))))) ;otherwise, add up box count in first and rest
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

(defun h004059195 (s)
	(h1 s) ;going back to the extremely basic but certainly admissible heuristic
	; upon calculating distances for a more accurate heuristic, we save space complexity but at a HUGE expense in time
)

;(defun h004059195 (s)
;	(let* ((numBox (h1 s)) (rKeep (second (getKeeperPosition s 0))) (cKeep (first (getKeeperPosition s 0)))
;		(rBox (second (getBoxPosition s 0))) (cBox (first (getBoxPosition s 0)))) ;find positions of a given box and the keeper
;	(+ numBox (distance rKeep cKeep rBox cBox) )) ;build onto h1 by calculating distance the keeper must move to a box
;		; this is admissible, since the player must at some point reach that box, in addition to moving all boxes to their goals
;)
;
;(defun distance (r1 c1 r2 c2)
;	(+ (cond ((> c2 c1) (- c2 c1)) (t (- c1 c2))) (cond ((> r2 r1) (- r2 r1)) (t (- r1 r2))))
;		;use a makeshift abs-value to determine distance between two positions
;)
;
;(defun getBoxColumn (r col)
;  (cond ((null r) nil)
;	(t (if (isBox (car r))
;	       col
;	     (getBoxColumn (cdr r) (+ col 1))
;	     );end if
;	   );end t
;	);end cond
;  )
;  
;(defun getBoxPosition (s row) ;returns as (c r)
;  (cond ((null s) nil)
;	(t (let ((x (getBoxColumn (car s) 0)))
;	     (if x
;		 ;keeper is in this row
;		 (list x row)
;		 ;otherwise move on
;		 (getBoxPosition (cdr s) (+ row 1))
;		 );end if
;	       );end let
;	 );end t
;	);end cond
;  );end defun
;  
;  (defun getGoalColumn (r col)
;  (cond ((null r) nil)
;	(t (if (isStar (car r))
;	       col
;	     (getGoalColumn (cdr r) (+ col 1))
;	     );end if
;	   );end t
;	);end cond
;  ) 
;  
;(defun getGoalPosition (s row) 
;  (cond ((null s) nil)
;	(t (let ((x (getGoalColumn (car s) 0)))
;	     (if x
;		 ;keeper is in this row
;		 (list x row)
;		 ;otherwise move on
;		 (getGoalPosition (cdr s) (+ row 1))
;		 );end if
;	       );end let
;	 );end t
;	);end cond
;  );end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
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