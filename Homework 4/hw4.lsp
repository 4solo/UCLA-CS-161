;Nathan Tung (004-059-195)

;Explanation: parse out basic values that are known to give errors to increase efficiency; then use DFS to solve this CSP
(defun QUEENS (N) 
	(cond ((<= N 0) nil) ;ignore impossible parameters
		;((oddp N) (append (multDFS 2 (- N 1) ()) (list N))) ;if odd, append N to the list of previous board that starts at col=2
			;not as efficient as I thought it would be
		(t (multDFS 1 N ())) ;use DFS to try all valid solutions until we get the right answer or nil
	)
)

(defun multDFS (col n ans)
	(cond ((> col n) nil) ;return nil if none of the valid columns we looked at worked
		((queenDFS col n ans)) ;return a list with a valid queen added, if possible
		(t (multDFS (+ col 1) n ans)) ;otherwise, try repeating with the next column
	)
)
		
(defun queenDFS (col n ans)
	(cond ((= (length ans) n) ans) ;return ans if queens don't attack each other and it is of the right length
		((isValid (+ 1 (length ans)) col 1 ans) (multDFS 1 n (append ans (list col)))) ;if col is valid, run multDFS with it appended
		(t nil) ;otherwise, end here and backtrack (try with a different col)
	)
)

;Return true if the queen with coordinates (nextrow, nextcol) is a valid addition to the list ans
;That is, is this queen valid with all coordinate combinations of (currentrow, (first ans)), (currentrow, (second ans)), etc.
(defun isValid (nextrow nextcol currentrow ans)
	(cond ((NULL ans) t) ;if ans is empty, we have checked everything and no contradiction exists, return true
		((not (twoQueensValid nextrow nextcol currentrow (first ans))) nil) ;if conflict arises with one queen in list, return false
		(t (isValid nextrow nextcol (+ currentrow 1) (rest ans))) ;otherwise, keep checking until ans is empty
	)
)

;Return whether two queens with specified coordinates can coexist
(defun twoQueensValid (r1 c1 r2 c2)
	(cond ((or (= r1 r2) (= c1 c2)) nil) ;if queens are on the same row or col, return nil
		((= (absvalDifference r1 r2) (absvalDifference c1 c2)) nil) ;if they are on the same diagonal, return nil
		(t t) ;otherwise, return true
	)
)

;Return the absolute value of the difference between two numbers
(defun absvalDifference (x y)
	(cond ((> x y) (- x y))
		(t (- y x))
	)
)