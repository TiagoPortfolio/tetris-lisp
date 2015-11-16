(defun depth-first-search (start goal been-list moves)
	(cond ((equal start goal) 
		(reverse (cons start been-list)))
	(t (try-moves start goal been-list moves moves))))

; Try-moves scans down the list of moves in moves-to-try, 
; attempting to generate a child state.  If it produces 
; this state, it calls depth-first-search to complete the search.

(defun try-moves (start goal been-list moves-to-try moves)
	(cond ((null moves-to-try) nil)
		((member start been-list :test #'equal) nil)
		(t (let ((child (funcall (car moves-to-try) start)))
			(if child 
				(or (depth-first-search (funcall (car moves-to-try) start)
					goal
					(cons start been-list)
					moves)
				(try-moves start goal been-list (cdr moves-to-try) moves))
				(try-moves start goal been-list (cdr moves-to-try) moves))))))

; run-depth-first calls depth-first-search, initializing the been-list to ().
(defun run-depth (start goal moves)
	(depth-first-search start goal () moves))