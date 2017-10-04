(DEFUN Round ( gameState)
	( COND
		( ( <= 2 (length gameState)) ;meaning no round items created
			();generate dominos and create roundstate
			(playRound gameState))
		(T 
			(playRound gameState))
	)
)

(DEFUN playRound( gameState )
	( COND
		(( =  0 (length (getStock gameState)) )
			(print "Round Ended"))  ;;should be returning a list of scores
		(T  
			() );round continues
	)
)


(DEFUN generateRound(gameState)
	( LET* (
			(dominos (shuffleDominos  ( generateAllDominos(generateNums 0 6))))
		)
		( APPEND
				gameState
				( LIST (getNElementsFromFront 8 dominos) '0 )
				( LIST (getNElementsFromFront 8 (removeNElementsFromFront 8 dominos )) '0)
				( LIST (LIST 'L 'R)  (removeNElementsFromFront 16 dominos) '( ) '( ))
		)
	)
)

(DEFUN getTournamentScore(gameState)
	( getIthElement 1 gameState )
)

(DEFUN getRoundNo(gameState)
	( getIthElement 2 gameState )
)

(DEFUN getComputerHand(gameState)
	 ( getIthElement 3 gameState )
)

(DEFUN getComputerScore(gameState)
	( getIthElement 4 gameState )
)

(DEFUN getHumanHand(gameState)
	( getIthElement 5 gameState )
)

(DEFUN getHumanScore(gameState)
	( getIthElement 6 gameState )
)

(DEFUN getLayout(gameState)
	( getIthElement 7 gameState )
)

(DEFUN getStock(gameState)
	( getIthElement 8 gameState )
)

(DEFUN getPlayerPassed(gameState)
	( getIthElement 9 gameState )
)

(DEFUN getTurn(gameState)
	( getIthElement 10 gameState )
)




(DEFUN generateNums(start end)
	( COND 
		((= start end)
			(LIST end))
		(T 
			( CONS  start (generateNums ( + start 1) end) ))
	)
)

(DEFUN generateDominosFor(pip pips)
	( COND
		( ( null pips) 
			())
		(T 
			(CONS ( LIST pip (first pips))	(generateDominosFor pip (rest pips))))
	)
)	

(DEFUN generateAllDominos(pips)
	( COND
		( (null pips) 
			())
		(T
			( APPEND (generateDominosFor (first pips) pips ) (generateAllDominos (rest pips)) ))
	)
)

(DEFUN getIthElement(i listA)
	( COND
		((< i 1)
			())
		((= i 1) 
			(first listA))
		(T
			(getIthElement (- i 1) (rest listA)))
	)
)

(DEFUN getNElementsFromFront(n listA)
	( COND
		( (OR (= n 0) (= (length listA) 0)) 
			())
		(T 
			(CONS (first listA) (getNElementsFromFront (- n 1) (rest listA))))
	)
)

(DEFUN removeIthElement(i listA)
	( COND
		((< i 1)
			())
		((= i 1) 
			(rest listA))
		(T
			( CONS (first listA) (removeIthElement (- i 1) (rest listA))))
	)
)

(DEFUN removeNElementsFromFront(n listA)
	( COND
		( (OR (= n 0) (= (length listA) 0)) 
			listA )
		(T 
			(removeNElementsFromFront(- n 1) (rest listA)))
	)
)


(DEFUN shuffleDominos(dominos)
	( COND
		( (null dominos)
			())
		(T
			(LET* 
				((x (random (length dominos))))
				( CONS 
					(getIthElement (+ x 1) dominos) 
					(shuffleDominos 
						(removeIthElement (+ x 1) dominos)) 
				))
		)
	)
)

;(print ( shuffleDominos  ( generateAllDominos(generateNums 0 6)) ))

(print (generateRound '()))

;giant loop with parameter a giant list storing all layout, stock, human hand, computer hand, human score, computer score, turn and passed
;need a functions to take these as parameters and return it back
;functions to edit this list and pass it around
;maybe name it gameState??