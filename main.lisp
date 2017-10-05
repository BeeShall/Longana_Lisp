(DEFUN Round ( gameState)
	( COND
		( ( <= 2 (length gameState)) ;meaning no round items created
			(playRound (generateRound gameState )))
		(T 
			(playRound gameState))
	)
)

(DEFUN playRound( gameState )
	( COND
		(( =  0 (length (getStock gameState)) )
			(print "Round Ended"))  ;;should be returning a list of scores
		(T  
			() );round continues Impldment game logic now
			;generate the engine first
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

(DEFUN displayRoundState(gameState)
		(terpri)
		(write-line "----------------------------------------------------------")
		(write-line "Current Layout:")
		(princ (getLayout gameState))
		(terpri)
		(terpri)
		(write-line "Current Stock: ")
		(princ (getStock gameState))
		(terpri)
		(write-line "----------------------------------------------------------")
		(terpri)

)

(DEFUN getHumanMove(gameState)
	(terpri)
	(princ "Human Hand: ")
	(print(getHumanHand gameState))
	(terpri)
	(terpri)
	(write-line "Please enter the domino you'd like to play enclosed in ( ) with side (L/R) as the first element. E.g. (L 1 6):: ")
	( LET* (
			(move (read))
		)
		( COND
			((listp move) 
				( COND(
					(OR (eq 'L (first move) ) (eq 'R (first move)))
					(LET*(
						(hand (getHumanHand gameState))
						(layout (getLayout gameState))
					)
					( COND(
							(position (rest move ) hand :test #'equal)
							(terpri)
							(write-line "You have the domino in hand!")
							
						)
						(T 
							(terpri)
							(write-line "You don't have that domino. Please select a domino in hand!")
							(getHumanMove gameState)) )
					;validate the move
					;check if is in hand
					;check if placable
					))
					(T 
						(terpri)
						(write-line "Plese select a valid side!")
						(getHumanMove gameState)
					)
				)
			)
			(T 
				(terpri)
				(write-line "Invalid move. Please follow the input syntax!!")
				(getHumanMove gameState)
			)
		)
	)
)

(DEFUN validateMove (move layout)
	( COND (
		(= 'L (first move))
		(
			;left stuff
		) 
		)
		(T 
			(
				;right stuff
			))
	)
)

(DEFUN getTournamentScore(gameState)
	( elt gameState 0)
)

(DEFUN getRoundNo(gameState)
	( elt gameState 1 )
)

(DEFUN getComputerHand(gameState)
	 ( elt gameState 2 )
)

(DEFUN getComputerScore(gameState)
	( elt gameState 3 )
)

(DEFUN getHumanHand(gameState)
	( elt gameState 4 )
)

(DEFUN getHumanScore(gameState)
	( elt gameState 5 )
)

(DEFUN getLayout(gameState)
	( elt gameState 6)
)

(DEFUN getStock(gameState)
	( elt gameState 7 )
)

(DEFUN getPlayerPassed(gameState)
	( elt gameState 8 )
)

(DEFUN getTurn(gameState)
	( elt gameState 9 )
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
					(elt dominos x) 
					(shuffleDominos 
						(removeIthElement (+ x 1) dominos)) 
				))
		)
	)
)

;(print ( shuffleDominos  ( generateAllDominos(generateNums 0 6)) ))
;(Round (LIST '200 '1) )
(getHumanMove (generateRound (LIST '200 '1)))
;(print(LIST 'l 0 0))
(terpri)
;(print(eq 'l (first (read))))
;giant loop with parameter a giant list storing all layout, stock, human hand, computer hand, human score, computer score, turn and passed
;need a functions to take these as parameters and return it back
;functions to edit this list and pass it around
;maybe name it gameState??