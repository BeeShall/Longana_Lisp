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
			() );round continues Implement game logic now
			;generate the engine first
	)
)


(DEFUN generateRound(gameState)
	( LET* (**
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

(DEFUN determineFirstPlayer(gameState engine)
	( COND(
		(null (first (last gameState)))
		(
			;no first player or engine decided yet
			COND (
				;if human has engine
				( find engine (getHumanHand gameState) :test #'equal)
				(terpri)
				(write "Human has the engine!")
				(terpri)
				;remove engine from hand
				;update layout
				;set passed to false and next player to computer
				(reverse (APPEND '()  ( CONS "Computer" (rest (rest (reverse (updateLayout (updateHumanHand gameState (remove engine (getHumanHand gameState)  :test #'equal )) (LIST 'L engine 'R))))))  ))
			)
			(
				;if computer has engine
				( find engine (getComputerHand gameState) :test #'equal)
				(terpri)
				(write "Computer has the engine!")
				(terpri)
				;same logic as above
				(reverse (APPEND '()  (CONS "Human" (rest (rest (reverse (updateLayout (updateComputerHand gameState (remove engine (getComputerHand game )  :test #'equal )) (LIST 'L engine 'R))))))))
			)
			(T ;neither of them have the engine
				(LET* (
					(updatedState ( updateStock (updateHumanHand gameState (CONS (first (getStock gameState)) (getHumanHand gameState)) ) (rest (getStock gameState) ) ))
					) 
					(terpri)
					(princ "Human drew ")
					(princ (first (getHumanHand updatedState)))
					(terpri)

					(terpri)
					(princ "Computer drew ")
					(princ (first (getStock updatedState)))
					(terpri)
					( determineFirstPlayer ( updateStock (updateComputerHand updatedState (CONS (first (getStock updatedState)) (getComputerHand updatedState))) (rest (getStock updatedState)) ) engine) 
					
				)
			)
		)
		)
		(T 
			;engine has already been decided
			gameState
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
			;validating if the input format is correct
			((listp move) 
				( COND(
					;validating if the given side is correct
					(OR (eq 'L (first move) ) (eq 'R (first move)))
					(LET*(
						(hand (getHumanHand gameState))
						(layout (getLayout gameState))
					)
					( COND(
						;checking for if chosen domino is in hand
							(position (rest move ) hand :test #'equal)
							(terpri)
							(write-line "You have the domino in hand!")
							;need let on placedomino
							( LET* (
									(resultMove (validateMove move layout))
									)
									( COND (
										(null resultMove)
										(terpri)
										(write-line "You cannot place that domino on that side! Please try again!")
										(getHumanMove gameState)
									)
									(T 
										;update pass and turn
										;implement pass concept
									 	(updateHumanHand (updateLayout gameState (placeDomino resultMove layout) ) (remove (rest move) hand :test #'equal))
									)
								))
							
							;use the utility functions
							
						)
						(T 
							(terpri)
							(write-line "You don't have that domino. Please select a domino in hand!")
							(getHumanMove gameState)) )
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

(DEFUN displayUserMenu()
	(terpri)
	(write-line "----------------------------------------------------------")
	(write-line "Please select one of the following options: ")
	(write-line "1. Make a move")
	(write-line "2. Draw from stock")
	(write-line "3. Pass")
	(write-line "4. Hint??")
	(write-line "----------------------------------------------------------")
	(terpri)

)

(DEFUN validateMove (move layout)
	( COND (
		(eq 'L (first move))
		(
			;left stuff
			LET*(
				(leftDomino (first(rest layout)))
			)
			( COND(
					(= (elt move 1) (first leftDomino))
					(CONS (first move) (reverse (rest move)))
				)
				(
					(= (elt move 2) (first leftDomino))
					move
				)
				(T 
					()
				)
			)
		) 
		)
		(T 
			(
				;right stuff
				LET*(
					(rightDomino ( elt layout (- (length layout) 2) )
				))
				( COND(
					(= (elt move 1) (second rightDomino))
					move
				)
				(
					(= (elt move 2) (second rightDomino))
					(CONS (first move) (reverse (rest move)))
				)
				(T 
					()
				))
			)
		)
	)
)

(DEFUN placeDomino(move layout)
	( COND (
		(eq 'L (first move))
		(
			;left stuff
			CONS (first layout) (CONS (rest move) (rest layout))
		) 
		)
		(T 
			(
				;right stuff
				reverse (CONS (first (reverse layout)) (CONS (rest move) (rest (reverse layout))))
			)
		)
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

(DEFUN updateComputerHand(gameState hand)
	(substitute hand ( elt gameState 2 ) gameState :test #'equal)
)


(DEFUN updateComputerHand(gameState hand)
	(substitute hand ( elt gameState 2 ) gameState :test #'equal)
)

(DEFUN getHumanHand(gameState)
	( elt gameState 4 )
)

(DEFUN updateHumanHand(gameState hand)
	(substitute hand ( elt gameState 4 ) gameState :test #'equal)
)

(DEFUN getHumanScore(gameState)
	( elt gameState 5 )
)

(DEFUN getLayout(gameState)
	( elt gameState 6)
)

(DEFUN updateLayout(gameState layout)
	(substitute layout ( elt gameState 6 ) gameState :test #'equal)
)

(DEFUN getStock(gameState)
	( elt gameState 7 )
)

(DEFUN updateStock(gameState stock)
	(substitute stock ( elt gameState 7 ) gameState :test #'equal)
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


(terpri)
;(trace determineFirstPlayer)
(print (determineFirstPlayer (generateRound (LIST '200 '1)) '(6 6)))



;(print (placeDomino '() (LIST 'L '(a b) '(c d) 'R)) )
;(print (substitute '( a b c) '(1 2) (LIST '(1 2) '(3 4) '(7 8)) :test #'equal))