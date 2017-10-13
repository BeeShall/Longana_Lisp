(DEFUN loadFile ()
	(write-line "Please enter a filename: ")
    (LET* (
		(in (open (read) :if-does-not-exist nil))
        (data ( COND (in (read in))
            (T
				(terpri)
                (write-line "File does not exist.")
                (loadFile)
			)))
		)
		(COND (
			(null in)
			NIL
		)
		(T 
			(close in)
		))
        data
	)
)

(DEFUN Longana ()
 	(terpri)
	(terpri)
	(write-line "Welcome to Lonagana!")
	(terpri)
	(write-line "Would you like to load a game? (Y for yes, anything else for no) ")
	(LET* (
		(choice (read))
	)
	(COND (
		(eq choice 'Y )
		(playTournament (loadFile) 0 0)
		)
		(T
			(Tournament)
		))
	)
)

(DEFUN Tournament( )
	( write-line "Please enter a tounament score: " )
	(LET* (
		(choice (read)))
		(COND(
			(listp choice)
			(write-line "Please input a number, not a list!")
			(Tournament)
		)
		(T 
			(GameRound (LIST choice 1) (getEngineFromRoundCount 0 7) )
		))
	)

)

(DEFUN playTournament (gameState humanScore computerScore) 
	( COND (
		(> (length gameState) 2)
		;its a loaded game
		(LET* (
			(roundResults (GameRound gameState (getEngineFromRoundCount (getRoundNo gameState) 7) ))
			)
			( COND (
				(null roundResults)
				()
			)
			(T 
				(playTournament 
				(LIST (first roundResults) (+ 1 (getRoundNo roundResults))) 
				(+ humanScore (getHumanScore gameState) (getHumanScore roundResults)) 
				(+ computerScore (getComputerScore gameState) (getComputerScore roundResults))
			)
			))	
		)
	)
	(T
		(terpri)
		(write-line "Tournament scores: ")
		(terpri)
		(princ "Human: ")
		(princ humanScore)
		(terpri)
		(princ "Computer : ")
		(princ computerScore)
		(terpri)
		( COND (
			(AND ( > humanScore 0 ) ( > humanScore computerScore))
			(terpri)
			(write-line "Human won the tournament! ")
		)
		(
			(AND ( > computerScore 0 ) ( > computerScore humanScore))
			(terpri)
			(write-line "Computer won the tournament! ")
		)
		(T 
			(LET* (
				(roundResults (GameRound gameState (getEngineFromRoundCount (getRoundNo gameState) 7) ))
			)
			( COND (
					(null roundResults)
					()
				)
				(T
					(playTournament 
						(LIST (first roundResults) (+ 1 (getRoundNo roundResults))) 
						(+ humanScore (getHumanScore roundResults))
						(+ computerScore (getComputerScore roundResults)))
				)
			)
		))) 
	))
)

(DEFUN getEngineFromRoundCount (roundCount pipCount) 
	( LET*
		( 
			(tempPip (mod roundCount pipCount))
		)
		( COND (
			(= 0 tempPip)
			'(0 0)
		)
		(T 
			(LIST (- pipCount tempPip) (- pipCount tempPip))
		))
	)
)


(DEFUN GameRound ( gameState engine )
	( COND(
		(askToSave (generateRound gameState ))
		()
		) 
		(T 
			( COND
				( ( <= (length gameState) 2) ;meaning no round items created
					(playRound (determineFirstPlayer (generateRound gameState ) engine) ))
				(T 
					(playRound (determineFirstPlayer gameState engine) ))
			)
		)
	)
)

(DEFUN playRound( gameState )
	( COND
		( ( =  0 (length (getHumanHand gameState)) )
			(write-line "Round Ended")
			(write-line "Human won"))  ;;should be returning a list of scores
		(( =  0 (length (getComputerHand gameState)) )
			(write-line "Round Ended")
			(write-line "Computer won"))  ;;should be returning a list of scores
			;need the logic for two passes and determining the winner
		(T  
			(displayRoundState gameState)
			( COND (
				(string= ( getTurn gameState) "Human")
				(write-line "Its your turn, yayyyyyy!")
				(terpri)
				( COND(
					(askToSave gameState)
					NIL
				)(T 
					(playRound ( getHumanMove gameState '()))
				) )	
			)
			(T
				(write-line "Its computer's turn!")
				(terpri)
				(playRound  (reverse (CONS "Human" (rest (reverse gameState) ))))
			) )
		)
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

(DEFUN determineFirstPlayer(gameState engine)
	( COND(
		(null (first (last gameState)))
		;meaning if none of the players have engine, which means turns hasn't been set yet
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
				(reverse (CONS "Computer"  ( CONS '() (rest (rest (reverse (updateLayout (updateHumanHand gameState (remove engine (getHumanHand gameState)  :test #'equal )) (LIST 'L engine 'R))))))  ))
			)
			(
				;if computer has engine
				( find engine (getComputerHand gameState) :test #'equal)
				(terpri)
				(write "Computer has the engine!")
				(terpri)
				;same logic as above
				(reverse (CONS "Human" (CONS '()  (rest (rest (reverse (updateLayout (updateComputerHand gameState (remove engine (getComputerHand gameState )  :test #'equal )) (LIST 'L engine 'R))))))))
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
		;print layout in 3 lines like you did in c++
		(princ (getLayout gameState))
		(terpri)
		(terpri)
		(write-line "Current Stock: ")
		(princ (getStock gameState))
		(terpri)
		(write-line "----------------------------------------------------------")
		(terpri)

)


(DEFUN displayUserMenu()
	(terpri)
	(write-line "----------------------------------------------------------")
	(write-line "Please select one of the following options: ")
	(write-line "1. Make a move")
	(write-line "2. Draw from stock")
	(write-line "3. Hint??")
	(write-line "----------------------------------------------------------")
	(terpri)

)

(DEFUN getHumanMove(gameState hasAlreadyDrawn)
	(terpri)
	(write-line "----------------------------------------------------------")
	(princ "Human Hand: ")
	(print(getHumanHand gameState))
	(terpri)
	(write-line "----------------------------------------------------------")
	(terpri)
	(displayUserMenu)
	(write-line "Your choice: ")
	(LET* (
		(choice (read)))
		(COND(
			(listp choice)
			(write-line "Please input a number, not a list!")
			(getHumanMove gameState hasAlreadyDrawn)
		)
		(
			(= choice 1)
			(playHuman gameState)
		)
		(
			(= choice 2) ;if already drawn check
			(COND(
				(= 0 (length (getStock gameState)))
				(write-line "Stock is empty! So you passed!")
				(reverse (CONS  "Computer"  (CONS 'T (rest (rest (reverse gameState))))))
			)
			( 
				(eq T hasAlreadyDrawn)
				(write-line "You already drew a tile from stock! So you passed!")
				(reverse (CONS "Computer" (CONS 'T (rest (rest (reverse gameState))))))
			)
			( T 
				(princ "You drew ")
				(princ (first (getStock gameState)))
				(princ " from the stock!")
				(terpri)
				(getHumanMove ( updateStock (updateHumanHand gameState (CONS (first (getStock gameState)) (getHumanHand gameState)) ) (rest (getStock gameState)) ) T)
			))
		)
		(
			(= choice 3)
			(print (getAllPossibleMoves (getLayout gameState) (getHumanHand gameState) (getPlayerPassed gameState) 'L))
			(write-line "Hint logic hasn't been implemented yet!")
			gameState
		)
		(T 
			(write-line "Invalid choice! ")
			(getHumanMove gameState hasAlreadyDrawn))
		)
	)
)


(DEFUN playHuman(gameState)
	;no options for pass
	;do it automatically after drawing from stock
	(terpri)
	(write-line "----------------------------------------------------------")
	(princ "Human Hand: ")
	(print(getHumanHand gameState))
	(terpri)
	(write-line "----------------------------------------------------------")
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
					( ;if side is right but hasn't been passed or tile is double then reask
						COND ( 
							(AND (eq 'R (first move))  (AND (NOT (getPlayerPassed gameState) ) (/= (first (rest move)) (second (rest move)))) )
							(terpri)
							(write-line "You cannot place on the right side at the moment! Please try again!") 
							(playHuman gameState)
							)
						(T 
							(LET*(
								(hand (getHumanHand gameState))
								(layout (getLayout gameState))
							)
							( COND(
								;checking for if chosen domino is in hand
								(position (rest move ) hand :test #'equal)
								(terpri)
								(write-line "Debug:: You have the domino in hand!")
								;need let on placedomino
								( LET* (
									(resultMove (validateMove move layout))
									)
									( COND (
										(null resultMove)
										(terpri)
										(write-line "You cannot place that domino on that side! Please try again!")
										(playHuman gameState)
									)
									(T 
										;update pass and turn
										;implement pass concept
									 	(updateHumanHand (updateLayout gameState (placeDomino resultMove layout) ) (remove (rest move) hand :test #'equal))
									))
								)
							
							)
							(T 
								(terpri)
								(write-line "You don't have that domino. Please select a domino in hand!")
								(playHuman gameState)) )
							)		
						)
					)
				)
				(T 
					(terpri)
					(write-line "Plese select a valid side!")
					(playHuman gameState)
				)
			)
		)
		(T 
			(terpri)
			(write-line "Invalid move. Please follow the input syntax!!")
			(playHuman gameState)
		)
	))
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
	(write-line "Debug: Placing the domino!")
	( COND (
		(eq 'L (first move))
		(write-line "Debug:: Placing on the left")
		(
			;left stuff
			CONS (first layout) (CONS (rest move) (rest layout))
		) )
		(T 
			(
				;right stuff
				reverse (CONS (first (reverse layout)) (CONS (rest move) (rest (reverse layout))))
			)
		)
	)

)

(DEFUN hasMoreMovesHuman (hand layout passed)
	( COND(
		(null hand)
		(write-line "No moves in hand to play")
		NIL
	)
	(
		(OR (= (first (first hand) ) (second (first hand))) passed)
		(write-line "Either double or pass")
		( COND(
				(OR (validateMove (CONS 'L (first hand)) layout) (validateMove (CONS 'R (first hand)) layout))
				(print (first hand))
				(write-line "It was a double or the previous player had passed")
				T
			)
			(T 
				(hasMoreMovesHuman (rest hand) layout passed ))
		)
	)(T 
		( COND(
				(validateMove (CONS 'L (first hand) ) layout) 
				(print(first hand))
				(write-line "Human played on his side")
				T
			)
			(T 
				(hasMoreMovesHuman (rest hand) layout passed ) )
		)
	)

	)
)

(DEFUN getHint(gameState)
	(

	)
)

(DEFUN getAllPossibleMoves(layout hand passed side)
	( COND (
		(null hand)
		()
	)
	(T 
		( COND(
			(AND (null passed) (/= (first (first hand)) (second (first hand)))) ;or not double
			( COND (
				(null (validateMove (CONS side (first hand)) layout ))
				(getAllPossibleMoves layout (rest hand) passed side)	
			)
			(T 
			(LIST (CONS side (first hand)) (getAllPossibleMoves layout (rest hand) passed side))))
			;condition check on player side
		)
		(T ;if passed or double
			( LET* (
					(left (validateMove (CONS 'L (first hand)) layout ))
					(right (validateMove (CONS 'R (first hand)) layout ))
				)
				( COND (
					(AND (NOT (null left) ) (NOT (null right) )) ;if it can be place on both sides
					(LIST (CONS 'L (first hand)) (CONS 'R (first hand)) (getAllPossibleMoves layout (rest hand) passed side))
				)
				(
					(NOT (null left))
					(LIST (CONS 'L (first hand)) (getAllPossibleMoves layout (rest hand) passed side))
				)
				(
					(NOT (null right))
					(LIST (CONS 'R (first hand)) (getAllPossibleMoves layout (rest hand) passed side))
				)
				( T 
				(getAllPossibleMoves layout (rest hand) passed side) )
					
				)	
			)
		) )
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

(DEFUN getHumanScore(gameState)
	( elt gameState 3 )
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
				(
					(state (make-random-state t))
					(x (random (length dominos) state)))
				( CONS 
					(elt dominos x) 
					(shuffleDominos 
						(removeIthElement (+ x 1) dominos)) 
				))
		)
	)
)

(DEFUN askToSave(gameState) 
	(displayRoundState gameState)
	(terpri)
	(write-line "Would you like to save and quit? Y for yes, everything else for no.")
	(LET* (
		(choice (read))
	)
	(COND (
			(eq choice 'Y )
			(serialize gameState)
			T
		)
		(T
			()
		))
	)

)

(DEFUN serialize (gameState)
	(with-open-file (output "./game.txt" 
                            :direction :output :if-exists :supersede)
		(format output "~a" gameState)

	)
)

(terpri)
(trace playRound)
(Longana)