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

(DEFUN shuffleDominos(dominos size)
	( COND
		( (null dominos)
			())
		(T
			(let* 
				((x (random size)))
				( CONS 
					(getIthElement (+ x 1) dominos) 
					(shuffleDominos 
						(removeIthElement (+ x 1) dominos) (- size 1)) 
				))
		)
	)
)


;(print (generateAllDominos (generateNums 0 6) ))
(trace shuffleDominos)
(trace getIthElement)
(trace removeIthElement)
(print ( shuffleDominos (generateAllDominos (generateNums 0 6) ) 28 ))
