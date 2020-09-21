	bits(0,[]).
		bits(N, [R | Rs]) :-
			N > 0,
			bit(R),
			N1 is N - 1,
			bits(N1,Rs).
	bit(0). 
	bit(1).
	printBit(0):- write(0).
	printBit(1):- write(1).
	printBits([R|Rs]):- printBit(R), write(' '), printBits(Rs).
	printBits([]).
	showTable(N):-bits(N,L),printBits(L),
	dummyPredicate(L,L1),write('>>>'), printBits(L1),nl,fail.
	
	dummyPredicate(L,L1):- reverse(L,L2),
	comp2(L2,L3),reverse(L3,L1).
	
	comp([],[]).
	comp([0|T1],[1|T2]):- comp(T1,T2).
	comp([1|T1],[0|T2]):- comp(T1,T2). 
	
	comp2([],[]).
	comp2([0|T1],[0|T2]):- comp2(T1,T2).
		comp2([1|T1],[1|T2]):- comp(T1,T2).