%findMax(X, Max) :-
%	List = atom_codes(X),
%	select(Max, List, Rest), \+ (member(E, Rest), E > Max).

accMax([H|T],A,Max)  :-
     H  >  A,
     accMax(T,H,Max).

accMax([H|T],A,Max)  :-
     H  =<  A,
     accMax(T,A,Max).

accMax([],A,A). 

max(String,Max)  :-
		 %List=String format("~s~n", [List]),
		 %List = atom_codes(String),
		 atom_number(String, List),
         List  = [H|_],
         accMax(List,H,Max).
         