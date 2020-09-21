likes(abhijit, প্রিয়ান্কা).
likes(প্রিয়ান্কা, abhijit).
likes(gopal, joy).

loves(X, Y):-
	likes(X, Y),
	likes(Y, X). 

loves(gopal, joy)
