% Gai Ashkenazy
% 204459127

% Maayan Wagenheim
% 312275431

-module(hw6).
-compile(export_all).

% Question 1
reverse(Lst) -> reverseAcc(Lst, []).

reverseAcc([], Ls) -> Ls;
reverseAcc([Hd | Tl], Ls) -> reverseAcc(Tl, [Hd | Ls]).


%Question 2
splitter(Ls) -> 
	First = fun(Tp) -> element(1, Tp) end,
	Second = fun(Tp) -> element(2, Tp) end,  
	{lists:map(First, Ls), lists:map(Second, Ls)}.


% Question 3


% Question 4 

sumTree({}) -> 
	0;
sumTree({leaf, X}) ->
	X;
sumTree({L, node, R}) ->
	sumTree(L) + sumTree(R).

sumTreeConc({}) -> 0;
sumTreeConc({leaf, X}) -> X;
sumTreeConc({L, node, R}) -> processCreation({L, node, R}, self()).


sumAll(X) ->
	receive
		Y -> sumAll(X+Y)
	end.


processCreation({L, node, R}, FirstID) -> 
	spawn(hw6, processSum, [L]);
	spawn(hw6, processSum, [R]);
	sumAll(X).

processSum({leaf, X}, PID) -> PID ! X;
processSum({L, node, R}, FirstID) ->
	processCreation(L);
	processCreation(R).


		
