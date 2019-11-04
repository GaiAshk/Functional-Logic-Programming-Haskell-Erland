% Gai Ashkenazy
% 204459127

% Maayan Wagenheim
% 312275431

-module(hw6).
-compile(export_all).


% Q1
reverse(Xs) ->
	reverseAcc(Xs,[]).
	
reverseAcc([],Revs) -> 
	Revs;
reverseAcc([Hd | Tl], Revs) -> 
	reverseAcc(Tl, [Hd | Revs]).


%Q2
splitter(Ls) ->
	First = fun(Tup) -> element(1,Tup) end,
	Second = fun(Tup) -> element(2,Tup) end, 
	{lists:map(First ,Ls) ,lists:map(Second, Ls)}. 	


% test practice
splitter2(Ts) -> splitter_aux(Ts, [], []).

splitter_aux([{X, Y} | Ts], Xs, Ys) -> splitter_aux(Ts, [X | Xs], [Y | Ys]);
splitter_aux([], Xs, Ys) -> {reverse(Xs), reverse(Ys)}.


%Q3
start_server() ->
	Pid = spawn(hw6, server, []),
	register(child_process,Pid),
	ok.

server() ->
	receive 
		stopped -> stop_server();
		Msg ->
				io:format("~p~n",[Msg]),
				server()
	end.

print(Msg) ->
	whereis(child_process) ! Msg,
	ok.

stop_server() ->
	whereis(child_process) ! stopped.


%Q4 a
sumTree({}) ->
	0;
sumTree({leaf, X}) ->
	X;
sumTree({L, node, R}) ->
	sumTree(L) + sumTree(R).
	

%Q4 b
sumTreeConc({}) -> 
	0;
sumTreeConc({leaf, X}) ->
	X;
sumTreeConc(Tree) ->
	sumTreeConci(Tree, self()),
	receive
		{toSum,Sum} -> Sum
	end.


sumTreeConci({leaf, X}, Parent) ->
	Parent ! {toSum , X};
	
sumTreeConci({L, node, R}, Parent) ->
	spawn(hw6, sumTreeConci, [R, self()]),
	spawn(hw6, sumTreeConci, [L, self()]),
	receiveMsg(Parent).
	
receiveMsg(Parent) ->
	receive
		{toSum, X} ->
			receive 
				{toSum, Y} -> SumTemp = X + Y,
				Parent ! {toSum, SumTemp}
			end
	end.
	
			
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
