-module(spl).
-compile (export_all).

process_create(RootID, 0) -> 
	Pid_next = RootID,
	message_handler(Pid_next, 0);
process_create(RootID, N) ->
	Pid_next = spawn(spl, process_create, [RootID, N-1]),
	message_handler(Pid_next, N).
	
message_handler(Pid_next, N) ->
	receive
		{msg, M} -> io:format("Process number ~p received message: ~p ~n", [N, M]),
					Pid_next ! {msg, M},
					message_handler(Pid_next, N);
		stop -> io:format("Process number ~p stopped ~n", [N]),
				Pid_next ! stop
	end.
	
init_messages([], Second_PID) ->
	Second_PID ! stop;
init_messages(M, Second_PID) ->
	[Head | Tail] = M,
	Second_PID ! {msg, Head},
	receive 
		{msg, Head} -> init_messages(Tail, Second_PID)
	end.
	
	


run() ->
	N = 3,
	M = ["hello", "world", "welcome"],
	
	Pid_second = spawn(spl, process_create, [self(), N]),
	init_messages(M, Pid_second),
	'stopped run'.
	
	
reverse([]) -> [];
reverse([He|Ta]) -> reverse(Ta) ++ [He].

findElem(0, [Hea|_]) -> {found, Hea};
findElem(_, []) -> 'not_found';
findElem(I, [_|Tai]) -> findElem(I-1, Tai).

find(_, []) -> 'not_found';
find(I, [Head|Tail]) -> 
	case (I == Head) of
		true -> {found, I};
		false -> find(I, Tail)
	end.

delete(I, List) -> delete2(I, [], List).

delete2(_, Acc, []) -> Acc;
delete2(I, Acc, [Head|Tail]) ->
	case I of
		Head -> Acc ++ Tail;
		_ -> delete2(I, Acc ++ [Head], Tail)
	end.

delete3(I, List) -> lists:foldr(fun(X, Acc) -> 
									case I == X of
										true-> Acc;
										false-> [X|Acc]
									end
								end, [], List).

%flatter(List) -> 
	%Cons = (++),
	%lists:foldr(Cons, [], List).

flatter2(List) -> lists:foldl(fun(Acc, X) -> Acc ++ X end, [], List).

square(List) -> lists:map(fun(X) -> X*X end, List).

filter(_, []) -> [];
filter(F, [Head|Tail]) ->
	case F(Head) of
		true -> [Head| filter(F, Tail)];
		false -> filter(F, Tail)
	end.

avg_server() -> 
	avg_server2([]).
	

avg_server2(List) -> 
	receive
		X -> 
			Temp = List ++ [X],
			ok,
			io:format("The average of ~w is ~p ~n", [ Temp, lists:sum(Temp) / length(Temp)]),
			avg_server2(Temp)
	end.

new_queue() -> 
	spawn(spl, new_queue2, [[]]).

new_queue2(List) ->
	receive
		{push, X, Pid} -> 
			Temp = List ++ [X],
			Pid ! ok,
			new_queue2(Temp);
		{pop, Pid} -> 
			[Head|Tail] = List,
			Pid ! Head,
			new_queue2(Tail)
	end.

push(Pid, X) -> 
	Pid ! {push, X, self()},
	receive
		ok -> ok
	end.

pop(Pid) ->
	Pid ! {pop, self()},
	receive
		Val -> io:format("~p ~n", [Val])
	end.


spliteListByN(0, List) -> {[], List};
spliteListByN(_, []) -> {[], []};
spliteListByN(I, [Head|Tail]) -> 
	{X, Y} = spliteListByN(I-1, Tail),
	{[Head|X], Y}.


	


		


	

	
	