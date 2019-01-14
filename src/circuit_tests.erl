-module(circuit_tests).
-include_lib("eunit/include/eunit.hrl").

%only one test at a time will work, to run: put either startCircuit_test_() or createNPipes_test_() in comments%
startCircuit_test_() ->
	{"start the circuit", {foreach, fun return_startCircuit/0, fun stop/1, [fun checkPipes/1]}}.

createNPipes_test_() ->
	{"checks if N pipes are created",{setup, fun return_createNPipes/0, fun stop/1, fun checkNPipes/1}}.


return_startCircuit() ->
	{ok, {PipeTypePid, Pipes, Cons, Locs}} = circuit:startCircuit(),
	{PipeTypePid, Pipes, Cons, Locs}.

stop(_) ->
	circuit:stop().
	
return_createNPipes() ->
	N = 4,	
	{ok, {PipeTypePid, Pipes, Cons, Locs}} = circuit:createNPipes(N),
	{PipeTypePid, Pipes, Cons, Locs, N}.


checkPipes({PipeTypePid, Pipes, Cons, Locs}) ->
	[PipeA, PipeB, PipeC|_] = Pipes,
	[[A1, A2],[B1, B2],[C1, C2]] = Cons,
	[LocA, LocB, LocC] = Locs,
	
	[
		?_assert(erlang:is_process_alive(PipeTypePid)),
		?_assert(erlang:is_process_alive(PipeA)),
		?_assert(erlang:is_process_alive(PipeB)),
		?_assert(erlang:is_process_alive(PipeC)),
		?_assert(erlang:is_process_alive(A1)),
		?_assert(erlang:is_process_alive(A2)),
		?_assert(erlang:is_process_alive(B1)),
		?_assert(erlang:is_process_alive(B2)),
		?_assert(erlang:is_process_alive(C1)),
		?_assert(erlang:is_process_alive(C2)),
		?_assert(erlang:is_process_alive(LocA)),
		?_assert(erlang:is_process_alive(LocB)),
		?_assert(erlang:is_process_alive(LocC))%,
	].

checkNPipes({PipeTypePid, Pipes, Cons, Locs, N}) ->
	[PipeA, PipeB, PipeC|_] = Pipes,
	[A1,A2,B1,B2,C1,C2|_] = Cons,
	[LocA, LocB, LocC|_] = Locs,
	
	[
		?_assert(erlang:is_process_alive(PipeTypePid)),
		?_assert(erlang:is_process_alive(PipeA)),
		?_assert(erlang:is_process_alive(PipeB)),
		?_assert(erlang:is_process_alive(PipeC)),
		?_assert(erlang:is_process_alive(A1)),
		?_assert(erlang:is_process_alive(A2)),
		?_assert(erlang:is_process_alive(B1)),
		?_assert(erlang:is_process_alive(B2)),
		?_assert(erlang:is_process_alive(C1)),
		?_assert(erlang:is_process_alive(C2)),
		?_assert(erlang:is_process_alive(LocA)),
		?_assert(erlang:is_process_alive(LocB)),
		?_assert(erlang:is_process_alive(LocC)),
		?_assertEqual(N, length(Pipes)),
		?_assertEqual(2*N, length(Cons)),
		?_assertEqual(N, length(Locs))%,
	].

