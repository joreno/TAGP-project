-module(circuit_tests).
-include_lib("eunit/include/eunit.hrl").

createSimpleCircuit_test_() ->
	{"checks if a circuit with 3 pipes is created", 
		{foreach,
		fun return_createSimpleCircuit/0, 
		fun stop/1, 
		[fun checkSimpleCircuit/1]}
	}.

createNPipes_test_() ->
	{"checks if N pipes are created",
		{setup, 
		fun return_createNPipes/0, 
		fun stop/1, 
		fun checkNPipes/1}
	}.

createCircuit_test_() ->
	{"checks if a circuit with 3 pipes and water is created",
		{setup, 
		fun return_createCircuit/0, 
		fun stop/1, 
		fun checkCircuit/1}
	}.

createCircuitWithPump_test_() ->
	{"checks if a circuit with 3 pipes, a pump and water is created",
		{setup, 
		fun return_createCircuitWithPump/0, 
		fun stop/1, 
		fun({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}) -> 
			[
			checkCircuitWithPump({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFunctions({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFlow({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst})
			]
		end
		}
	}.

createCircuitWithPumpAndFlowmeter_test_() ->
	{"checks if a circuit with 3 pipes, a pump, a flowmeter and water is created",
		{setup, 
		fun return_createCircuitWithPumpAndFlowmeter/0, 
		fun stop/1, 
		fun({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}) -> 
			[
			checkCircuitWithPump({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFunctions({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFlow({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkFlowmeter({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst})
			]
		end
		}
	}.


%%%%%setup%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop(_) ->
	circuit:stop().

return_createSimpleCircuit() ->
	{ok, {PipeTypePid, Pipes, Cons, Locs}} = circuit:createSimpleCircuit(),
	{PipeTypePid, Pipes, Cons, Locs}.

	
return_createNPipes() ->
	N = 4,	
	{ok, {PipeTypePid, Pipes, Cons, Locs}} = circuit:createNPipes(N),
	{PipeTypePid, Pipes, Cons, Locs, N}.

return_createCircuit() ->
	{ok, {PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst}} = circuit:createCircuit(),
	{PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst}.

return_createCircuitWithPump() ->
	{ok, {PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}} = circuit:createCircuitWithPump(),
	{PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}.

return_createCircuitWithPumpAndFlowmeter() ->
	{ok, {PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}} = circuit:createCircuitWithPumpAndFlowmeter(),
	{PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}.
	

%%%%%Tests%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkSimpleCircuit({PipeTypePid, Pipes, Cons, Locs}) ->
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

checkCircuit({PipeTypePid, Pipes, Cons, Locs, FluidumType, FluidumInst}) ->
	[PipeA, PipeB, PipeC|_] = Pipes,
	[A1,A2,B1,B2,C1,C2] = Cons,
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
		?_assert(erlang:is_process_alive(LocC)),
		?_assert(erlang:is_process_alive(FluidumType)),
		?_assert(erlang:is_process_alive(FluidumInst))%,
	].

checkCircuitWithPump({PipeTypePid, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}) ->
	[PipeA, PipeB, PipeC|_] = Pipes,
	[A1,A2,B1,B2,C1,C2] = Cons,
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
		?_assert(erlang:is_process_alive(LocC)),
		?_assert(erlang:is_process_alive(FluidumType)),
		?_assert(erlang:is_process_alive(FluidumInst)),
		?_assert(erlang:is_process_alive(PumpType)),
		?_assert(erlang:is_process_alive(PumpInst))%,
	].

checkPumpFunctions({_, _, _, _, _, _, PumpType, PumpInst}) ->
	%check if pump is turned off
	{ok, A} = pumpInst:is_on(PumpInst),
	TestA = 
		[
			?_assert(erlang:is_process_alive(PumpType)),
			?_assert(erlang:is_process_alive(PumpInst)),
			?_assertEqual(A, off)
		],
	
	%turn on pump and check if it worked
	pumpInst:switch_on(PumpInst),
	{ok, B} = pumpInst:is_on(PumpInst),
	TestB = ?_assertEqual(B, on),

	%turn on pump again and check if still on
	pumpInst:switch_on(PumpInst),
	{ok, C} = pumpInst:is_on(PumpInst),
	TestC = ?_assertEqual(C, on),

	%turn off pump and check if it worked
	pumpInst:switch_off(PumpInst),
	{ok, D} = pumpInst:is_on(PumpInst),
	TestD = ?_assertEqual(D, off),

	%turn off pump again and check if still off
	pumpInst:switch_off(PumpInst),
	{ok, E} = pumpInst:is_on(PumpInst),
	TestE = ?_assertEqual(E, off),
	
	[TestA, TestB, TestC, TestD, TestE].


checkPumpFlow({_,_,_,_,_,_, PumpType, PumpInst}) ->
	%check if pump is turned off
	{ok, A} = pumpInst:is_on(PumpInst),
	TestA = 
		[
			?_assert(erlang:is_process_alive(PumpType)),
			?_assert(erlang:is_process_alive(PumpInst)),
			?_assertEqual(A, off)
		],
	
	%setFlow
	Flow = 5,
	
	%pump is turned off, so flow = 0, check if true
	{ok, B} = pumpInst:flow_influence(PumpInst),
	TestB = ?_assertEqual(B(Flow), 0),

	%turn on pump and check if it worked
	pumpInst:switch_on(PumpInst),
	{ok, C} = pumpInst:is_on(PumpInst),
	TestC = ?_assertEqual(C, on),

	%check if flow is correct
	{ok, D} = pumpInst:flow_influence(PumpInst),
	TestD = ?_assertEqual(D(Flow), 250 - 5 * Flow - 2 * Flow * Flow),
	
	[TestA, TestB, TestC, TestD].


checkFlowmeter({_,Pipes,_,_,_,_,_,_, FlowmeterType, FlowmeterInst}) ->
%	[PipeA, PipeB, PipeC] = Pipes,
	
	TestA = 
		[
		?_assert(erlang:is_process_alive(FlowmeterType)),	
		?_assert(erlang:is_process_alive(FlowmeterInst))
		],
	
	%check if the measured flow is equal to {ok, real_flow}
	{ok, B} = flowMeterInst:measure_flow(FlowmeterInst),
	TestB = ?_assertEqual(B,{ok, real_flow}),
	[TestA, TestB].	

%	{ok, C} = flowMeterInst:estimate_flow(FlowmeterInst),
%	{ok, RefA} = apply(resource_instance, get_flow_influence, [PipeA]),
%	{ok, RefB} = apply(resource_instance, get_flow_influence, [PipeB]),
%	{ok, RefC} = apply(resource_instance, get_flow_influence, [PipeC]),
%	Refs = [RefA, RefB, RefC],
%	Ref = compute({0,10}, Refs),
%	TestC = ?_assertEqual(C, Ref),
%	[TestA, TestB, TestC].


%%%%%Other Functions%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compute({Low, High}, _InflFnCircuit) when (High - Low) < 1 -> 
	(Low + High) / 2 ;
	
compute({Low, High}, InflFnCircuit) ->
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	Mid = (H + L) / 2, M = eval(Mid, InflFnCircuit, 0),
	if 	M > 0 -> 
			compute({Low, Mid}, InflFnCircuit);
        true -> 
            compute({Mid, High}, InflFnCircuit)
    end.

eval(Flow, [Fn | RemFn] , Acc) ->
	eval(Flow, RemFn, Acc + Fn(Flow));

eval(_Flow, [], Acc) -> Acc. 
