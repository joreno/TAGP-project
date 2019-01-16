-module(circuit_tests).
-include_lib("eunit/include/eunit.hrl").

createSimpleCircuit_test_() ->
	{"checks if a circuit with 3 pipes is created", 
		{foreach,
		fun getSimpleCircuit/0, 
		fun stop/1, 
		[fun checkSimpleCircuit/1]}
	}.

createNPipes_test_() ->
	{"checks if N pipes are created",
		{setup, 
		fun getNPipes/0, 
		fun stop/1, 
		fun checkNPipes/1}
	}.

createCircuit_test_() ->
	{"checks if a circuit with 3 pipes and water is created",
		{setup, 
		fun getCircuit/0, 
		fun stop/1,
		fun({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst})->	
			[
				checkCircuit({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst}),	
				checkFluidum({Pipes, Cons, FluidumType, FluidumInst})
			]	
		end
		}
	}.

createCircuitWithPump_test_() ->
	{"checks if a circuit with 3 pipes, a pump and water is created",
		{setup, 
		fun getCircuitWithPump/0, 
		fun stop/1, 
		fun({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}) -> 
			[
			checkFluidum({Pipes, Cons, FluidumType, FluidumInst}),
			checkCircuitWithPump({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFunctions({PumpType, PumpInst}),
			checkPumpFlow({PumpType, PumpInst})
			]
		end
		}
	}.

createCircuitWithPumpAndFlowmeter_test_() ->
	{"checks if a circuit with 3 pipes, a pump, a flowmeter and water is created",
		{setup, 
		fun getCircuitWithPumpAndFlowmeter/0, 
		fun stop/1, 
		fun({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}) -> 
			[
			checkFluidum({Pipes, Cons, FluidumType, FluidumInst}),
			checkCircuitWithPump({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFunctions({PumpType, PumpInst}),
			checkPumpFlow({PumpType, PumpInst}),
			checkFlowmeter({FlowmeterType, FlowmeterInst, Pipes})
			]
		end
		}
	}.


createFullCircuit_test_() ->
	{"checks if a circuit with 3 pipes, a pump, a flowmeter, a heat exchanger and water is created",
		{setup, 
		fun getFullCircuit/0, 
		fun stop/1, 
		fun({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst, HEType, HEInst}) -> 
			[
			%previous tests
			checkFluidum({Pipes, Cons, FluidumType, FluidumInst}),
			checkCircuitWithPump({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFunctions({PumpType, PumpInst}),
			checkPumpFlow({PumpType, PumpInst}),
			checkFlowmeter({FlowmeterType, FlowmeterInst, Pipes}),
			%new test
			checkHE({HEType, HEInst})
			]
		end
		}
	}.


stop(_) ->
	circuit:stop().

%%%%%getCircuits%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getSimpleCircuit() ->
	circuit:startSurvivor(),
	{ok, {PipeTypePid, Pipes, Cons, Locs}} = circuit:createSimpleCircuit(),
	{PipeTypePid, Pipes, Cons, Locs}.

	
getNPipes() ->
	circuit:startSurvivor(),
	N = 4,	
	{ok, {PipeTypePid, Pipes, Cons, Locs}} = circuit:createNPipes(N),
	{PipeTypePid, Pipes, Cons, Locs, N}.

getCircuit() ->
	circuit:startSurvivor(),
	{ok, {PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst}} = circuit:createCircuit(),
	{PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst}.

getCircuitWithPump() ->
	circuit:startSurvivor(),
	{ok, {PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}} = circuit:createCircuitWithPump(),
	{PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}.

getCircuitWithPumpAndFlowmeter() ->
	circuit:startSurvivor(),
	{ok, {PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}} = circuit:createCircuitWithPumpAndFlowmeter(),
	{PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}.
	
getFullCircuit() ->
	circuit:startSurvivor(),
	{ok, {PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst, HEType, HEInst}} = circuit:createFullCircuit(),
	{PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst, HEType, HEInst}.
	
%%%%%Tests%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkSimpleCircuit({PipeTypePid, Pipes, Cons, Locs}) ->
	[PipeA, PipeB, PipeC|_] = Pipes,
	[A1, A2, B1, B2, C1, C2] = Cons,
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

checkFluidum({Pipes, Cons, FluidumType, FluidumInst}) ->
	{ok, A} = msg:get(FluidumInst, get_locations),
	TestA = ?_assertEqual(A, []),

	{ok, B} = msg:get(FluidumInst, get_type),
	TestB = ?_assertEqual(B, FluidumType),

	{ok, C} = msg:get(FluidumInst, get_resource_circuit),
	TestC = checkFluidumCircuit(Pipes, C),
	
	[ConA|_] = Cons,
	{ok, {D, E}} = fluidumTyp:discover_circuit(ConA),
	TestD = ?_assertEqual(D, ConA),
	TestE = checkFluidumCircuit(Cons, E),	

	[TestA, TestB, TestC, TestD, TestE].

checkFluidumCircuit(Pipes, Circuit) ->
	checkFluidumCircuit(Pipes, Circuit, []).

checkFluidumCircuit([Pipe|OtherPipes], Circuit, Tests) ->
	{ok, A} = maps:find(Pipe, Circuit),
	TestA = ?_assertEqual(A,processed),
	checkFluidumCircuit(OtherPipes, Circuit, [TestA |Tests]);

checkFluidumCircuit([], _, Tests) ->
	Tests.

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

checkPumpFunctions({PumpType, PumpInst}) ->
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


checkPumpFlow({PumpType, PumpInst}) ->
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


checkFlowmeter({FlowmeterType, FlowmeterInst, Pipes}) ->
	
	TestA = 
		[
		?_assert(erlang:is_process_alive(FlowmeterType)),	
		?_assert(erlang:is_process_alive(FlowmeterInst))
		],
	
	%check if the measured flow is equal to {ok, real_flow}
	{ok, B} = flowMeterInst:measure_flow(FlowmeterInst),
	TestB = ?_assertEqual(B,{ok, real_flow}),

	%check the estimate_flow function
	[PipeA, PipeB, PipeC| _] = Pipes,
	{ok, C} = flowMeterInst:estimate_flow(FlowmeterInst),
	{ok, RefA} = apply(resource_instance, get_flow_influence, [PipeA]),
	{ok, RefB} = apply(resource_instance, get_flow_influence, [PipeB]),
	{ok, RefC} = apply(resource_instance, get_flow_influence, [PipeC]),
	TestC = ?_assertEqual(C, compute({0,10}, [RefA, RefB, RefC])),	
	[TestA, TestB, TestC].	

compute({Low, High}, _InflFnCircuit) when (High - Low) < 1 -> 
	%Todo convergentiewaarde instelbaar maken. 
	(Low + High) / 2 ;
	
compute({Low, High}, InflFnCircuit) ->
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	Mid = (H + L) / 2, M = eval(Mid, InflFnCircuit, 0),
	if 	M > 0 -> 
			compute({Low, Mid}, InflFnCircuit);
        true -> % works as an 'else' branch
            compute({Mid, High}, InflFnCircuit)
    end.

	
eval(Flow, [Fn | RemFn] , Acc) ->
	eval(Flow, RemFn, Acc + Fn(Flow));

eval(_Flow, [], Acc) -> Acc. 


checkHE({HEType, HEInst})->
	TestA = [
		?_assert(erlang:is_process_alive(HEType)),
		?_assert(erlang:is_process_alive(HEInst))
		],

	{ok, {ok, Inf}} = heatExchangerInst:temp_influence(HEInst),
	{ok, B} = Inf(5, 20),
	TestB = ?_assertEqual(B, 20 + (1/5)),

	[TestA, TestB].


