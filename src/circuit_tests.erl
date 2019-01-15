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
				checkFluidum({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst})
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
			checkFluidum({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst}),
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
		fun getCircuitWithPumpAndFlowmeter/0, 
		fun stop/1, 
		fun({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}) -> 
			[
			checkFluidum({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst}),
			checkCircuitWithPump({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFunctions({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFlow({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkFlowmeter({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst})
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
			checkFluidum({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst}),
			checkCircuitWithPump({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFunctions({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkPumpFlow({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}),
			checkFlowmeter({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}),
			%new test
			checkHE({PipeType, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst, HEType, HEInst})
			]
		end
		}
	}.

createComplexCircuit_test_() ->
	{"checks if a complex circuit with A pipes, B pumps, C flowmeters, D heatexchangers and a fluid is created",
	{setup,
	fun getComplexCircuit/0,
	fun stop/1,
	fun({Types, Pipes, Cons, Locs, Fluidum, Pumps, FMs, HEs, A, B, C, D}) ->
	[
		checkComplexCircuit({Types, Pipes, Cons, Locs, Fluidum, Pumps, FMs, HEs, A, B, C, D}),
		checkPipes({Types, Pipes, Cons, Locs, Fluidum, Pumps, FMs, HEs, A, B, C, D}),
		checkPumps({Types, Pipes, Cons, Locs, Fluidum, Pumps, FMs, HEs, A, B, C, D})
	] end
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
	
getComplexCircuit() ->
	A = 10, B = 3, C =2, D =1,
	circuit:startSurvivor(),
	{ok, {Types, Pipes, Cons, Locs, Fluidum, Pumps, FMs, HEs}} = circuit:createComplexCircuit(A,B,C,D),
	{Types, Pipes, Cons, Locs, Fluidum, Pumps, FMs, HEs, A, B, C, D}. 

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

checkFluidum({_, Pipes, _, _, FluidumType, FluidumInst}) ->
	[PipeA, PipeB, PipeC|_] = Pipes,
	{ok, A} = msg:get(FluidumInst, get_locations),
	TestA = ?_assertEqual(A, []),

	{ok, B} = msg:get(FluidumInst, get_type),
	TestB = ?_assertEqual(B, FluidumType),

	%{ok, C} = msg:get(FluidumInst, get_resource_circuit),
	%PipeList = [PipeA, PipeB, PipeC],
	%TestC = ?_assertEqual(PipeList, C),

	[TestA, TestB].%, TestC].

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

checkHE({_,_,_,_,_,_,_,_,_,_, HEType, HEInst})->
	TestA = [
		?_assert(erlang:is_process_alive(HEType)),
		?_assert(erlang:is_process_alive(HEInst))
		],

	{ok, {ok, Inf}} = heatExchangerInst:temp_influence(HEInst),
	{ok, B} = Inf(5, 20),
	TestB = ?_assertEqual(B, 20 + (1/5)),

	[TestA, TestB].

checkComplexCircuit({Types, Pipes, Cons, Locs, Fluidum, Pumps, FMs, HEs, A, B, C, D}) ->
	[PipeType, FluidumType, PumpType, FMType, HEType] = Types,

	[
		?_assert(erlang:is_process_alive(PipeType)),
		?_assert(erlang:is_process_alive(FluidumType)),
		?_assert(erlang:is_process_alive(PumpType)),
		?_assert(erlang:is_process_alive(FMType)),
		?_assert(erlang:is_process_alive(HEType)),
		?_assert(erlang:is_process_alive(Fluidum)),
		?_assertEqual(A, length(Pipes)),
		?_assertEqual(2*A, length(Cons)),
		?_assertEqual(A, length(Locs)),
		?_assertEqual(B, length(Pumps)),
		?_assertEqual(C, length(FMs)),
		?_assertEqual(D, length(HEs))
	].

checkPipes({_, Pipes, Cons, Locs, _, _, _, _,A, _, _, _}) ->
	checkPipes(A, Pipes, Cons, Locs, []).

checkPipes(1, Pipes, Cons, Locs, PipeTests) ->
	[Pipe|_] = Pipes,
	TestA = ?_assert(erlang:is_process_alive(Pipe)),
	[Con1, Con2|_] = Cons,	
	TestB = ?_assert(erlang:is_process_alive(Con1)),	
	TestC = ?_assert(erlang:is_process_alive(Con2)),
	[Loc|_] = Locs,
	TestD = ?_assert(erlang:is_process_alive(Loc)),
	[TestA, TestB, TestC, TestD |PipeTests];

checkPipes(N,Pipes, Cons, Locs, PipeTests) ->	
	[Pipe|OtherPipes] = Pipes,
	TestA = ?_assert(erlang:is_process_alive(Pipe)),
	[Con1, Con2|OtherCons] = Cons,	
	TestB = ?_assert(erlang:is_process_alive(Con1)),	
	TestC = ?_assert(erlang:is_process_alive(Con2)),
	[Loc|OtherLocs] = Locs,
	TestD = ?_assert(erlang:is_process_alive(Loc)),
	checkPipes(N-1, OtherPipes, OtherCons, OtherLocs, [TestA, TestB, TestC, TestD |PipeTests]).  
	

checkPumps({_,_,_,_,_,Pumps,_,_,_,B,_,_}) -> 
	checkPumps(B, Pumps, []).

checkPumps(1, Pumps, PumpTests) ->
	[Pump|_] = Pumps,
	%check if pump exists	
	TestA = ?_assert(erlang:is_process_alive(Pump)),
	
	%check if pump is turned off
	{ok, B} = pumpInst:is_on(Pump),
	TestB = ?_assertEqual(B, off),

	%turn on pump and check if it worked
	pumpInst:switch_on(Pump),
	{ok, C} = pumpInst:is_on(Pump),
	TestC = ?_assertEqual(C, on),

	%turn on pump again and check if still on
	pumpInst:switch_on(Pump),
	{ok, D} = pumpInst:is_on(Pump),
	TestD = ?_assertEqual(D, on),

	%turn off pump and check if it worked
	pumpInst:switch_off(Pump),
	{ok, E} = pumpInst:is_on(Pump),
	TestE = ?_assertEqual(E, off),

	%turn off pump again and check if still off
	pumpInst:switch_off(Pump),
	{ok, F} = pumpInst:is_on(Pump),
	TestF = ?_assertEqual(F, off),
	
	%returning the tests
	[TestA, TestB, TestC, TestD, TestE, TestF| PumpTests];

checkPumps(N, Pumps, PumpTests) ->
		[Pump|OtherPumps] = Pumps,
		%check if pump exists	
		TestA = ?_assert(erlang:is_process_alive(Pump)),
		
		%check if pump is turned off
		{ok, B} = pumpInst:is_on(Pump),
		TestB = ?_assertEqual(B, off),

		%turn on pump and check if it worked
		pumpInst:switch_on(Pump),
		{ok, C} = pumpInst:is_on(Pump),
		TestC = ?_assertEqual(C, on),

		%turn on pump again and check if still on
		pumpInst:switch_on(Pump),
		{ok, D} = pumpInst:is_on(Pump),
		TestD = ?_assertEqual(D, on),

		%turn off pump and check if it worked
		pumpInst:switch_off(Pump),
		{ok, E} = pumpInst:is_on(Pump),
		TestE = ?_assertEqual(E, off),

		%turn off pump again and check if still off
		pumpInst:switch_off(Pump),
		{ok, F} = pumpInst:is_on(Pump),
		TestF = ?_assertEqual(F, off),
		
		%add tests and check next pump
		checkPumps(N-1, OtherPumps, [TestA, TestB, TestC, TestD, TestE, TestF|PumpTests]).
	
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
