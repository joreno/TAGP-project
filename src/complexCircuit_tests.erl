-module(complexCircuit_tests).
-include_lib("eunit/include/eunit.hrl").

%%%%%DESCRIPTION%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createComplexCircuit_test_() ->
	{"checks if a complex circuit with A pipes, B pumps, C flowmeters, D heatexchangers and a fluid is created",
	{setup,
	fun getComplexCircuit/0,
	fun stop/1,
	fun({Types, Pipes, Cons, Locs, Fluidum, Pumps, FM, HEs, A, B, C}) ->
	[
		checkComplexCircuit({Types, Pipes, Cons, Locs, Fluidum, Pumps, FM, HEs, A, B, C}),
		checkPipes({Pipes, Cons, Locs, A}),
		checkFluidum({Pipes, Cons, Types, Fluidum}),
		checkPumps({Pumps, B}),
		checkFM({FM, Pipes}),
		checkHEs({HEs, C})
	] end
	}
	}.

%%%%%STOP%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop(_) ->
	circuit:stop().

%%%%%SETUP%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getComplexCircuit() ->
	A = 10, B = 3, C =2,
	circuit:startSurvivor(),
	{ok, {Types, Pipes, Cons, Locs, Fluidum, Pumps, FM, HEs}} = complexCircuit:createComplexCircuit(A,B,C),
	{Types, Pipes, Cons, Locs, Fluidum, Pumps, FM, HEs, A, B, C}.

%%%%%CHECKCOMPLEXCIRCUIT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
checkComplexCircuit({Types, Pipes, Cons, Locs, Fluidum, Pumps, FM, HEs, A, B, C}) ->
	%checks if everything exists
	[PipeType, FluidumType, PumpType, FMType, HEType] = Types,

	[
		%PipeTests:
		?_assert(erlang:is_process_alive(PipeType)),
		?_assertEqual(A, length(Pipes)),
		?_assertEqual(2*A, length(Cons)),
		?_assertEqual(A, length(Locs)),
		
		%FluidumTests:		
		?_assert(erlang:is_process_alive(FluidumType)),
		?_assert(erlang:is_process_alive(Fluidum)),
		
		%PumpTests:	
		?_assert(erlang:is_process_alive(PumpType)),
		?_assertEqual(B, length(Pumps)),
		
		%FMTests:		
		?_assert(erlang:is_process_alive(FMType)),
		?_assert(erlang:is_process_alive(FM)),
		
		%HETests:
		?_assert(erlang:is_process_alive(HEType)),
		?_assertEqual(C, length(HEs))
	].

%%%%%CHECKPIPES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkPipes({Pipes, Cons, Locs, A}) ->
	checkPipes(A, Pipes, Cons, Locs, []).

checkPipes(1, Pipes, Cons, Locs, PipeTests) ->
	[Pipe|_] = Pipes,

	%Check if pipe exists
	TestA = ?_assert(erlang:is_process_alive(Pipe)),
	[Con1, Con2|_] = Cons,

	%check if connections are made	
	TestB = ?_assert(erlang:is_process_alive(Con1)),	
	TestC = ?_assert(erlang:is_process_alive(Con2)),
	[Loc|_] = Locs,

	%check if pipe has received a location
	TestD = ?_assert(erlang:is_process_alive(Loc)),
	
	%run all tests
	[TestA, TestB, TestC, TestD |PipeTests];

checkPipes(N,Pipes, Cons, Locs, PipeTests) ->	
	[Pipe|OtherPipes] = Pipes,

	%Check if pipe exists
	TestA = ?_assert(erlang:is_process_alive(Pipe)),

	%check if connections are made	
	[Con1, Con2|OtherCons] = Cons,	
	TestB = ?_assert(erlang:is_process_alive(Con1)),	
	TestC = ?_assert(erlang:is_process_alive(Con2)),

	%check if pipe has received a location
	[Loc|OtherLocs] = Locs,
	TestD = ?_assert(erlang:is_process_alive(Loc)),
	
	%add new tests to PipeTests and test the next Pipe
	checkPipes(N-1, OtherPipes, OtherCons, OtherLocs, [TestA, TestB, TestC, TestD |PipeTests]).  

%%%%%CHECKFLUIDUM%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFluidum({Pipes, Cons, Types, Fluidum}) ->
	%Test if the locationslist is empty	
	{ok, A} = msg:get(Fluidum, get_locations),
	TestA = ?_assertEqual(A, []),
	
	%Check if instance is of type Fluidum
	[_, FluidumType,_,_,_] = Types,
	{ok, B} = msg:get(Fluidum, get_type),
	TestB = ?_assertEqual(B, FluidumType),

	%Test the get_resource_circuit function
	{ok, C} = msg:get(Fluidum, get_resource_circuit),
	TestC = checkFluidumCircuit(Pipes, C),
	
	%Test the discover_circuit function
	[ConA|_] = Cons,
	{ok, {D, E}} = fluidumTyp:discover_circuit(ConA),
	TestD = ?_assertEqual(D, ConA),
	TestE = checkFluidumCircuit(Cons, E),	

	%run all tests
	[TestA, TestB, TestC, TestD, TestE].


checkFluidumCircuit(Pipes, Circuit) ->
	checkFluidumCircuit(Pipes, Circuit, []).

checkFluidumCircuit([Pipe|OtherPipes], Circuit, Tests) ->
	{ok, A} = maps:find(Pipe, Circuit),
	TestA = ?_assertEqual(A,processed),
	checkFluidumCircuit(OtherPipes, Circuit, [TestA |Tests]);

checkFluidumCircuit([], _, Tests) ->
	Tests.

%%%%%CHECKPUMPS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkPumps({Pumps,B}) -> 
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

	%setFlow
	Flow = 5,
	
	%pump is turned off, so flow should be 0, check if true
	{ok, G} = pumpInst:flow_influence(Pump),
	TestG = ?_assertEqual(G(Flow), 0),

	%turn on pump, flow should be 5 now, check if true
	pumpInst:switch_on(Pump),
	{ok, H} = pumpInst:flow_influence(Pump),
	TestH = ?_assertEqual(H(Flow), 250 - 5 * Flow - 2 * Flow * Flow),
	
	%returning the tests
	NewTests = [TestA, TestB, TestC, TestD, TestE, TestF, TestG, TestH],
	[NewTests|PumpTests];

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
	
	%setFlow
	Flow = 5,
	
	%pump is turned off, so flow should be 0, check if true
	{ok, G} = pumpInst:flow_influence(Pump),
	TestG = ?_assertEqual(G(Flow), 0),

	%turn on pump, flow should be 5 now, check if true
	pumpInst:switch_on(Pump),
	{ok, H} = pumpInst:flow_influence(Pump),
	TestH = ?_assertEqual(H(Flow), 250 - 5 * Flow - 2 * Flow * Flow),

	%add new tests and check next pump
	NewTests = [TestA, TestB, TestC, TestD, TestE, TestF, TestG, TestH],
	checkPumps(N-1, OtherPumps, [NewTests|PumpTests]).

%%%%%CHECKFLOWMETER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFM({FM, Pipes}) ->
	%check the measure_flow function
	{ok, A} = flowMeterInst:measure_flow(FM),
	TestA = ?_assertEqual(A,{ok, real_flow}),
	
	%check the estimate_flow function
	{ok, B} = flowMeterInst:estimate_flow(FM),
	TestB = ?_assertEqual(B, compute({0,10}, getReference(Pipes, []))),	
	[TestA, TestB].

getReference([], Reference) ->
	Reference;

getReference(Pipes, Reference) ->
	[Pipe|OtherPipes] = Pipes,
	{ok, Ref} = apply(resource_instance, get_flow_influence, [Pipe]),
	getReference(OtherPipes, Reference++[Ref]).

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

%%%%%CHECKHES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkHEs({HEs, C}) ->
	checkHEs(C, HEs, []).

checkHEs(1, HEs, Tests) ->
	[HE|_] = HEs,
	
	%check if HE exists	
	TestA = ?_assert(erlang:is_process_alive(HE)),

	%test temp_influence
	{ok, {ok, Inf}} = heatExchangerInst:temp_influence(HE),
	{ok, B} = Inf(5, 20),
	TestB = ?_assertEqual(B, 20 + (1/5)),
	
	%return all tests
	[TestA, TestB| Tests];

checkHEs(N, HEs, Tests) ->
	[HE|OtherHEs] = HEs,
	
	%check if HE exists	
	TestA = ?_assert(erlang:is_process_alive(HE)),

	%test temp_influence
	{ok, {ok, Inf}} = heatExchangerInst:temp_influence(HE),
	{ok, B} = Inf(5, 20),
	TestB = ?_assertEqual(B, 20 + (1/5)),

	%add new tests to Tests and check the next HE
	checkHEs(N-1, OtherHEs, [TestA, TestB| Tests]).



