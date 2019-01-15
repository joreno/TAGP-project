-module(circuit).
-export([createSimpleCircuit/0, createNPipes/1, createPipes/3, createPumps/4, createFMs/4, createHEs/4, connectPipes/1, stop/0, getConnectors/1, createCircuit/0, createCircuitWithPump/0, createCircuitWithPumpAndFlowmeter/0, createFullCircuit/0, startSurvivor/0, createComplexCircuit/4]).

%%%%%Circuits%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createSimpleCircuit() ->
	%creates a simple circuit with 3 pipes
	%Pipes
	{ok, PipeTypePid} = resource_type:create(pipeTyp, []),		
	{ok, PipeAInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeBInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeCInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, [A1, A2]} = resource_instance:list_connectors(PipeAInstPid),
	{ok, [B1, B2]} = resource_instance:list_connectors(PipeBInstPid),
	{ok, [C1, C2]} = resource_instance:list_connectors(PipeCInstPid),
	{ok, [LocA]} = resource_instance:list_locations(PipeAInstPid),
	{ok, [LocB]} = resource_instance:list_locations(PipeBInstPid),
	{ok, [LocC]} = resource_instance:list_locations(PipeCInstPid),
	connector:connect(A1, C2),
	connector:connect(A2, B1),
	connector:connect(B2, C1),

	{ok,{
		PipeAInstPid,
		[PipeAInstPid, PipeBInstPid, PipeCInstPid],
		[A1, C2, A2, B1, B2, C1],
		[LocA, LocB, LocC]
	    }
	}.	

createNPipes(N) ->
	%creates a simple circuit with N pipes
	{ok, PipeTypePid} = resource_type:create(pipeTyp,[]),
	Pipes = createPipes(N,[],PipeTypePid),
	connectPipes(Pipes),
	{ok, {PipeTypePid, Pipes, getConnectors(Pipes), getLocations(Pipes)}}.

createCircuit() ->
	%creates a circuit with 3 pipes and fills it with water
	%Pipes
	{ok, PipeTypePid} = resource_type:create(pipeTyp, []),		
	{ok, PipeAInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeBInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeCInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, [A1, A2]} = resource_instance:list_connectors(PipeAInstPid),
	{ok, [B1, B2]} = resource_instance:list_connectors(PipeBInstPid),
	{ok, [C1, C2]} = resource_instance:list_connectors(PipeCInstPid),
	connector:connect(A1, C2),
	connector:connect(A2, B1),
	connector:connect(B2, C1),
	
	Pipes = [ PipeAInstPid, PipeBInstPid, PipeCInstPid],
	Cons = getConnectors(Pipes),
	Locs = getLocations(Pipes),
	
	%Fluidum
	FluidumType = fluidumTyp:create(),
	{ok, FluidumInst} = fluidumInst:create(A1, FluidumType),

	{ok, {PipeTypePid, Pipes, Cons, Locs, FluidumType, FluidumInst}}.

createCircuitWithPump() ->
	%creates a circuit with 3 pipes and a pump and fills it with water.
	%Pipes	
	{ok, PipeTypePid} = resource_type:create(pipeTyp, []),		
	{ok, PipeAInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeBInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeCInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, [A1, A2]} = resource_instance:list_connectors(PipeAInstPid),
	{ok, [B1, B2]} = resource_instance:list_connectors(PipeBInstPid),
	{ok, [C1, C2]} = resource_instance:list_connectors(PipeCInstPid),
	connector:connect(A1, C2),
	connector:connect(A2, B1),
	connector:connect(B2, C1),
	{ok, PumpType} = pumpTyp:create(),
	{ok, PumpInst} = pumpInst:create(self(), PumpType, PipeAInstPid, fun(on) -> {ok, on}; (off) -> {ok, off} end),
	 
	Pipes = [PipeAInstPid,PipeBInstPid,PipeCInstPid],
	Cons = getConnectors(Pipes),
	Locs = getLocations(Pipes),
	
	%Fluidum
	FluidumType = fluidumTyp:create(),
	{ok, FluidumInst} = fluidumInst:create(A1, FluidumType),
	
	%Pump
	{ok, {PipeTypePid, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}}.
	
createCircuitWithPumpAndFlowmeter() ->
	%creates a circuit with 3 pipes, a pump, a flowmeter and fluid
	%pipes	
	{ok, PipeTypePid} = resource_type:create(pipeTyp, []),		
	{ok, PipeAInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeBInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeCInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, [A1, A2]} = resource_instance:list_connectors(PipeAInstPid),
	{ok, [B1, B2]} = resource_instance:list_connectors(PipeBInstPid),
	{ok, [C1, C2]} = resource_instance:list_connectors(PipeCInstPid),
	connector:connect(A1, C2),
	connector:connect(A2, B1),
	connector:connect(B2, C1),

	Pipes = [PipeAInstPid,PipeBInstPid,PipeCInstPid],
	Cons = getConnectors(Pipes),
	Locs = getLocations(Pipes),

	%pump	
	{ok, PumpType} = pumpTyp:create(),
	{ok, PumpInst} = pumpInst:create(self(), PumpType, PipeAInstPid, fun(on) -> {ok, on}; (off) -> {ok, off} end),
			 
	%fluid
	FluidumType = fluidumTyp:create(),
	{ok, FluidumInst} = fluidumInst:create(A1, FluidumType),
	
	%flowmeter
	{ok, FlowmeterType} = flowMeterTyp:create(),
	Fm = fun() ->	{ok, real_flow} end,
	{ok, FlowmeterInst} = flowMeterInst:create(self(), FlowmeterType, PipeBInstPid, Fm),

	{ok, {PipeTypePid, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst}}.
	
createFullCircuit() ->
	% Creates a circuit with 3 pipes, a pump, a flowmeter, a heatexchanger and a fluid	
	%pipes	
	{ok, PipeTypePid} = resource_type:create(pipeTyp, []),		
	{ok, PipeAInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeBInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeCInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, [A1, A2]} = resource_instance:list_connectors(PipeAInstPid),
	{ok, [B1, B2]} = resource_instance:list_connectors(PipeBInstPid),
	{ok, [C1, C2]} = resource_instance:list_connectors(PipeCInstPid),
	connector:connect(A1, C2),
	connector:connect(A2, B1),
	connector:connect(B2, C1),

	Pipes = [PipeAInstPid,PipeBInstPid,PipeCInstPid],
	Cons = getConnectors(Pipes),
	Locs = getLocations(Pipes),
	
	%pump	
	{ok, PumpType} = pumpTyp:create(),
	{ok, PumpInst} = pumpInst:create(self(), PumpType, PipeAInstPid, fun(on) -> {ok, on}; (off) -> {ok, off} end),
			 
	%fluid
	FluidumType = fluidumTyp:create(),
	{ok, FluidumInst} = fluidumInst:create(A1, FluidumType),
	
	%flowmeter
	{ok, FlowmeterType} = flowMeterTyp:create(),
	Fm = fun() ->	{ok, real_flow} end,
	{ok, FlowmeterInst} = flowMeterInst:create(self(), FlowmeterType, PipeBInstPid, Fm),
	
	%heatexchanger
	{ok, HEType} = heatExchangerTyp:create(),
	{ok, HEInst} = heatExchangerInst:create(self(), HEType,PipeCInstPid, #{delta => 1}),
	{ok, {PipeTypePid, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst, FlowmeterType, FlowmeterInst, HEType, HEInst}}.
	

createComplexCircuit(A,B,C,D) ->
	% creates a circuit with A pipes, B pumps, C flowmeters, D heatexchangers and a fluid
	%first create A pipes:	
	{ok, PipeType} = resource_type:create(pipeTyp,[]),
	Pipes = createPipes(A, [], PipeType),
	connectPipes(Pipes),
	Cons = getConnectors(Pipes),
	Locs = getLocations(Pipes),
	
	%next add the fluid:
	FluidumType = fluidumTyp:create(),
	[ConA|_] = Cons,
	{ok, Fluidum} = fluidumInst:create(ConA, FluidumType),

	%create B pumps:
	{ok, PumpType} = pumpTyp:create(),
	{ok, {Pumps, AvPipes}} = createPumps(B, [], PumpType, Pipes), 
	
	%create C flowmeters:
	{ok, FlowmeterType} = flowMeterTyp:create(),
	{ok, {Flowmeters, AvPipes2}} = createFMs(C, [], FlowmeterType, AvPipes),
	
	%create D Heatexchangers: 
	{ok, HEType} = heatExchangerTyp:create(),
	{ok, Heatexs} = createHEs(D, [], HEType, AvPipes2),
	
	%return everything:
	Types = [PipeType, FluidumType, PumpType, FlowmeterType, HEType],
	{ok,{Types, Pipes, Cons, Locs, Fluidum, Pumps, Flowmeters, Heatexs}}.
	
%%%%%Other Functions%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createPipes(1, PipeList, PipeTypePid) ->
	{ok, PipeInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	[PipeInstPid | PipeList];

createPipes(N, PipeList, PipeTypePid) ->
	if N >1 ->
		{ok, PipeInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
		createPipes(N-1, [PipeInstPid|PipeList] , PipeTypePid);
	true ->
		{error, "N < 0"}
	end.

connectPipes([FirstPipe|OtherPipes]) ->
	connectPipes(FirstPipe, FirstPipe, OtherPipes).

connectPipes(FirstPipe, Pipe, [MiddlePipe | OtherPipes]) ->
	{ok,[_PA1,PA2]} = resource_instance:list_connectors(Pipe),
	{ok,[PB1,_PB2]} = resource_instance:list_connectors(MiddlePipe),
	connector:connect(PA2, PB1),
	connectPipes(FirstPipe, MiddlePipe, OtherPipes);

connectPipes(FirstPipe, Pipe, []) ->
	{ok,[PA1,_PA2]} = resource_instance:list_connectors(FirstPipe),
	{ok,[_PB1,PB2]} = resource_instance:list_connectors(Pipe),
	connector:connect(PA1, PB2).


stop() ->
	survivor ! stop,
	{ok, stopped}.
	
getConnectors(Pipes) ->	getConnectors(Pipes,[]).
	
getConnectors([Pipe|OtherPipes],Connectors) ->
	{ok,Con} = resource_instance:list_connectors(Pipe),
	getConnectors(OtherPipes,Connectors++Con);
	
getConnectors([],Connectors) ->
	Connectors.

getLocations(Pipes) -> getLocations(Pipes,[]).
getLocations([FirstPipe|OtherPipes], Locations) ->
	{ok, [NewLocation]} = resource_instance:list_locations(FirstPipe),
	getLocations(OtherPipes, lists:append(Locations, [NewLocation]));

getLocations([], Locations) -> Locations.

startSurvivor() ->
	survivor:start(),
	{ok, survivorHasStarted}.

createPumps(1, PumpList, PumpType, Pipes) ->
	{Pipe, OtherPipes} = lists:split(1,Pipes),
	{ok, PumpInst} = pumpInst:create(self(), PumpType, Pipe, fun(on) -> {ok, on}; (off) -> {ok, off} end),
	{ok, {PumpList ++[PumpInst], OtherPipes}};

createPumps(N, PumpList, PumpType, Pipes) ->
	if N > 1 ->
		{Pipe, OtherPipes} = lists:split(1,Pipes),
		{ok, PumpInst} = pumpInst:create(self(), PumpType, Pipe, fun(on) -> {ok, on}; (off) -> {ok, off} end),
		createPumps(N-1, PumpList ++[PumpInst], PumpType, OtherPipes);

	true ->
		{error, "N<0"}
	end.

createFMs(1, FMList, FMType, Pipes) ->
	{Pipe, OtherPipes} = lists:split(1,Pipes),
	{ok, FMInst} = flowMeterInst:create(self(), FMType, Pipe, fun() -> {ok, real_flow} end),
	{ok,{FMList ++[FMInst], OtherPipes}};

createFMs(N, FMList, FMType, Pipes) ->
	if N > 1 ->
		{Pipe, OtherPipes} = lists:split(1,Pipes),
		{ok, FMInst} = flowMeterInst:create(self(), FMType, Pipe, fun() -> {ok, real_flow} end),
		createFMs(N-1, FMList ++[FMInst], FMType, OtherPipes);
	true ->
		{error, "N<0"}
	end.

createHEs(1, HEList, HEType, Pipes) ->
	{Pipe, _} = lists:split(1, Pipes),
	{ok, HEInst} = heatExchangerInst:create(self(), HEType, Pipe, #{delta => 1}),
	{ok, HEList ++[HEInst]};

createHEs(N, HEList, HEType, Pipes) ->
	if N > 1 ->
		{Pipe, OtherPipes} = lists:split(1, Pipes),
		{ok, HEInst} = heatExchangerInst:create(self(), HEType, Pipe, #{delta => 1}),
		createHEs(N-1, HEList ++[HEInst], HEType, OtherPipes);
	true ->
		{error, "N<0"}
	end.

				
	
