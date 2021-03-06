-module(circuit).
-export([createSimpleCircuit/0, createNPipes/1, createPipes/3, connectPipes/1, stop/0, getConnectors/1, createCircuit/0, createCircuitWithPump/0]).

createSimpleCircuit() ->
	%creates a simple circuit with 3 pipes
	survivor:start(),
	%observer:start(),
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
		[[A1, C2], [A2, B1], [B2, C1]],
		[LocA, LocB, LocC]
	    }
	}.	

createNPipes(N) ->
	%creates a simple circuit with N pipes
	survivor:start(),
	%observer:start(),
	{ok, PipeTypePid} = resource_type:create(pipeTyp,[]),
	Pipes = createPipes(N,[],PipeTypePid),
	connectPipes(Pipes),
	{ok, {PipeTypePid, Pipes, getConnectors(Pipes), getLocations(Pipes)}}.

createCircuit() ->
	%creates a circuit with 3 pipes and fills it with water
	survivor:start(),	
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
	
	FluidumType = fluidumTyp:create(),
	{ok, FluidumInst} = fluidumInst:create(A1, FluidumType),

	{ok, {PipeTypePid, Pipes, Cons, Locs, FluidumType, FluidumInst}}.

createCircuitWithPump() ->
	%creates a circuit with 3 pipes and a pump and fills it with water.
	survivor:start(),	
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
	
	FluidumType = fluidumTyp:create(),
	{ok, FluidumInst} = fluidumInst:create(A1, FluidumType),

	{ok, {PipeTypePid, Pipes, Cons, Locs, FluidumType, FluidumInst, PumpType, PumpInst}}.
	
	
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

