-module(circuit).
-export([startCircuit/0, createNPipes/1, createPipes/3, connectPipes/1, stop/0, getConnectors/1]).

startCircuit() ->
	survivor:start(),
	observer:start(),
	{ok, PipeTypePid} = resource_type:create(pipeTyp, []),		
	{ok, PipeAInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeBInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, PipeCInstPid} = resource_instance:create(pipeInst, [self(), PipeTypePid]),
	{ok, [PA1, PA2]} = resource_instance:list_connectors(PipeAInstPid),
	{ok, [PB1, PB2]} = resource_instance:list_connectors(PipeBInstPid),
	{ok, [PC1, PC2]} = resource_instance:list_connectors(PipeCInstPid),
	{ok, [LocA]} = resource_instance:list_locations(PipeAInstPid),
	{ok, [LocB]} = resource_instance:list_locations(PipeBInstPid),
	{ok, [LocC]} = resource_instance:list_locations(PipeCInstPid),
	connector:connect(PA1, PC2),
	connector:connect(PA2, PB1),
	connector:connect(PB2, PC1),

	{ok,{
		PipeAInstPid,
		[PipeAInstPid, PipeBInstPid, PipeCInstPid],
		[[PA1, PC2], [PA2, PB1], [PB2, PC1]],
		[LocA, LocB, LocC]
	    }
	}.	

createNPipes(N) ->
	survivor:start(),
	observer:start(),
	{ok, PipeTypePid} = resource_type:create(pipeTyp,[]),
	Pipes = createPipes(N,[],PipeTypePid),
	connectPipes(Pipes),
	{ok, {PipeTypePid, Pipes, getConnectors(Pipes), getLocations(Pipes)}}.

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

