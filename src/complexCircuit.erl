-module(complexCircuit).
-export([createComplexCircuit/3]).
-export([createPipes/3, createPumps/4, createHEs/4, connectPipes/1, stop/0, getConnectors/1, startSurvivor/0]).

%%%%%Creation of the Complex Circuit%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createComplexCircuit(A,B,C) ->
	% creates a circuit with A pipes, B pumps, a flowmeter, C heatexchangers and a fluid
	% A has to be bigger than B+C+1, otherwise some of the other instances can't be placed
	if A >= B+C+1 ->
		%first create A pipes:
		io:format("Creating ~p pipes~n", [A]),	
		{ok, PipeType} = resource_type:create(pipeTyp,[]),
		Pipes = createPipes(A, [], PipeType),
		io:format("~p pipes are created, connecting them together now~n", [A]),	
		connectPipes(Pipes),
		Cons = getConnectors(Pipes),
		Locs = getLocations(Pipes),
		io:format("Connections are made, filling the pipes with water~n"),	
		
		%next add the fluid:
		FluidumType = fluidumTyp:create(),
		[ConA|_] = Cons,
		{ok, Fluidum} = fluidumInst:create(ConA, FluidumType),
		fillCircuit(Fluidum, Locs),
		io:format("Circuit has been filled with water~n"),

		%create B pumps:
		io:format("Adding ~p pumps to the circuit~n", [B]),
		{ok, PumpType} = pumpTyp:create(),
		{ok, {Pumps, AvPipes}} = createPumps(B, [], PumpType, Pipes), 
		io:format("~p pumps have been added to the circuit~n", [B]),

		%create a flowmeter:		
		io:format("Adding a flowmeter to the circuit~n"),	
		{ok, FMType} = flowMeterTyp:create(),
		[Pipe|AvPipes2] = AvPipes,
		{ok, FM} = flowMeterInst:create(self(), FMType, Pipe, fun() -> {ok, real_flow} end), 
		io:format("Flowmeter has been added to the circuit~n"),
		
		%create C Heatexchangers:		
		io:format("Adding ~p heatexchangers to the circuit~n", [C]), 
		{ok, HEType} = heatExchangerTyp:create(),
		{ok, HEs} = createHEs(C, [], HEType, AvPipes2),
		io:format("~p heatexchangers have been added to the circuit~n", [B]),		
	
		%return everything:
		io:format("Circuit has been created:~nPipes are: ~p~nConnections between these pipes are ~p~nThe pipes are located in ~p~nThe fluidum is: ~p~nThe pumps are: ~p~nThe flowmeter is: ~p~nThe heatexchangers are: ~p~n", [Pipes, Cons, Locs, Fluidum, Pumps, FM, HEs]),
		Types = [PipeType, FluidumType, PumpType, FMType, HEType],
		{ok,{Types, Pipes, Cons, Locs, Fluidum, Pumps, FM, HEs}};
	true ->
		io:format("There is not enough space to place all these components! You will need ~p more pipes~n", [(B+C+1)-A]),
		{error, "Not enough Pipes!!!"}
	end.


%%%%%Other Functions%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

fillCircuit(_,[]) ->
	ok;

fillCircuit(Fluidum, Locs) ->
	[Loc|OtherLocs] = Locs,	
	location:arrival(Loc, Fluidum),
	fillCircuit(Fluidum, OtherLocs).


