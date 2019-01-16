-module(prop_complexCircuit).
-include_lib("proper/include/proper.hrl").

prop_testFlowInfluence() ->
    survivor:start(),
    Test = ?FORALL(Flow, integer(0,1000),testFlowInf(Flow)),

    timer:send_after(1000, survivor, stop),
    Test.

prop_testEstimatedFlow() ->
    survivor:start(),
    Test = ?FORALL(N, integer(4,30), testEstFlow(N)),
    timer:send_after(1000, survivor, stop),
    Test.

%===========================================================================================
%HELP FUNCTIONS
%===========================================================================================

testFlowInf(Flow)->
    {ok, {_,_,_,_,_,_,_, Pump}} = circuit:createCircuitWithPump(),
    pumpInst:switch_on(Pump),
    {ok, Inf} = pumpInst:flow_influence(Pump),
    Inf(Flow) == 250 - 5*Flow-2*Flow*Flow.


testEstFlow(N) ->
    {ok,{_,Pipes,_,_,_,_, FM,_}} = complexCircuit:createComplexCircuit(N, 1, 1),
    {ok, A} = flowMeterInst:estimate_flow(FM),
    timer:sleep(1),
    A == compute({0, 10}, getReference(Pipes, [])).

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


