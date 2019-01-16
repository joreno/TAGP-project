TAGP-project
=====

This project simulates a Digital Twin Circuit that exists out of a number of components (pipe, pump, flowmeter and heatexchanger) and can be filled with a fluidum. The source code for these components was provided by: https://github.com/ValckenaersPaul/TAGP. The purpose of the project was to create a module that links all these components and the fluidum together. Next, EUnit and PropEr tests had to be implemented to test the module. To run the application, rebar3 was used.

Build
-----
To run the application, first compile:

	$ rebar3 compile

Next run the application by:

	$ rebar3 shell

An erl shell will be started, in which the application has been started. A circuit has already been created and some information about this circuit is printed in your terminal. The created circuit can be found and modified in the init/0-function of project_sup.erl.

The Digital Twin Circuit
-----

The implementation of the complete Digital Twin Circuit can be found in the file complexCircuit.erl. The function that creates this circuit is createComplexCircuit/3. The parameters are three positive integers and refer respectively to the number of pipes, pumps and heatexchangers. 

Smaller circuits can also be created, their implementations are found in the file circuit.erl.

Dialyzer tests
-----
Dialyzer is a tol to find type errors in Erlang code. To run this tool on the application, the following command is used:
	
	$ rebar3 dialyzer

The Dialyzer tests showed several errors throughout the creation of the application. But all these errors have been resolved.

EUnit tests
-----
EUnit is a tool used to unit test the implementation and functioning of the Digital Twin Circuit.
The testfile that includes the EUnit tests for the Digital Twin Circuit is complexCircuit_tests.erl. 
Unit tests for the smaller circuits were als created an can be found in circuit_tests.erl.

To run the EUnit tests, the following command is used:
	
	$ rebar3 eunit

Throughout the project, many eunit tests have failed over many errors. But eventually, running EUnit will return "616 tests, 0 failures".

PropEr tests
-----
PropEr is a tool used for property-based testing. For this project, the PropEr-tests can be found in prop_complexCircuit.erl. 

To run the PropEr tests, the following command is used:

	$ rebar3 proper

Running this command will also not return any errors.


