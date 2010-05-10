%%%----------------------------------------------------------------------------
%%% File        : quick.erl
%%% Description : Quick Start for erlunit.erl
%%% Type        : Documentation
%%% Version     : 0.2.6/alpha
%%% Status      : alpha
%%%----------------------------------------------------------------------------
%%%
%%% Usage:
%%%
%%%      #  erl
%%%      1> c(erlunit), c(quick), quick:run().
%%%  
%%%----------------------------------------------------------------------------

-module(quick).
-import(erlunit).
-export([run/0, run2/0]).

%%%****************************************************************************
%%% QUICK START - SEQUENTIAL CALL STYLE
%%%****************************************************************************

run() ->

	erlunit:start(),
	erlunit:equal(1, 1),
	erlunit:execute().

	% TRY: Alter it to ... erlunit:equal(1, 2) ... and run again. 

%%%****************************************************************************
%%% QUICK START - MESSAGE PASSING STYLE
%%%****************************************************************************

%%% Same as above, different notation. Allows for concurrent tests.

run2() ->

	Test = erlunit:start(),
	Test ! { equal, 1, 1 },
	erlunit:execute().

	% TRY: Alter it to ... Test ! { fail(fun() -> 1 / 0 end)) } ... and run again. 

%%%----------------------------------------------------------------------------
%%% Also: equal, true, false, bigger, lesser, pass, fail, throws, error, exits.
%%% More: read sample.erl next. It's written to be read top down. Or MANUAL.
%%% Home: http://github.com/Eonblast/Erlunit
%%%----------------------------------------------------------------------------
