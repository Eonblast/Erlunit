%%%----------------------------------------------------------------------------
%%% File        : test.erl
%%% Description : Test of test functions in erlunit.erl
%%% Version     : 0.2
%%% Status      : alpha
%%% Copyright   : (c) 2010 Eonblast Corporation http://www.eonblast.com
%%% License     : MIT - http://www.opensource.org/licenses/mit-license.php 
%%% Author      : H. Diedrich <hd2010@eonblast.com>
%%% Created     : 18 Apr 2010
%%% Changed     : 22 Apr 2010 - see CHANGES
%%% Tested on   : Erlang R13B01
%%%----------------------------------------------------------------------------
%%%
%%% This program is for development if Erlunit, it tests the Erlunit functions.
%%%
%%% It's not a regular Erlunit test, it breaks less controlled.
%%% 
%%% To learn about Erlunit, read sample.erl
%%% 
%%%----------------------------------------------------------------------------
%%%
%%% Usage 
%%%
%%% # erl
%%% 1> c(erlunit), c(test), test:run().
%%%  
%%%----------------------------------------------------------------------------
%%%
%%% Mail to hd2010@eonblast.com with questions and suggestions, I will
%%% be happy to answer. - Henning
%%%
%%%----------------------------------------------------------------------------

-module(test).

-vsn(0.2).
-author("H. Diedrich <hd2010@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010 Eonblast Corporation http://www.eonblast.com").

%%%----------------------------------------------------------------------------

-export([run/0]).
-import(erlunit).

-compile({nowarn_unused_function, [suite_verbose/0]}).

%-export([one/0]).

-define(VERSION, "0.2").
-define(PROGRAM, "Test-Test").

%%%****************************************************************************
%%% MAIN FUNCTION
%%%****************************************************************************

run() ->

	banner("Testing the test functions"),
	
	erlunit:start(),

	erlunit:echo("Testing the test functions, basically by trying them all."),

	suite_minimal_seq(),
	suite_minimal_msg(),

	erlunit:execute().


%%%****************************************************************************
%%% SAMPLE SUITES
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% #1 Minimal
%%%----------------------------------------------------------------------------

suite_minimal_seq() ->

	erlunit:suite("Minimal / sequential"),

	erlunit:true(1 == 1),
	erlunit:equal(fun() -> one() end, 1),
	erlunit:fail(fun() -> 1 / zero() end).

	% using zero() for 0 to avoid Erlang compile time warnings

suite_minimal_msg() ->

	Suite = erlunit:suite("Minimal / message passing"),

	Suite ! { true, 1 == 1 },
	Suite ! { equal, 1, 1 }.
%	Suite ! { fail, fun() -> 1 / zero() end }.

	% using zero() for 0 to avoid Erlang compile time warnings

%%%
%%%----------------------------------------------------------------------------
%%% #2: same as above, with names for individual checks
%%%----------------------------------------------------------------------------
%%%
%%% NOT YET USED

suite_verbose() ->

	erlunit:suite("#2 - Very Simple / verbose"),
	erlunit:echo("Suite #2 gives names to individual checks"),

	erlunit:true(1 == 1, "One Equals One"),
	erlunit:false(1 == 0, "One Equals Zero"),

	erlunit:not_true(1 == 2, "One Does Not Equal Two"),
	erlunit:not_false(foo, "Foo Is Not True"),
	erlunit:not_false(1 == 1, "One Not Not Equals One"),
	erlunit:not_false(bar, "Bar Is Not False"),

	erlunit:equal(1, 1, "Once More Equality"),
	erlunit:not_equal(1, 0, "And Non-Equality"),

	erlunit:pass(fun() -> 1 + 0 end, "Trivial Fun"),
	erlunit:fail(fun() -> 1 / zero() end, "Div By Zero").

	% using zero() for 0 to avoid Erlang compile time warnings

%%%****************************************************************************
%%% UTILITY
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% This is used to avoid Erlang compile time warnings.
%%%----------------------------------------------------------------------------

zero() -> 0.

%%%
%%%----------------------------------------------------------------------------
%%% This is used to test the automatic execution of funs in payload/1.
%%%----------------------------------------------------------------------------

one() -> 1.

%%%****************************************************************************
%%% SCREEN UTILITY
%%%****************************************************************************
%%%
%%% This duplicates utility source in sample.erl

-define(WIDTH, 75).
-define(DASHLINE, string:chars($*, ?WIDTH)).

%%%----------------------------------------------------------------------------
%%% Banner
%%%----------------------------------------------------------------------------
%%%
%%% Centered banner with text only

banner(Text) ->

    io:format("~s~n~s~s~n~s~n",[?DASHLINE, center_indent(Text, ?WIDTH), Text,?DASHLINE]).

%%%----------------------------------------------------------------------------
%%% Centering a headline
%%%----------------------------------------------------------------------------

center_indent(Text, Width) ->
	string:chars(32, erlang:max(0, trunc((Width - length(Text)) / 2))).
	

%%%----------------------------------------------------------------------------
%%% Further reading
%%%----------------------------------------------------------------------------
%%%
%%% The actual test functions are in erlunit.erl
%%% View online at http://github.com/hdiedrich/erlunit/blob/master/erlunit.erl