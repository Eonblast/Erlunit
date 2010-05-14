%%%----------------------------------------------------------------------------
%%% File        : testtest.erl
%%% Description : Test of test functions in erlunit.erl
%%% Type        : Internal, for development of Erlunit
%%% Version     : 0.2.8/alpha
%%% Status      : alpha
%%% Copyright   : (c) 2010 Eonblast Corporation http://www.eonblast.com
%%% License     : MIT - http://www.opensource.org/licenses/mit-license.php 
%%% Author      : H. Diedrich <hd2010@eonblast.com>
%%% Created     : 18 Apr 2010
%%% Changed     : 23 Apr 2010 - see CHANGES
%%% Tested on   : Erlang R13B01
%%%----------------------------------------------------------------------------
%%%
%%% This program is for development of Erlunit, it tests the test functions.
%%%
%%% It's not part of what is needed to use Erlunit. It is not a regular Erlunit
%%% test either, it can break somewhat less controlled.
%%% 
%%% To learn about Erlunit, read sample.erl
%%% 
%%%----------------------------------------------------------------------------
%%%
%%% Usage 
%%%
%%% #  erl
%%% 1> {ok,_} = c(erlunit), {ok,_} = c(testtest), testtest:run().
%%%  
%%% Copy and paste, it will break before executing run() if there are errors.  
%%%  
%%%----------------------------------------------------------------------------
%%%
%%% Mail to hd2010@eonblast.com with questions and suggestions, I will
%%% be happy to answer. - Henning
%%%
%%%----------------------------------------------------------------------------

-module(testtest).

-vsn("0.2.8/alpha").
-author("H. Diedrich <hd2010@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010 Eonblast Corporation http://www.eonblast.com").

%%%----------------------------------------------------------------------------

-export([run/0]).
-import(erlunit).
-include("erlunit.hrl").

-compile({nowarn_unused_function, [suite_verbose/0]}).

%-export([one/0]).

-define(VERSION, "0.2.8/alpha").
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

	suite_complete(),
	suite_complete_inverted(),
	suite_complete_verbose(),
	suite_complete_verbose_mp(),
	suite_complete_mp(),

	suite_macros(),
	suite_macros_inverted(),

	erlunit:execute().


%%%****************************************************************************
%%% SAMPLE SUITES
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% #1 Minimal
%%%----------------------------------------------------------------------------

suite_minimal_seq() ->

	erlunit:suite("#1 minimal - sequential"),

	erlunit:true(1 == 1),
	erlunit:equal(fun() -> one() end, 1),
	erlunit:fail(fun() -> 1 / zero() end).

	% using zero() for 0 to avoid Erlang compile time warnings

suite_minimal_msg() ->

	Suite = erlunit:suite("#2 minimal - message passing"),

	Suite ! { true, 1 == 1 },
	Suite ! { equal, 1, 1 }.
%	Suite ! { fail, fun() -> 1 / zero() end }.

	% using zero() for 0 to avoid Erlang compile time warnings

%%%
%%%----------------------------------------------------------------------------
%%% #3: all types of checks
%%%----------------------------------------------------------------------------
%%%

suite_complete() ->

	erlunit:suite("#3 all types of checks - sequential"),

	% true and false
	% -------------------------------
	erlunit:true(true),
	erlunit:true(1 == 1),
	erlunit:not_true(false),
	erlunit:not_true(1 == 2),
	erlunit:not_true(x),
	erlunit:false(false),
	erlunit:false(1 == 0),
	erlunit:not_false(true),
	erlunit:not_false(1 == 1),
	erlunit:not_false(x),

	% true and false / with functions
	% -------------------------------
	erlunit:true(fun() -> true end),
	erlunit:not_true(fun() -> x end),
	erlunit:not_true(fun() -> false end),
	erlunit:false(fun() -> false end),
	erlunit:not_false(fun() -> x end),
	erlunit:not_false(fun() -> true end),

	% equality
	% -------------------------------
	erlunit:equal(1, 1),
	erlunit:equal(a, a),
	erlunit:equal("a", "a"),
	erlunit:exact(1, 1),
	erlunit:exact(a, a),
	erlunit:exact("a", "a"),
	erlunit:not_equal(1, 0),
	erlunit:not_equal(1, true),
	erlunit:not_equal(1, false),
	erlunit:bigger(2, 1),
	erlunit:bigger(a, 1),
	erlunit:bigger(self(), a),
	erlunit:lesser(1, 2),
	erlunit:lesser(1, a),
	erlunit:lesser(1, self()),

	% equality (with functions)
	% -------------------------------
	erlunit:equal(fun() -> 1 end, 1),
	erlunit:exact(fun() -> 1 end, 1),
	erlunit:not_equal(fun() -> 1 end, 0),
	erlunit:bigger(2, fun() -> 1 end),
	erlunit:lesser(fun() -> 1 end, 2),

	% control always (functions)
	% -------------------------------
	erlunit:pass(fun() -> 1 + 0 end),
	erlunit:fail(fun() -> throw(sic) end),
	erlunit:fail(fun() -> 1 / zero() end),
	erlunit:fail(fun() -> exit(sic) end),
	erlunit:throws(fun() -> throw(sic) end),
	erlunit:error(fun() -> 1 / zero() end),
	erlunit:exits(fun() -> exit(sic) end),
	
	ok.

	% using zero() for 0 to avoid Erlang compile time warnings

%%%
%%%----------------------------------------------------------------------------
%%% #4: same as above, but inverted
%%%----------------------------------------------------------------------------
%%%
%%% Inversion is for testing the test functions. Not for normal use of Erlunit.
%%% It switches the suite so that it counts passes as fails and vice versa.
%%% Crashes are still counted as crashes, as it should be. Remember that crashes
%%% are not at all the same as a fail(), error(), exits() or throws(). Those
%%% latter catch failing test cases, if they fail on purpose or not. Crashes are
%%% failures higher up in the Erlunit mechanics that point to an error in 
%%% Erlunit or a mistake on the part of the developer using Erlunit.

suite_complete_inverted() ->

	erlunit:suite("#4 - All checks/inverted", [inverted]),

	% true and false
	% -------------------------------
	% (inverted)
	erlunit:not_true(true),
	erlunit:not_true(1 == 1),
	erlunit:true(false),
	erlunit:true(1 == 2),
	erlunit:true(x),
	erlunit:not_false(false),
	erlunit:not_false(1 == 0),
	erlunit:false(true),
	erlunit:false(1 == 1),
	erlunit:false(x),

	% true and false / with functions
	% -------------------------------
	% (inverted)
	erlunit:not_true(fun() -> true end),
	erlunit:true(fun() -> x end),
	erlunit:true(fun() -> false end),
	erlunit:not_false(fun() -> false end),
	erlunit:false(fun() -> x end),
	erlunit:false(fun() -> true end),

	% equality
	% -------------------------------
	% (inverted)
	erlunit:not_equal(1, 1),
	erlunit:not_equal(a, a),
	erlunit:not_equal("a", "a"),
	erlunit:equal(1, 0),
	erlunit:equal(1, true),
	erlunit:equal(1, false),
	erlunit:exact(1, 1.0),  % =:= -> not -> pass as inverted.
	erlunit:exact(1, true),
	erlunit:exact(1, false),
	erlunit:lesser(2, 1),
	erlunit:lesser(a, 1),
	erlunit:lesser(self(), a),
	erlunit:bigger(1, 2),
	erlunit:bigger(1, a),
	erlunit:bigger(1, self()),

	% equality (with functions)
	% -------------------------------
	% (inverted)
	erlunit:not_equal(fun() -> 1 end, 1),
	erlunit:equal(fun() -> 1 end, 0),
	erlunit:exact(fun() -> 1 end, 0),
	erlunit:lesser(2, fun() -> 1 end),
	erlunit:bigger(fun() -> 1 end, 2),

	% control always (functions)
	% -------------------------------
	% (inverted)
	erlunit:fail(fun() -> 1 + 0 end),
	erlunit:pass(fun() -> throw(sic) end),
	erlunit:pass(fun() -> 1 / zero() end),
	erlunit:pass(fun() -> exit(sic) end),
	erlunit:error(fun() -> throw(sic) end),
	erlunit:exits(fun() -> throw(sic) end),
	erlunit:throws(fun() -> 1 / zero() end),
	erlunit:exits(fun() -> 1 / zero() end),
	erlunit:throws(fun() -> exit(sic) end),
	erlunit:error(fun() -> exit(sic) end),
	
	ok.

	% using zero() for 0 to avoid Erlang compile time warnings

%%%
%%%----------------------------------------------------------------------------
%%% #5: same as above, with names for individual checks = 'verbose'
%%%----------------------------------------------------------------------------
%%%

suite_complete_verbose() ->

	erlunit:suite("#5 - All types, verbose"),

	% true and false
	% -------------------------------
	erlunit:true(true, "True is true!"),
	erlunit:true(1 == 1, "1=1 is true!"),
	erlunit:not_true(false, "False is not true!"),
	erlunit:not_true(1 == 2, "1=2 is not true!"),
	erlunit:not_true(x, "x is not true!"),
	erlunit:false(false, "False is false :-o"),
	erlunit:false(1 == 0, "1=0 is false :-("),
	erlunit:not_false(true, "True is not false :-)"),
	erlunit:not_false(1 == 1, "1=1 is not false :-)"),
	erlunit:not_false(x, "x is not false :-)"),

	% true and false / with functions
	% -------------------------------
	erlunit:true(fun() -> true end, "True is true, w/function"),
	erlunit:not_true(fun() -> x end, "X is not true, w/function"),
	erlunit:not_true(fun() -> false end, "False is not true, w/function"),
	erlunit:false(fun() -> false end, "False is false, w/function"),
	erlunit:not_false(fun() -> x end, "X is not false :-) w/function"),
	erlunit:not_false(fun() -> true end, "True is not false :-) w/function"),

	% equality
	% -------------------------------
	erlunit:equal(1, 1, "1=1 Equality"),
	erlunit:equal(a, a, "Atom Equality"),
	erlunit:equal("a", "a", "String Equality"),
	erlunit:exact(1, 1, "1=1 Exact Equality"),
	erlunit:exact(a, a, "Atom Exact Equality"),
	erlunit:exact("a", "a", "String Exact Equality"),
	erlunit:not_equal(1, 0, "Numberic Non-Equality"),
	erlunit:not_equal(1, true, "Type Inequality"),
	erlunit:not_equal(1, false, "Type Inequality"),
	erlunit:bigger(2, 1, "Numeric Bigger"),
	erlunit:bigger(a, 1, "Bigger By Type"),
	erlunit:bigger(self(), a, "Bigger By Type II"),
	erlunit:lesser(1, 2, "Numberic Lesser"),
	erlunit:lesser(1, a, "Number lesser than Atom"),
	erlunit:lesser(1, self(), "Type of Number smaller than Pid"),

	% equality (with functions)
	% -------------------------------
	erlunit:equal(fun() -> 1 end, 1, "1=1 Equality, w/function"),
	erlunit:exact(fun() -> 1 end, 1, "1=1 Exact Equality, w/function"),
	erlunit:not_equal(fun() -> 1 end, 0, "1=0 Inequality, w/function"),
	erlunit:bigger(2, fun() -> 1 end, "2>1, w/function"),
	erlunit:lesser(fun() -> 1 end, 2, "1<2, w/function"),

	% control always (functions)
	% -------------------------------
	erlunit:pass(fun() -> 1 + 0 end, "Pass of Calculation, w/function"),
	erlunit:fail(fun() -> throw(sic) end, "Fail by throw, w/function"),
	erlunit:fail(fun() -> 1 / zero() end, "Fail by error, w/function"),
	erlunit:fail(fun() -> exit(sic) end, "Fail by exit, w/function"),
	erlunit:throws(fun() -> throw(sic) end, "Control Exception, w/function"),
	erlunit:error(fun() -> 1 / zero() end, "Control Error Condition, w/function"),
	erlunit:exits(fun() -> exit(sic) end, "Control Exit, w/function"),
	
	ok.

	% using zero() for 0 to avoid Erlang compile time warnings
%%%
%%%
%%%----------------------------------------------------------------------------
%%% #6: same as above, names for every check, but message passing style
%%%----------------------------------------------------------------------------
%%%

suite_complete_verbose_mp() ->

	Suite = erlunit:suite( "#6 - All types, verbose, message passing style" ),

	% true and false
	% -------------------------------
	Suite ! { true, true, "True is true!" },
	Suite ! { true, 1 == 1, "1=1 is true!" },
	Suite ! { not_true, false, "False is not true!" },
	Suite ! { not_true, 1 == 2, "1=2 is not true!" },
	Suite ! { not_true, x, "x is not true!" },
	Suite ! { false, false, "False is false :-o" },
	Suite ! { false, 1 == 0, "1=0 is false :-(" },
	Suite ! { not_false, true, "True is not false :-)" },
	Suite ! { not_false, 1 == 1, "1=1 is not false :-)" },
	Suite ! { not_false, x, "x is not false :-)" },

	% true and false / with functions
	% -------------------------------
	Suite ! { true, fun() -> true end, "True is true, w/function" },
	Suite ! { not_true, fun() -> x end, "X is not true, w/function" },
	Suite ! { not_true, fun() -> false end, "False is not true, w/function" },
	Suite ! { false, fun() -> false end, "False is false, w/function" },
	Suite ! { not_false, fun() -> x end, "X is not false :-) w/function" },
	Suite ! { not_false, fun() -> true end, "True is not false :-) w/function" },

	% equality
	% -------------------------------
	Suite ! { equal, 1, 1, "1=1 Equality" },
	Suite ! { equal, a, a, "Atom Equality" },
	Suite ! { equal, "a", "a", "String Equality" },
	Suite ! { exact, 1, 1, "1=1 Exact Equality" },
	Suite ! { exact, a, a, "Atom Exact Equality" },
	Suite ! { exact, "a", "a", "String Exact Equality" },
	Suite ! { not_equal, 1, 0, "Numberic Non-Equality" },
	Suite ! { not_equal, 1, true, "Type Inequality" },
	Suite ! { not_equal, 1, false, "Type Inequality" },
	Suite ! { bigger, 2, 1, "Numeric Bigger" },
	Suite ! { bigger, a, 1, "Bigger By Type" },
	Suite ! { bigger, self(), a, "Bigger By Type II" },
	Suite ! { lesser, 1, 2, "Numberic Lesser" },
	Suite ! { lesser, 1, a, "Number lesser than Atom" },
	Suite ! { lesser, 1, self(), "Type of Number smaller than Pid" },

	% equality (with functions)
	% -------------------------------
	Suite ! { equal, fun() -> 1 end, 1, "1=1 Equality, w/function" },
	Suite ! { exact, fun() -> 1 end, 1, "1=1 Exact Equality, w/function" },
	Suite ! { not_equal, fun() -> 1 end, 0, "1=0 Inequality, w/function" },
	Suite ! { bigger, 2, fun() -> 1 end, "2>1, w/function" },
	Suite ! { lesser, fun() -> 1 end, 2, "1<2, w/function" },

	% control always (functions)
	% -------------------------------
	Suite ! { pass, fun() -> 1 + 0 end, "Pass of Calculation, w/function" },
	Suite ! { fail, fun() -> throw(sic) end, "Fail by throw, w/function" },
	Suite ! { fail, fun() -> 1 / zero() end, "Fail by error, w/function" },
	Suite ! { fail, fun() -> exit(sic) end, "Fail by exit, w/function" },
	Suite ! { throws, fun() -> throw(sic) end, "Control Exception, w/function" },
	Suite ! { error, fun() -> 1 / zero() end, "Control Error Condition, w/function" },
	Suite ! { exits, fun() -> exit(sic) end, "Control Exit, w/function" },
	
	ok.

	% using zero() for 0 to avoid Erlang compile time warnings
%%%
%%%
%%%----------------------------------------------------------------------------
%%% #7: same as above, but without text element
%%%----------------------------------------------------------------------------
%%%
suite_complete_mp() ->

	Suite = erlunit:suite( "#7 - All types, non-verbose, message passing style" ),

	% true and false
	% -------------------------------
	Suite ! { true, true },
	Suite ! { true, 1 == 1 },
	Suite ! { not_true, false },
	Suite ! { not_true, 1 == 2 },
	Suite ! { not_true, x },
	Suite ! { false, false },
	Suite ! { false, 1 == 0 },
	Suite ! { not_false, true },
	Suite ! { not_false, 1 == 1 },
	Suite ! { not_false, x },

	% true and false / with functions
	% -------------------------------
	Suite ! { true, fun() -> true end },
	Suite ! { not_true, fun() -> x end },
	Suite ! { not_true, fun() -> false end },
	Suite ! { false, fun() -> false end },
	Suite ! { not_false, fun() -> x end },
	Suite ! { not_false, fun() -> true end },

	% equality
	% -------------------------------
	Suite ! { equal, 1, 1 },
	Suite ! { equal, a, a },
	Suite ! { equal, "a", "a" },
	Suite ! { exact, 1, 1 },
	Suite ! { exact, a, a },
	Suite ! { exact, "a", "a" },
	Suite ! { not_equal, 1, 0 },
	Suite ! { not_equal, 1, true },
	Suite ! { not_equal, 1, false },
	Suite ! { bigger, 2, 1 },
	Suite ! { bigger, a, 1 },
	Suite ! { bigger, self(), a },
	Suite ! { lesser, 1, 2 },
	Suite ! { lesser, 1, a },
	Suite ! { lesser, 1, self() },

	% equality (with functions)
	% -------------------------------
	Suite ! { equal, fun() -> 1 end, 1 },
	Suite ! { exact, fun() -> 1 end, 1 },
	Suite ! { not_equal, fun() -> 1 end, 0 },
	Suite ! { bigger, 2, fun() -> 1 end },
	Suite ! { lesser, fun() -> 1 end, 2 },

	% control always (functions)
	% -------------------------------
	Suite ! { pass, fun() -> 1 + 0 end },
	Suite ! { fail, fun() -> throw(sic) end },
	Suite ! { fail, fun() -> 1 / zero() end },
	Suite ! { fail, fun() -> exit(sic) end },
	Suite ! { throws, fun() -> throw(sic) end },
	Suite ! { error, fun() -> 1 / zero() end },
	Suite ! { exits, fun() -> exit(sic) end },
	
	ok.

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

%%%
%%%----------------------------------------------------------------------------
%%% #8: macros
%%%----------------------------------------------------------------------------
%%%

suite_macros() ->

	erlunit:suite("#8 macros"),

	?ERLUNIT_TRUE(1 == 1),
	?ERLUNIT_TRUE(_A = true), % sic
	?ERLUNIT_TRUE(0 < b),  % sic

	?ERLUNIT_TRUE_MSG(1 == 1, "One equals One"),
	?ERLUNIT_TRUE_MSG(_A = true, "A is assigned true"),
	?ERLUNIT_TRUE_MSG(0 < b,  "A Number Is Smaller Than an Atom"),

	?ERLUNIT_NOT_TRUE(1 == 2),
	?ERLUNIT_NOT_TRUE(1),
	?ERLUNIT_NOT_TRUE(0 > b),

	?ERLUNIT_NOT_TRUE_MSG(1 == 2, "One is not Two"),
	?ERLUNIT_NOT_TRUE_MSG(1, "One is not True"),
	?ERLUNIT_NOT_TRUE_MSG(0 > b, "A Number Is Not Bigger Than an Atom"),

	?ERLUNIT_FALSE(1 == 2),
	?ERLUNIT_FALSE(false),
	?ERLUNIT_FALSE(0 > b),

	?ERLUNIT_FALSE_MSG(1 == 2, "One is not Two"),
	?ERLUNIT_FALSE_MSG(not true, "Not true is false"),
	?ERLUNIT_FALSE_MSG(0 > b, "A Number Is Not Bigger Than an Atom"),

	?ERLUNIT_NOT_FALSE(1 == 1),
	?ERLUNIT_NOT_FALSE(1),
	?ERLUNIT_NOT_FALSE(0 < b),

	?ERLUNIT_NOT_FALSE_MSG(1 == 1, "One is One"),
	?ERLUNIT_NOT_FALSE_MSG(1, "One is not False"),
	?ERLUNIT_NOT_FALSE_MSG(0 < b, "A Number Is Smaller Than an Atom"),

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	?ERLUNIT_FAIL(1 / zero()),
	?ERLUNIT_FAIL(exit(sic)),
	?ERLUNIT_FAIL(throw(sic)),

	?ERLUNIT_FAIL_MSG(1 / zero(), "Error: One By Zero"),
	?ERLUNIT_FAIL_MSG(exit(sic), "Exit"),
	?ERLUNIT_FAIL_MSG(throw(sic), "Throw"),

	?ERLUNIT_PASS(1 / 1),
	?ERLUNIT_PASS(_A = a),
	?ERLUNIT_PASS(0 < b),

	?ERLUNIT_PASS_MSG(1 / 1, "One by One"),
	?ERLUNIT_PASS_MSG(_A = a, "A Can Become a"),
	?ERLUNIT_PASS_MSG(0 < b, "A Number Is Smaller Than an Atom"),

	?ERLUNIT_EXITS(exit(sic)),
	?ERLUNIT_EXITS_MSG(exit(sic), "Exit"),

	?ERLUNIT_THROWS(throw(sic)),
	?ERLUNIT_THROWS_MSG(throw(sic), "Throw"),

	?ERLUNIT_ERROR(1 / zero()),
	?ERLUNIT_ERROR_MSG(1 / zero(), "Error: One By Zero"),

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	?ERLUNIT_EQUAL(true, true),
	?ERLUNIT_EQUAL(1, 1.0),
	?ERLUNIT_EQUAL(0, -0),
	
	?ERLUNIT_EQUAL_MSG(true, true, "True is True"),
	?ERLUNIT_EQUAL_MSG(1, 1.0, "One (Integer) is One (Float)"),
	?ERLUNIT_EQUAL_MSG(0, -0, "Zero is minus Zero"),
	
	?ERLUNIT_NOT_EQUAL(true, false),
	?ERLUNIT_NOT_EQUAL(1, 2),
	?ERLUNIT_NOT_EQUAL(true, 1),
	
	?ERLUNIT_NOT_EQUAL_MSG(true, false, "True is not equal False"),
	?ERLUNIT_NOT_EQUAL_MSG(1, 2, "One is not equal Two"),
	?ERLUNIT_NOT_EQUAL_MSG(true, 1, "True is not equal 1"),

	?ERLUNIT_EXACT(true, true),
	?ERLUNIT_EXACT(1, 1),
	?ERLUNIT_EXACT(true, 1 == 1),
	
	?ERLUNIT_EXACT_MSG(true, true, "True exactly equals True"),
	?ERLUNIT_EXACT_MSG(1, 1, "One exactly equals One"),
	?ERLUNIT_EXACT_MSG(true, 1==1, "True exactly equals (1==1)"),

	?ERLUNIT_BIGGER(2, 1),
	?ERLUNIT_BIGGER(a, 0),
	?ERLUNIT_BIGGER(true, false),
	
	?ERLUNIT_BIGGER_MSG(2, 1, "Two is bigger than One"),
	?ERLUNIT_BIGGER_MSG(a, 0, "An Atom is bigger than Zero"),
	?ERLUNIT_BIGGER_MSG(true, false, "True is bigger than false"),
	
	?ERLUNIT_LESSER(1, 2),
	?ERLUNIT_LESSER(0, a),
	?ERLUNIT_LESSER(1, 1.1),
	
	?ERLUNIT_LESSER_MSG(1, 2, "One is lesser than Two"),
	?ERLUNIT_LESSER_MSG(0, a, "Zero is lesser than an Atom"),
	?ERLUNIT_LESSER_MSG(1, 1.1, "One is lesser than One point One"),

	ok.

	% using zero() for 0 to avoid Erlang compile time warnings

%%%
%%%----------------------------------------------------------------------------
%%% #9: macros, inverted
%%%----------------------------------------------------------------------------
%%%

suite_macros_inverted() ->

	erlunit:suite("#9 - macros, inverted", [inverted]),

	
	% inverted!
	?ERLUNIT_TRUE(1 /= 1),
	?ERLUNIT_TRUE(_A = false), % sic
	?ERLUNIT_TRUE(0 > b),  % sic

	% inverted!
	?ERLUNIT_TRUE_MSG(1 /= 1, "One equals One"),
	?ERLUNIT_TRUE_MSG(_A = false, "A is assigned false"),
	?ERLUNIT_TRUE_MSG(0 > b,  "A Number Is Bigger Than an Atom"),

	% inverted!
	?ERLUNIT_NOT_TRUE(1 /= 2),
	?ERLUNIT_NOT_TRUE(true),
	?ERLUNIT_NOT_TRUE(0 < b),

	% inverted!
	?ERLUNIT_NOT_TRUE_MSG(1 == 1, "One is One"),
	?ERLUNIT_NOT_TRUE_MSG(true, "True is not True"),
	?ERLUNIT_NOT_TRUE_MSG(0 < b, "A Number Is Smaller Than an Atom"),

	% inverted!
	?ERLUNIT_FALSE(1 == 1),
	?ERLUNIT_FALSE(true),
	?ERLUNIT_FALSE(0 < b),

	% inverted!
	?ERLUNIT_FALSE_MSG(1 == 1, "One is One"),
	?ERLUNIT_FALSE_MSG(not false, "Not false is false"),
	?ERLUNIT_FALSE_MSG(0 < b, "A Number Is Smaller Than an Atom"),

	% inverted!
	?ERLUNIT_NOT_FALSE(1 == 2),
	?ERLUNIT_NOT_FALSE(false),
	?ERLUNIT_NOT_FALSE(0 > b),

	% inverted!
	?ERLUNIT_NOT_FALSE_MSG(1 == 2, "One is Two"),
	?ERLUNIT_NOT_FALSE_MSG(false, "False is not False"),
	?ERLUNIT_NOT_FALSE_MSG(0 > b, "A Number Is Bigger Than an Atom"),

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	% inverted!
	?ERLUNIT_FAIL(_A = a),
	?ERLUNIT_FAIL_MSG(_A = a, "A = a"),

	% expected to crash, which will in this inverted suite count as pass:
	?ERLUNIT_PASS(1 / zero()),
	?ERLUNIT_PASS(exit(sic)), 
	?ERLUNIT_PASS(throw(sic)),

	% inverted!
	?ERLUNIT_PASS_MSG(1 / zero(), "One By Zero"),
	?ERLUNIT_PASS_MSG(exit(sic), "Exit"),
	?ERLUNIT_PASS_MSG(throw(sic), "Throw"),

	% inverted!
	?ERLUNIT_THROWS(exit(sic)),
	?ERLUNIT_THROWS(_A = a),
	?ERLUNIT_THROWS(1 / zero()),

	% inverted!
	?ERLUNIT_THROWS_MSG(exit(sic),  "Exit"),
	?ERLUNIT_THROWS_MSG(_A = a, "A = a"),
	?ERLUNIT_THROWS_MSG(1 / zero(), "Error (1/0)"),

	% inverted!
	?ERLUNIT_ERROR(exit(sic)),
	?ERLUNIT_ERROR(throw(sic)),
	?ERLUNIT_ERROR(_A = a),

	% inverted!
	?ERLUNIT_ERROR_MSG(exit(sic),  "Exit"),
	?ERLUNIT_ERROR_MSG(throw(sic), "Throw"),
	?ERLUNIT_ERROR_MSG(_A = a, "A = a"),

	% inverted!
	?ERLUNIT_EXITS(_A = a),
	?ERLUNIT_EXITS(throw(sic)),
	?ERLUNIT_EXITS(1 / zero()),

	% inverted!
	?ERLUNIT_EXITS_MSG(_A = a,  "A = a"),
	?ERLUNIT_EXITS_MSG(throw(sic), "Throw"),
	?ERLUNIT_EXITS_MSG(1 / zero(), "Error (1/0)"),


%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	% inverted!
	?ERLUNIT_EQUAL(true, false),
	?ERLUNIT_EQUAL(1, 2),
	?ERLUNIT_EQUAL(a, b),
	
	% inverted!
	?ERLUNIT_EQUAL_MSG(true, false, "True is False"),
	?ERLUNIT_EQUAL_MSG(1, 2, "One is Two"),
	?ERLUNIT_EQUAL_MSG(a, b, "a is b"),
	
	% inverted!
	?ERLUNIT_NOT_EQUAL(true, true),
	?ERLUNIT_NOT_EQUAL(1, 1),
	?ERLUNIT_NOT_EQUAL(true, 1 == 1),
	
	% inverted!
	?ERLUNIT_NOT_EQUAL_MSG(true, true, "True is not equal True"),
	?ERLUNIT_NOT_EQUAL_MSG(1, 1, "One is not equal One"),
	?ERLUNIT_NOT_EQUAL_MSG(true, 1 == 1, "True is equal (1 == 1)"),

	% inverted!
	?ERLUNIT_EXACT(true, false),
	?ERLUNIT_EXACT(1, 1.0),
	?ERLUNIT_EXACT(false, 1 == 1),
	
	% inverted!
	?ERLUNIT_EXACT_MSG(true, false, "True exactly equals False"),
	?ERLUNIT_EXACT_MSG(1, 1.0, "One (Integer) exactly equals One (Integer)"),
	?ERLUNIT_EXACT_MSG(false, 1==1, "False exactly equals (1==1)"),

	% inverted!
	?ERLUNIT_BIGGER(1, 1),
	?ERLUNIT_BIGGER(0, a),
	?ERLUNIT_BIGGER(false, true),
	
	% inverted!
	?ERLUNIT_BIGGER_MSG(1, 2, "One is bigger than Two"),
	?ERLUNIT_BIGGER_MSG(0, a, "An Number is bigger than an Atom"),
	?ERLUNIT_BIGGER_MSG(falser, truer, "Falser is bigger than Truer"),
	
	% inverted!
	?ERLUNIT_LESSER(2, 1),
	?ERLUNIT_LESSER(a, 0),
	?ERLUNIT_LESSER(1.1, 1),
	
	% inverted!
	?ERLUNIT_LESSER_MSG(2, 1, "Two is lesser than One"),
	?ERLUNIT_LESSER_MSG(a, 0, "a is lesser than 0"),
	?ERLUNIT_LESSER_MSG(1.1, 1, "One point one is lesser than One"),

	ok.

	% using zero() for 0 to avoid Erlang compile time warnings

%%%
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
%%% View online at http://github.com/Eonblast/Erlunit/blob/master/erlunit.erl