%%%----------------------------------------------------------------------------
%%% File        : sample.erl
%%% Description : Sample usage of test functions in erlunit.erl
%%% Version     : 0.1
%%% Status      : beta
%%% Copyright   : (c) 2010 Eonblast Corporation http://www.eonblast.com
%%% License     : MIT - http://www.opensource.org/licenses/mit-license.php 
%%% Author      : H. Diedrich <hd2010@eonblast.com>
%%% Created     : 18 Apr 2010
%%% Changed     : 22 Apr 2010 - see CHANGES
%%% Tested on   : Erlang R13B01
%%%----------------------------------------------------------------------------
%%%
%%% This program demonstrates the use of the Erlunit test functions.
%%%
%%% It can be read top down. There are explanation sections mixed in.
%%% 
%%% You should read the two pages covering samples 1, 2, 3 to get started. 
%%% 
%%%
%%%----------------------------------------------------------------------------
%%%
%%% Prerequisite: Erlang installed. From R12B-4 might do. R13B-1 will.
%%%
%%%----------------------------------------------------------------------------
%%%
%%% Usage is: 'reading this source'. 
%%%
%%% Try, then alter and re-try the samples 1, 2, 3, 4, 5, 6, 10:
%%%
%%% # erl
%%% 1> c(erlunit), c(sample), sample:sample1().
%%%  
%%% Run all samples at once:
%%%
%%% # erl
%%% 1> c(erlunit), c(sample), sample:run().
%%%
%%%----------------------------------------------------------------------------
%%%
%%% This is a naive approach at an academically discussed issue. Plus,
%%% there are Erlang masters on erlang-questions, this here source is
%%% not written by one. Beware of copying mistakes if you are learning.
%%%
%%% Mail to hd2010@eonblast.com with questions and suggestions, I will
%%% be happy to answer. - Henning
%%%
%%%----------------------------------------------------------------------------

-module(sample).

-export([run/0, sample1/0, sample2/0, sample3/0, sample4/0, sample5/0]).
-export([sample6/0,sample10/0]).
-export([banner/1, banner2/1]).

-import(erlunit).

-compile({nowarn_unused_function, [center/2]}).


-define(VERSION, "0.1").
-define(PROGRAM, "Test Samples").

%%%****************************************************************************
%%% #1 SUPER SIMPLE SAMPLE - SEQUENTIAL CALL STYLE
%%%****************************************************************************

sample1() ->

	banner("Super simple demonstration #1 - imperative style."),
	
	erlunit:start(),
	erlunit:equal(1, 1),
	erlunit:execute().


	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample1().

	% --- TRY THIS: Alter to erlunit:equal(1, 2) and run again.
	% ---           The test will 'fail' and say so. 

%%%-------------------------------------------------------------------------#1-
%%%
%%% Tests are written to test functions of your own source. There is a
%%% a lot to be said for writing tests for your source and even write
%%% test before you write your source: to clarify and define your task
%%% and to make life easier once you change implementations.
%%%
%%% See http://www.c2.com/cgi/wiki?TestDrivenDevelopment for more.
%%%
%%% Note that your code comes into play where 1 is used in this first 
%%% sample. Eg. to test your function Foo() that is expected to always
%%% result into 42 when fed with a bar, you'd write a test like this: 
%%% equal(Foo(bar), 42). This way you have a test coded that will make
%%% sure that Foo(bar) still results into 42, whenever you run it, no
%%% matter how many source iterations later you come back to it.
%%%
%%%----------------------------------------------------------------------------

%%%****************************************************************************
%%% #2 SUPER SIMPLE SAMPLE - MESSAGE PASSING STYLE
%%%****************************************************************************

%%%-------------------------------------------------------------------------#2-
%%%
%%% The above was a sequential example and if you come from an impera-
%%% tive language you might feel more at home with it. Why not use it.
%%%
%%% The following notation uses Erlang message passing to achieve the
%%% same results. If you use that you will be able to execute tests
%%% concurrently later.
%%%
%%%----------------------------------------------------------------------------


sample2() ->

	banner("Super simple demonstration #2 - message passing style."),
	
	Test = erlunit:start(),
	Test ! { equal, 1, 1 },
	erlunit:execute().
	

	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample2().


%%%****************************************************************************
%%% #3 SUPER SIMPLE SAMPLE: SUITES, SEQUENTIAL
%%%****************************************************************************

%%%-------------------------------------------------------------------------#3-
%%%
%%% 'Suites' are used to group individual tests. 
%%%
%%% A suite is created with erlunit:suite("<suite name>"). Tests are
%%% grouped into the suite last created, by default.
%%%
%%%----------------------------------------------------------------------------

sample3() ->

	banner("Super simple demonstration #3 - suites."),
	
	erlunit:start(),

	erlunit:suite("#1"),
	erlunit:equal(100, 100),

	erlunit:suite("#2"),
	erlunit:not_equal(a, b),

	erlunit:execute().


	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample3a().


%%%****************************************************************************
%%% #4 SUPER SIMPLE SAMPLE: THREE SUITES, SEQUENTIAL
%%%****************************************************************************
sample4() ->

	banner("Super simple demonstration #4 - concurrent suites."),
	
	erlunit:start(),

	erlunit:suite("#1"),
	erlunit:equal(100, 100),

	erlunit:suite("#2"),
	erlunit:not_equal(1, 2),


	erlunit:suite("#3"),
	erlunit:not_equal("A", 3),

	erlunit:execute().


	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample3b().
	

%%%****************************************************************************
%%% #5 SIMPLE SAMPLE with suites
%%%****************************************************************************

%%%----------------------------------------------------------------------------
%%%
%%% Suites can be run concurrently and are by 
%%%
%%%----------------------------------------------------------------------------

sample5() ->

	erlunit:banner(),
	banner("Simple demonstration of test functions: parallel with suites."),
	erlunit:echo("Suites to run concurrently and their output will interleave."),
	
	erlunit:start(),

	Suite1 = erlunit:suite("#1"),
	Suite1 ! { equal,  1,  1 },
	Suite1 ! { equal,  2,  2 },
	Suite1 ! { equal,  3,  3 },
	Suite1 ! { equal,  4,  4 },
	Suite1 ! { equal,  5,  5 },
	Suite1 ! { equal,  6,  6 },
	Suite1 ! { equal,  7,  7 },
	Suite1 ! { equal,  8,  8 },
	Suite1 ! { equal,  9,  9 },
	Suite1 ! { equal, 10, 10 },

	Suite2 = erlunit:suite("#2"),
	Suite2 ! { not_equal, 1, 2 },
	Suite2 ! { not_equal, 1, 3 },
	Suite2 ! { not_equal, 1, 4 },

	erlunit:execute(),
	timer:sleep(1000).

	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample2().


%%%****************************************************************************
%%% #6 SIMPLE SAMPLE with parallel suites
%%%****************************************************************************

sample6() ->

	banner("Simple demonstration of test functions: with parallel suites."),
	
	erlunit:start(),

	Suite1 = erlunit:suite("#1"),
	Suite1 ! { equal, 1, 1 },
	Suite1 ! { equal, 2, 2 },
	Suite1 ! { equal, 3, 3 },

	Suite2 = erlunit:suite("#2"),
	Suite2 ! { not_equal, 1, 2 },
	Suite2 ! { not_equal, 1, 3 },
	Suite2 ! { not_equal, 1, 4 },

	erlunit:execute().

	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample6().


%%%****************************************************************************
%%% #10 MORE STRUCTURED SAMPLE - SUITES
%%%****************************************************************************

sample10() ->

	banner("Demonstrating the use of suite functions."),
	
	erlunit:start(),
	
	suite1(),
	suite2(),
	
	erlunit:execute().

%%%****************************************************************************
%%% SAMPLE SUITES
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% #1: Very Simple
%%%----------------------------------------------------------------------------

suite1() ->

	erlunit:suite("#1 - Very Simple"),

	erlunit:true(1 == 1),
	erlunit:equal(1, 1),
	erlunit:fail(fun() -> 1 / zero() end).

	% using zero() for 0 to avoid Erlang compile time warnings

%%%
%%%----------------------------------------------------------------------------
%%% #2: same as above, with names for individual checks
%%%----------------------------------------------------------------------------

suite2() ->

	erlunit:suite("#2 - Very Simple / verbose"),
	erlunit:echo("Suite #2 gives names to individual checks"),

	erlunit:true(1 == 1, "One Equals One"),
	erlunit:false(1 == 0, "One Equals Zero"),

	erlunit:equal(1, 1, "Again Equality"),
	erlunit:not_equal(1, 0, "And Non-Equality"),

	erlunit:pass(fun() -> 1 + 0 end, "Trivial Fun"),
	erlunit:fail(fun() -> 1 / zero() end, "Div By Zero").

	% using zero() for 0 to avoid Erlang compile time warnings

	
% run_suite2() ->
%
%	erlunit:suite("2"),
%
%	erlunit:echo("This suite is intended to have failures"),
%
%	erlunit:equal(1,1,"1 and 1 are equal"),
%	erlunit:notequal(1,1,"1 and 1 are not equal"),
%	erlunit:lesser(1,1,"1 is lesser than 1"),
%	erlunit:bigger(1,1,"1 is bigger than 1"),
%
%	erlunit:suite_end().


%%%****************************************************************************
%%% run - run all samples
%%%****************************************************************************

run() ->

	banner("ALL SAMPLES"),
	
	sample1(),
	sample2(),
	sample3(),
	sample4(),
	sample5(),
	sample6(),
	sample10().


%%%****************************************************************************
%%% UTILITY
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% This is used to avoid Erlang compile time warnings.
%%%----------------------------------------------------------------------------

zero() -> 0.

%%%****************************************************************************
%%% SCREEN UTILITY
%%%****************************************************************************
%%%

-define(WIDTH, 75).
-define(DASHLINE, string:chars($*, ?WIDTH)).

%%%----------------------------------------------------------------------------
%%% Banner
%%%----------------------------------------------------------------------------

%%% Centered banner with text only:

banner(Text) ->

    io:format("~s~n~s~s~n~s~n",[?DASHLINE, center_indent(Text, ?WIDTH), Text,?DASHLINE]).

%%% Left-aligned banner with program name, version and message text:

banner2(Message) ->
    io:format("~s~n~s ~s~n~s~n~s~n",[?DASHLINE, ?PROGRAM, ?VERSION, Message,?DASHLINE]).

%%%----------------------------------------------------------------------------
%%% Centering a headline
%%%----------------------------------------------------------------------------

center(Text, Width) ->

	center_indent(Text, Width) ++ Text.

center_indent(Text, Width) ->
	string:chars(32, erlang:max(0, trunc((Width - length(Text)) / 2))).
	

%%%----------------------------------------------------------------------------
%%% Further reading
%%%----------------------------------------------------------------------------
	
%%% The actual test functions are in erlunit.erl
%%% Click to view http://github.com/hdiedrich/erlunit/blob/master/erlunit.erl