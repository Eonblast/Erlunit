%%%----------------------------------------------------------------------------
%%% File        : sample.erl
%%% Description : Sample usage of test functions in erlunit.erl
%%% Version     : 0.2.5/alpha
%%% Status      : alpha
%%% Copyright   : (c) 2010 Eonblast Corporation http://www.eonblast.com
%%% License     : MIT - http://www.opensource.org/licenses/mit-license.php 
%%% Author      : H. Diedrich <hd2010@eonblast.com>
%%% Created     : 18 Apr 2010
%%% Changed     : 03 May 2010 - see CHANGES
%%% Tested on   : Erlang R13B01
%%%----------------------------------------------------------------------------
%%%
%%% This program demonstrates the use of the Erlunit test functions.
%%%
%%% It can be read top down. There are explanation sections mixed in.
%%% 
%%% You should read the two pages covering samples 1, 2, 3 to get started. 
%%%
%%%----------------------------------------------------------------------------
%%%
%%% Prerequisite: Erlang installed. From R12B-4 might do. R13B-1 will.
%%%
%%%----------------------------------------------------------------------------
%%%
%%% Usage: reading and run. 
%%%
%%%      To find out if it all compiles on your machine:
%%%  
%%%      #  erl
%%%      1> {ok,_} = c(erlunit), {ok,_} = c(sample), sample:run().
%%%  
%%%      
%%%      To try individual samples, do (1, 2, 3, 4, 5, 10):
%%%
%%%      #  erl
%%%      1> c(erlunit), c(sample), sample:sample1().
%%%
%%%----------------------------------------------------------------------------
%%% :                                                                         : 
%%% : E r l u n i t - is a naive approach at an academically discussed issue. :
%%% : There are Erlang masters on the erlang-questions list, this here source :
%%% : ist not written by one. Beware of copying mistakes if you are learning. :
%%% :                                                                         : 
%%% : Thus consider these two serious contenders who deserve your attention:  :
%%% :                                                                         :
%%% : E U n i t - by Richard Carlsson and Michaêl Rémond.                     :
%%% : "EUnit is a unit testing framework for Erlang. It is very powerful and  :
%%% : flexible, is easy to use, and has small syntactical overhead."          :
%%% : http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html
%%% :                                                                         :
%%% : e t a p - by Jeremy Wall, Paul J. Davis, Nick Gerakines et al.          :
%%% : "etap is a collection of Erlang modules that provide a TAP testing      :
%%% : client library. These modules allow developers to create extensive and  :
%%% : comprehensive tests covering many aspects of application and module     :
%%% : development. This includes simple assertions, exceptions, the           :
%%% : application behavior and event web requests."                           :
%%% : - http://github.com/ngerakines/etap/downloads                           :
%%% :                                                                         : 
%%%----------------------------------------------------------------------------
%%% :                                                                         :
%%% : Mail to hd2010@eonblast.com with questions and suggestions, I will be   :
%%% : happy to answer. - Henning                                              :
%%% :                                                                         : 
%%%----------------------------------------------------------------------------

-module(sample).

-vsn("0.2.5/alpha").
-author("H. Diedrich <hd2010@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010 Eonblast Corporation http://www.eonblast.com").

%%%----------------------------------------------------------------------------

-export([run/0, sample1/0, sample2/0, sample3/0, sample4/0, sample5/0]).
-export([sample9/0,sample10/0]).

-import(erlunit).
-include("erlunit.hrl").

-compile({nowarn_unused_function, [banner/1]}).

-define(VERSION, "0.2.5/alpha").
-define(PROGRAM, "Test Samples").

%%%****************************************************************************
%%% #1 SUPER SIMPLE SAMPLE - SEQUENTIAL CALL STYLE
%%%****************************************************************************

sample1() ->

	erlunit:strong_banner("Super simple demonstration #1 - imperative style."),
	
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

	erlunit:strong_banner("Super simple demonstration #2 - message passing style."),
	
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

	erlunit:strong_banner("Super simple demonstration #3 - suites."),
	
	erlunit:start(),

	erlunit:suite("#1"),
	erlunit:equal(100, 100),

	erlunit:suite("#2"),
	erlunit:not_equal(a, b),

	erlunit:execute().


	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample3().


%%%****************************************************************************
%%% #4 SUPER SIMPLE SAMPLE: SREE SUITES, SEQUENTIAL
%%%****************************************************************************
sample4() ->

	erlunit:strong_banner("Super simple demonstration #4 - concurrent suites."),
	
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
	% --- 1> c(erlunit), c(sample), sample:sample4().
	

%%%****************************************************************************
%%% #5 SIMPLE SAMPLE - WITH SUITES
%%%****************************************************************************

%%%-------------------------------------------------------------------------#5-
%%%
%%% Suites can be run concurrently. Running this sample will show how the
%%% individual checks of both suits are neatly interleaving.
%%%
%%% This only works with the message passing notation. It exploits the effect
%%% that the suites are 'loaded' with checks pretty fast and the actual
%%% execution of the checks doesn't start before erlunit:execute() is called.
%%%
%%% Which in effect mostly means: wait for the suites to get done. And so it
%%% waits and gives the suites the chance to use the cpu time between them.
%%%
%%%----------------------------------------------------------------------------

sample5() ->

	erlunit:strong_banner("Simple demonstration of test functions: parallel with suites."),
	erlunit:echo("Suites will run concurrently and their output will *interleave*!"),
	
	erlunit:start(),

	Suite1 = erlunit:suite("#1"),
	Suite1 ! { true,  1 == 1 },
	% bug, harden: Suite1 ! { true,  2,  2 }, (right would be Suite1 ! { true,  2 ==  2 },
	Suite1 ! { equal,  3,  3 },
	Suite1 ! { equal,  4,  4 },
	Suite1 ! { equal,  5,  5 },
	Suite1 ! { equal,  6,  6 },
	Suite1 ! { equal,  7,  7 },
	Suite1 ! { equal,  8,  8 },
	Suite1 ! { equal,  9,  9 },
	Suite1 ! { equal, 10, 10 },

	Suite2 = erlunit:suite("#2"),
	Suite2 ! { not_equal, 1,  2 },
	Suite2 ! { not_equal, 1,  3 },
	Suite2 ! { not_equal, 1,  4 },
	Suite2 ! { not_equal, 1,  5 },
	Suite2 ! { not_equal, 1,  6 },
	Suite2 ! { not_equal, 1,  7 },
	Suite2 ! { not_equal, 1,  8 },
	Suite2 ! { not_equal, 1,  9 },
	Suite2 ! { not_equal, 1, 10 },

	erlunit:execute().


	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample5().

%%%****************************************************************************
%%% #9 SIMPLE SAMPLE - MACROS WITH SUITES
%%%****************************************************************************

%%%-------------------------------------------------------------------------#9-
%%%
%%% Macros reduce the boilerplate, i.e. save you a lot of typing and increase
%%% speed of pinpointing an error by adding line numbers to the output auto-
%%% matically. They also add a string representation of your source to a
%%% possible error message. Take a look at erlunit.hrl to check them out.
%%%
%%% E.g. ?ERLUNIT_EQUAL(F, R) is the same as if you wrote
%%% erlunit:equal(fun() -> F end, R, "Expect F == R", ?MODULE, ?LINE)).
%%%
%%% To use macros, you have to include the header erlunit.hrl in your source,
%%% like so:
%%% - include("erlunit.hrl").
%%%
%%%----------------------------------------------------------------------------

sample9() ->

	erlunit:strong_banner("Demonstration of Macros"),
	
	erlunit:start(),

	erlunit:suite("#1"),
	?ERLUNIT_EQUAL(1, 1.0),
	?ERLUNIT_EXACT(1, 1),
	?ERLUNIT_PASS(1 / 1),

	erlunit:suite("#2"),
	?ERLUNIT_FAIL(1 / zero()),

	erlunit:execute().


	% --- That's it. A complete test program. Run with 
	% --- # erl
	% --- 1> c(erlunit), c(sample), sample:sample9().


%%%****************************************************************************
%%% #10 MORE STRUCTURED SAMPLE - SUITES
%%%****************************************************************************

%%%------------------------------------------------------------------------#10-
%%%
%%% Suites are well suited to be put in functions to structure your code.
%%%
%%%----------------------------------------------------------------------------

sample10() ->

	erlunit:strong_banner("Demonstrating the use of suite functions."),
	
	erlunit:start(),
	
	suite1(),
	suite2(),
	
	erlunit:execute().

	
%                                     -(o)-                                   %   

%%%****************************************************************************
%%% SAMPLE SUITES
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% #1: Simple Suite
%%%----------------------------------------------------------------------------

suite1() ->

	erlunit:suite("#1/Simple"),

	erlunit:true(1 == 1),
	erlunit:equal(1, 1),
	erlunit:fail(fun() -> 1 / zero() end).

	% using zero() for 0 to avoid Erlang compile time warnings

%%%
%%%----------------------------------------------------------------------------
%%% #2: same as above, with names for individual checks
%%%----------------------------------------------------------------------------

suite2() ->

	erlunit:suite("#2/Simple (verbose)"),
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

	erlunit:strong_banner("ALL SAMPLES"),
	
	sample1(),
	sample2(),
	sample3(),
	sample4(),
	sample5(),
	sample9(),
	sample10().


%                                     -(o)-                                   %   

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
%%%----------------------------------------------------------------------------
%%% Light Banner
%%%----------------------------------------------------------------------------
%%% Left-aligned banner with program name, version and message text:

banner(Message) ->
    io:format("~s~n~s ~s~n~s~n~s~n",[erlunit:dashline(), ?PROGRAM, ?VERSION, Message, erlunit:dashline()]).


%%%----------------------------------------------------------------------------
%%% Further reading
%%%----------------------------------------------------------------------------
	
%%% The actual test functions are in erlunit.erl
%%% View online at http://github.com/hdiedrich/erlunit/blob/master/erlunit.erl