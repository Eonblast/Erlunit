%%%----------------------------------------------------------------------------
%%% File        : erlunit.erl
%%% Description : Test functions
%%% Version     : 0.1
%%% Status      : beta
%%% Copyright   : (c) 2010 Eonblast Corporation http://www.eonblast.com
%%% License     : MIT - see below 
%%% Author      : H. Diedrich <hd2010@eonblast.com>
%%% Created     : 18 Apr 2010
%%% Changed     : 22 Apr 2010 - see CHANGES
%%% Tested on   : Erlang R13B01
%%%----------------------------------------------------------------------------
%%%
%%% This Module contains test functions to test other modules.
%%%
%%% Usage, see sample.erl. Basically -
%%%
%%%    either:                   or:  
%%%	   erlunit:start()       |   Test = erlunit:start(),
%%%    erlunit:equal(1, 1),  |   Test ! { equal, 1, 1 },
%%%    ...  				 |   ...
%%%	   erlunit:execute().    |   erlunit:execute().
%%%
%%% There are also suites, names for tests and concurrent execution.
%%%
%%%----------------------------------------------------------------------------
%%%
%%% Start source inspection in sample.erl.
%%%
%%% There are Erlang masters on erlang-questions, this here source is
%%% not written by one. Beware of copying mistakes if you are learning.
%%%
%%% Mail to hd2010@eonblast.com with questions and suggestions, I will
%%% be quite happy to answer. - Henning
%%%
%%% This bit is dedicated to Joe, who had me smile now and then reading
%%% his book Programming Erlang. Thanks, Joe! You are the man.
%%%
%%%----------------------------------------------------------------------------
% 
% Copyright (c) 2010 Eonblast Corporation http://www.eonblast.com
% 
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
% 
% The above copyright notice, including link, and this permission notice shall 
% be included in all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.
% 
%%%----------------------------------------------------------------------------

-module(erlunit).
-vsn(0.1).
-author("H. Diedrich <hd2010@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010 Eonblast Corporation http://www.eonblast.com").

%%%----------------------------------------------------------------------------

-export([start/0, execute/0, stats/1]).
-export([suite/1, suite/2]).
-export([true/1, not_true/1, false/1, not_false/1, pass/1, fail/1]).
-export([true/2, not_true/2, false/2, not_false/2, pass/2, fail/2]).
-export([exits/1, throws/1, error/1]).
-export([exits/2, throws/2, error/2]).
-export([equal/2, not_equal/2, bigger/2, lesser/2]).
-export([equal/3, not_equal/3, bigger/3, lesser/3]).

-export([echo/1, echo/2]).
-export([banner/0, banner/1]).
-export([vecho/2, vecho/3]).
-export([alive/1]).

-export([glist/1, glist_add/3, glist_get/2, glist_get/3, glist_drop/2, glist_loop/0, glist_loop/3]).

%%%----------------------------------------------------------------------------

-define(VERSION, "0.1").
-define(LIBRARY, "Erlunit").
-define(COPYRIGHT, "(c) 2010 Eonblast Corporation - Open Source MIT License").

-define(PROMPT, "erlunit: ").
-define(INDENT, "         ").

-define(USE, "This is an error in the way you use erlunit, or an error in erlunit itself.").

-define(DEFAULT_SUITE_NAME, "Default Suite").

-define(V1, false).  			% verbosity settings
% not used: -define(V2, false). % higler level enabled
% not used: -define(V3, false). % means more details.

-define(D3, false).  % Debug verbosity:
-define(D4, false).  % Numbers mean different areas, not levels.
-define(D5, false).  % They are called out as hint in error messages.

-define(LINES, true). % governs display of lines in output.


%%%****************************************************************************
%%% TESTS
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% true - check if an expression returns true
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL
%%%
%%% For true/0 see (*). That is used for a different purposed.

true(A) -> true(A, "Check for true").

true(A, Message) -> true(suite, A, Message).

true(Suite, A, Message) ->

    if A ->
			passed(Suite, Message, "evaluates to ~p", [A]);
       true ->
			failed(Suite, Message, "evaluates to ~p, should be true but is not", [A])
       end.

%%%
%%%----------------------------------------------------------------------------
%%% not_true - in Erlang, this is not necessarily 'false'
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

not_true(A) -> not_true(A, "Check for not true").

not_true(A, Message) -> not_true(suite, A, Message).

not_true(Suite, A, Message) ->

    if not A ->
			passed(Suite, Message, "evaluates to ~p, not true, as it should", [A]);
       true ->
			failed(Suite, Message, "evaluates to ~p, but should not be true", [A])
       end.
       
%%%
%%%----------------------------------------------------------------------------
%%% false
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

false(A) -> false(A, "Check for false").

false(A, Message) -> false(suite, A, Message).

false(Suite, A, Message) ->

    if A == false ->
			passed(Suite, Message, "evaluates to ~p", [A]);
       true ->
			failed(Suite, Message, "evaluates to ~p, should be false but is not", [A])
       end.

%%%
%%%----------------------------------------------------------------------------
%%% not_false - in Erlang, this is not necessarily 'true'
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

not_false(A) -> not_false(A, "Check for not false").

not_false(A, Message) -> not_false(suite, A, Message).

not_false(Suite, A, Message) ->

    if A /= false ->
			passed(Suite, Message, "evaluates to ~p, not false, as it should", [A]);
       true ->
			failed(Suite, Message, "evaluates to ~p, but should not be false", [A])
       end.

%%%
%%%----------------------------------------------------------------------------
%%% pass - ok means: Fun throws NO exception
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

pass(Fun) -> pass(Fun, "Check for passing without exception").

pass(Fun, Message) -> pass(suite, Fun, Message).

pass(Suite, Fun, Message) ->

    try 
    	Fun(),
    	passed(Suite, Message, "passes ok")
	catch
    	throw:_Term -> 
    		failed(Suite, Message, "throws exception but should pass ok");
    	exit:_Reason -> 
    		failed(Suite, Message, "make exit but should pass ok");
    	error:_Reason -> 
    		failed(Suite, Message, "runs into error but should pass ok")
	end.

%%%
%%%----------------------------------------------------------------------------
%%% fail - ok when Fun throws an exception
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

fail(Fun) -> fail(Fun, "Check for exception").

fail(Fun, Message) -> fail(suite, Fun, Message).

fail(Suite, Fun, Message) ->

    try 
    	Fun(),
    	failed(Suite, Message, "passes ok but should fail")
	catch
    	throw:_Term -> 
    		passed(Suite, Message, "throws exception, so fails as it should");
    	exit:_Reason -> 
    		passed(Suite, Message, "makes exit, so fails as it should");
    	error:_Reason -> 
    		passed(Suite, Message, "runs into error, so fails as it should")
	end.

%%%
%%%----------------------------------------------------------------------------
%%% throws - ok when Fun throws an exception
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

throws(Fun) -> throws(Fun, "Check for throw").

throws(Fun, Message) -> throws(suite, Fun, Message).

throws(Suite, Fun, Message) ->

    try 
    	Fun(),
    	failed(Suite, Message, "passes ok but should throw and fail")
	catch
    	throw:_Term -> 
    		passed(Suite, Message, "throws exception, as it should");
    	exit:_Reason -> 
    		failed(Suite, Message, "makes exit, but should throw and and fail");
    	error:_Reason -> 
    		failed(Suite, Message, "runs into error, but should throw and fail")
	end.
%%%
%%%----------------------------------------------------------------------------
%%% exits - ok when Fun calls exit
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

exits(Fun) -> exits(Fun, "Check for exit to be called").

exits(Fun, Message) -> exits(suite, Fun, Message).

exits(Suite, Fun, Message) ->

    try 
    	Fun(),
    	failed(Suite, Message, "passes ok but should throw and fail")
	catch
    	throw:_Term -> 
    		failed(Suite, Message, "throws exception, as it should");
    	exit:_Reason -> 
    		passed(Suite, Message, "makes exit, but should throw and and fail");
    	error:_Reason -> 
    		failed(Suite, Message, "runs into error, but should throw and fail")
	end.
%%%
%%%----------------------------------------------------------------------------
%%% error - ok when Fun runs into error
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

error(Fun) -> error(Fun, "Check for error to take place").

error(Fun, Message) -> error(suite, Fun, Message).

error(Suite, Fun, Message) ->

    try 
    	Fun(),
    	failed(Suite, Message, "passes ok but should run into error")
	catch
    	throw:_Term -> 
    		failed(Suite, Message, "throws exception, but should run into error");
    	exit:_Reason -> 
    		failed(Suite, Message, "makes exit, but should run into error");
    	error:_Reason -> 
    		passed(Suite, Message, "runs into error, as it should")
	end.
%%%
%%%----------------------------------------------------------------------------
%%% equal
%%%----------------------------------------------------------------------------

%%%

equal(msg) -> "Equality check".

equal(A, B) -> equal(A, B, equal(msg)).

equal(A, B, Message) -> equal(whereis(suite), A, B, Message).

equal(Suite, A, B, Message) ->

    if 
         A == B ->
			passed(Suite, Message, "~p == ~p as it should", [A, B]);
         true ->
         	failed(Suite, Message, "~p /= ~p but should be equal", [A, B])
    end.
%%%
%%%----------------------------------------------------------------------------
%%% not_equal
%%%----------------------------------------------------------------------------


not_equal(msg) -> "Non-equality check".

not_equal(A, B) -> not_equal(A, B, not_equal(msg)).

not_equal(A, B, Message) -> not_equal(whereis(suite), A, B, Message).

not_equal(Suite, A, B, Message) ->

    if 
         A /= B ->
			passed(Suite, Message, "~p /= ~p as it should", [A, B]);
         true ->
         	failed(Suite, Message, "~p == ~p but should NOT be equal", [A, B])
       end.

%%%
%%%----------------------------------------------------------------------------
%%% bigger
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

bigger(A, B) -> bigger(A, B, "Check for bigger").

bigger(A, B, Message) -> bigger(suite, A, B, Message).

bigger(Suite, A, B, Message) ->

    if 
         A > B ->
			passed(Suite, Message, "~p > ~p as it should", [A, B]);
         true ->
         	failed(Suite, Message, "~p <= ~p but should be bigger", [A, B])
       end.

%%%
%%%----------------------------------------------------------------------------
%%% lesser
%%%----------------------------------------------------------------------------

%%%
%%% NOT YET OPERATIONAL

lesser(A, B) -> lesser(A, B, "Check for lesser").

lesser(A, B, Message) -> lesser(suite, A, B, Message).

lesser(Suite, A, B, Message) ->

    if 
         A < B ->
			passed(Suite, Message, "~p < ~p as it should", [A, B]);
         true ->
         	failed(Suite, Message, "~p >= ~p but should be lesser", [A, B])
       end.

%%%****************************************************************************
%%% COUNTER CALLS
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% passed - echo positive result and count it
%%%----------------------------------------------------------------------------


passed(Suite, Message, Result) -> passed(Suite, Message, Result, []).

passed(Suite, Message, Result, ResultParameter) ->

		SuiteName = glist_get(suitenames, Suite, ""),
		io:format(?PROMPT ++ "+ ok | ~s~s ~s | " ++ Result ++ ".~n",
			[SuiteName, iff(SuiteName,":",""), Message | ResultParameter]),
		Suite ! passed.


%%%----------------------------------------------------------------------------
%%% failed - echo negative result and count it
%%%----------------------------------------------------------------------------


failed(Suite, Message, Result) -> failed(Suite, Message, Result, []).

failed(Suite, Message, Result, ResultParameter) ->

		SuiteName = glist_get(suitenames, Suite, ""),
		io:format(?PROMPT ++ "- fails | ##### ~s~s ~s | " ++ Result ++ ". #####~n",
			[SuiteName, iff(SuiteName,":",""), Message | ResultParameter]),
		Suite ! failed.


%%%****************************************************************************
%%% MAIN TEST
%%%****************************************************************************
%%%
%%% You can have multiple Main Tests, that are collections of suites
%%% but they should probably be executed in sequence, not in parallel.
%%%
%%%----------------------------------------------------------------------------
%%% Start
%%%----------------------------------------------------------------------------


start() ->

	banner(),

	echo("Start of Tests."),
	register(main, self()),

	Stats = spawn(fun() -> stats("Overall") end),
	register(stats, Stats),

	glist(suitenames), % mind you, before first suite() call.
	
	DefaultSuite = spawn(fun() -> suite(?DEFAULT_SUITE_NAME, self()) end),
	register(suite, DefaultSuite),
	self() ! { add, DefaultSuite, ?DEFAULT_SUITE_NAME },
		
	main.

%%%----------------------------------------------------------------------------
%%% execute - work the list of queued checks
%%%----------------------------------------------------------------------------


execute() -> 
	
	execute_loop(start, [], []).

%%% non-exported: -------------------------------------------------------------

execute_loop(Phase, SuitesActive, SuitesToPrint) ->

	vecho(?D3, "Main process phase: ~p | active suites: ~p | to print: ~p", [Phase, SuitesActive, SuitesToPrint]),

	receive
	Rcv ->
	vecho(?D3, "Main process receives: ~p", [Rcv]),
	
	case Rcv of
		{ add, Suite, Name } -> 
			if 
				Phase == start ->
					Suite ! done,
					execute_loop(Phase,  [ Suite | SuitesActive ],  lists:append(SuitesToPrint, [Suite])  );

				% protection vs misuse
				true ->
					exit("Too late to add ~s. " ++ ?USE ++ " Try D3.", [Name])
			end;

		{ done, Suite } -> 
			vecho(?D3, "~s done.", [glist_get(suitenames, Suite, "*")]),
			self() ! check_done,
			execute_loop(Phase,  lists:delete(Suite, SuitesActive), SuitesToPrint );

		check_done -> 
			% print_suites must be sent only once. Problem: check_done can come
			% in multiple times with empty SuitesActive list, depending on Suite message sequences.
			% Therefore, the if, and the phase forced to 'print'.
			if
				(Phase /= print) and (SuitesActive == []) -> 
					self() ! print_suites,
					NewPhase = print; % must be here, not later.
				true ->
					NewPhase = Phase
			end,
			execute_loop(NewPhase, SuitesActive, SuitesToPrint);
		
		print_suites ->
			% There is always at least one suite, at least the default suite.
			% Print one and wait for ack in the form of { printed, Suite }, below.
			[ Suite | LeftToPrint ] = SuitesToPrint,
			Suite ! print,
			execute_loop(Phase, SuitesActive, LeftToPrint);
			
		{ printed, _Suite } ->
		    if 
				SuitesToPrint == [] -> self() ! print_summary; 
				true -> self() ! print_suites
			end,
			execute_loop(Phase,  SuitesActive, SuitesToPrint );
			
		print_summary ->
			stats ! print,
			self() ! finis,
			execute_loop(finish, SuitesActive, SuitesToPrint);
			
		finis ->
			timer:sleep(200), % allow rogue processes to finish
			safe_unregister(suitenames),
			safe_unregister(suite),
			safe_unregister(stats),
			safe_unregister(main),
			complete;

		Any -> 
			vecho(?D3, "Handed on from main process to suite: ~p~n", [Any]),
			suite ! Any,
			execute_loop(Phase, SuitesActive, SuitesToPrint)
		end

		after 3000 ->
			exit("Main process stalled. " ++ ?USE ++ " Try 3D.")
	end.


%%%****************************************************************************
%%% SUITE
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% Create Suite Process
%%%----------------------------------------------------------------------------


%%% expects to be called by the main process -------------------------

suite(Nom) -> 

	vecho(?V1, "~s~nStarting Suite ~s.", [line(), Nom]),
	Suite = spawn(fun() -> suite("Suite " ++ Nom, self()) end),
	
	glist_add(suitenames, Suite, "Suite " ++ Nom), 
	main ! { add, Suite, Nom },

	% register default suite	
	Prev = whereis(suite),
	if Prev /= undefined -> unregister(suite); true -> nil end,
	register(suite, Suite),
	
	Suite.
	
%%% non-exported: -------------------------------------------------------------
	
suite(Nom, Caller) ->

	suite_loop(Nom, Caller, nil, 0, 0, 0),
	exit(1000).

suite_loop(Nom, Caller, Sub, Passed, Failed, Crashed) ->
	
	receive

		%% counters
		%% -----------------------------------------------------------
		%% Note that these counter calls are processed way later than
		%% the check calls, because the latter are fielded, usually,
		%% in fast succession, thus are queued first in line, before
		%% even the first of them are executed, which then, in turn,
		%% generate the below counter calls
		
		passed -> 
			stats ! passed,
			suite_loop(Nom, Caller, Sub, Passed +1, Failed, Crashed);
		failed -> 
			stats ! failed,
			suite_loop(Nom, Caller, Sub, Passed, Failed +1, Crashed);
		crashed -> 
			stats ! crashed,
			suite_loop(Nom, Caller, Sub, Passed, Failed +1, Crashed +1);

		%% checks
		%% -----------------------------------------------------------
		{ equal, A, B } ->
			equal(self(), A, B, equal(msg)),
			suite_loop(Nom, Caller, Sub, Passed, Failed, Crashed);

		{ equal, A, B, Message } ->
			equal(self(), A, B, Message),
			suite_loop(Nom, Caller, Sub, Passed, Failed, Crashed);

		{ not_equal, A, B } ->
			not_equal(self(), A, B, not_equal(msg)),
			suite_loop(Nom, Caller, Sub, Passed, Failed, Crashed);

		{ not_equal, A, B, Message } ->
			not_equal(self(), A, B, Message),
			suite_loop(Nom, Caller, Sub, Passed, Failed, Crashed);

		%% screen
		%% -----------------------------------------------------------
		print -> 
			if
				% suppress printing the default suite when unused
%				(Nom == ?DEFAULT_SUITE_NAME) and (Passed+Failed+Crashed == 0) ->
%					nil;

				true ->
					if 
						Failed > 0 -> 
							{ Verdict, Line } = { "failed", line(bad) };
						true -> 
							{ Verdict, Line } = { "passed", line(good) }
					end,
		
					echo("~s~n~s ~s - Tests: ~p, Passed: ~p, Failed: ~p, Crashes: ~p~n~s",
						 [Line, Nom, Verdict, Passed+Failed, Passed, Failed, Crashed, Line])
			end,

			main ! { printed, self() },

			suite_loop(Nom, Caller, Sub, Passed, Failed, Crashed);
			
		%% control
		%% -----------------------------------------------------------
		stop -> 
			return;

		done -> 
			% self() ! print,
			main ! { done, self() },
			vecho(?V1, "~s~n~s done.", [line(), Nom]),
			suite_loop(Nom, Caller, Sub, Passed, Failed, Crashed);
			
		Malformed -> 
			echo("##### ~s can't deal with ~p. ~s #####", [Nom, Malformed, ?USE]),
			self() ! crashed,
			suite_loop(Nom, Caller, Sub, Passed, Failed, Crashed)
			
		after 1000 ->
			exit("Suite stalled. " ++ ?USE)
    end.

%%%****************************************************************************
%%% STATS HOLDER
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% Stats Process
%%%----------------------------------------------------------------------------


stats(Nom) ->

	stats_loop(Nom, 0, 0, 0).

stats_loop(Nom, Passed, Failed, Crashed) ->
	
	% io:format("~nEntering stats_loop: ~s ~p ~p ~p ~n", [Nom, Passed, Failed, Crashed]),
	
	receive
	
		% X -> io:format("Stats loop received ~p~n", [X]),
		%	stats_loop(Nom, Passed, Failed, Crashed);

		passed -> 
			stats_loop(Nom, Passed +1, Failed, Crashed);

		failed -> 
			stats_loop(Nom, Passed, Failed +1, Crashed);

		crashed -> 
			stats_loop(Nom, Passed, Failed +1, Crashed +1);

		print ->
			if
				Failed > 0 -> 
					{ Verdict, Line } = { "failed", line(bad) };
				true -> 
					{ Verdict, Line } = { "passed", line(good) }
			end,

			echo("~s~n~s ~s - Tests: ~p, Passed: ~p, Failed: ~p, Crashes: ~p~n~s",
				 [Line, Nom, Verdict, Passed+Failed, Passed, Failed, Crashed, Line]),

			stats_loop(Nom, Passed, Failed, Crashed)

    end.

%%%****************************************************************************
%%% GLOBAL LIST
%%%****************************************************************************
%%%
%%% Ack, ugh *** don't be me so hard! Oh ye Gods of FP, no, please ...
%%%
%%%----------------------------------------------------------------------------
%%% Create Global List
%%%----------------------------------------------------------------------------


glist(AtomName) ->

	GList = spawn(fun() -> glist_loop() end),
	register(AtomName, GList),
	GList.
	
%%%
%%%----------------------------------------------------------------------------
%%% Main Loop
%%%----------------------------------------------------------------------------


glist_loop() -> glist_loop([], nil, nil).

glist_loop(List, PrevCaller, Return) ->

	% call back to originally caller, to deliver result.
	if is_pid(PrevCaller) -> 
		vecho(?D4, "call ~p ~p", [PrevCaller, Return]), 
		PrevCaller ! Return; 
		true -> nil
	 end,

	receive
		{ add, Element, Caller } ->	glist_loop([Element | List], Caller, ok);
		{ drop, Key, Caller } -> glist_loop(lists:keydelete(Key,1,List), Caller, ok);
		{ get, Key, Caller } -> glist_loop(List, Caller, lists:keyfind(Key,1,List));
		E -> throw(E)
	end.

%%%
%%%----------------------------------------------------------------------------
%%% Create a Global List
%%%----------------------------------------------------------------------------


glist_add(GList, Key, Contents) ->
	
	vecho(?D4, "glist_add: GList ~p | Key ~p | Contents ~p", [GList, Key, Contents]),

	GList ! { add, { Key, Contents }, self() },
	receive ok -> ok after 3000 -> exit("Urgh. Programmed to death. glist_add() stalled. " ++ ?USE ++ " Try D4.") end.
	% this is forced sequentiality

%%%
%%%----------------------------------------------------------------------------
%%% Drop an element from the Global List, by Key
%%%----------------------------------------------------------------------------

	
glist_drop(GList, Key) ->
	
	GList ! { drop, Key, self() },
	receive ok -> ok after 3000 -> exit("Urgh. Programmed to death. glist_drop() stalled. " ++ ?USE ++ " Try D4.") end.
	% this is forced sequentiality

%%%
%%%----------------------------------------------------------------------------
%%% Get an element from the Global List, by Key
%%%----------------------------------------------------------------------------

%%%
%%% Key may be a Pid or an atom, to be interpreted as registered name.
	
glist_get(GList, Key) when is_atom(Key) -> glist_get(GList, whereis(Key), nil, 0);

glist_get(GList, Key) when is_pid(Key) -> glist_get(GList, Key, nil, 0).

glist_get(GList, Key, Default) when is_atom(Key) -> glist_get(GList, whereis(Key), Default, 0);

glist_get(GList, Key, Default) when is_pid(Key) -> glist_get(GList, Key, Default, 0).

%%% non-exported: -------------------------------------------------------------

glist_get(GList, Key, Default, Retry) -> 

	vecho(?D4, "glist_get: GList ~p | Key ~p | Retry ~p", [GList, Key, Retry]),

	case lists:member(GList, registered()) of
		true ->
			GList ! { get, Key, self() },
			receive 
				{ Key, Name } -> 
					Name;
				false ->
					% ok, short time out and retry for slow concurrent adds (?)
					case Retry of
						0 ->
							timer:sleep(100),
							glist_get(GList, Key, Default, Retry +1);
						_ ->
							Default 
					end
				after 3000 -> exit("Urgh. Programmed to death. glist_get() stalled. " ++ ?USE ++ " Try D4.") 
			end;
			% immediate receive forces sequentiality
		
		% not yet ready or never registered
		_ -> Default
	end.

%%%****************************************************************************
%%% UTILITY
%%%****************************************************************************
%%%
%%% Be especially wary not to take a cue from these.
%%%
%%%----------------------------------------------------------------------------
%%% safe_unregister - unregister w/o complaint if missing
%%%----------------------------------------------------------------------------


safe_unregister(Name) ->

	Registered = whereis(Name) /= undefined,

	if Registered -> unregister(Name); true -> nil end.
%%%
%%%----------------------------------------------------------------------------
%%% if-function
%%%----------------------------------------------------------------------------

%%%
%%% Returns 2nd parameter if 1st is true, not "" and not 0. Else 3rd.

iff(Cond, A, _) when is_list(Cond) and Cond /= "" -> A;

iff(Cond, A, _) when Cond == true -> A;

iff(Cond, A, _) when is_integer(Cond) and Cond /= 0 -> A;

iff(_, _, B) -> B.


%%%----------------------------------------------------------------------------
%%% true - vehicle to suppress warnings where definitions are used
%%%----------------------------------------------------------------------------

%%%
%%% To suppress warnings where definitions are used for clauses (**).

true() -> true. 

%%%****************************************************************************
%%% SCREEN UTILITY
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% Echo
%%%----------------------------------------------------------------------------

%%%
%%% A io:format() pretext that adds 'erlunit: ' left hand side.

echo(Message) -> echo(Message, []).

echo(Format, Para) -> 

	Format2 = re:replace(Format, "~n", "~n" ++ ?PROMPT, [global, {return,list}]),
	Format3 = ?PROMPT ++ Format2 ++ "~n",

	io:format(Format3, Para).

%%%
%%%----------------------------------------------------------------------------
%%% Verbose Echo
%%%----------------------------------------------------------------------------

%%%
%%% Echo only if the switch parameter is true. To be refactored.

vecho(Switch, Message) -> vecho(Switch, Message, []).

vecho(Switch, Format, Para) ->

	if Switch -> echo(Format, Para); true -> nil end.

%%%
%%%----------------------------------------------------------------------------
%%% Line
%%%----------------------------------------------------------------------------


line() -> line(default).

line(Type) ->

	PrintLines = ?LINES and true(), % see (**)
	if PrintLines ->
	
		if
			Type == good ->
				"-----------------------------------------------------------------";
			Type == bad ->
				"#################################################################";
			Type == strong ->
				"*****************************************************************";
		    true ->
				"................................................................."
		end;
	
		true -> ""
	
	end.
%%%
%%%----------------------------------------------------------------------------
%%% Banner
%%%----------------------------------------------------------------------------

%%%
%%% This illegible ASCII art serves the purpose of quick orientation 
%%% when scrolling up and down the terminal window. And I like it, too.
%%%

banner() -> banner("").

banner(Message) ->
    io:format("------------------------------------------------------------------------o--~n"),
    io:format("---ooo-ooo--o--o--o-o--o-o-ooo------------------------------------------o--~n"),
    io:format("---Oo--OoO--O--O--O-Oo-O-O--O-------------------------------------------o--~n"),
    io:format("---Ooo-O--O-Ooo-OO--O-oO-O--O-------------------------------------------o--~n"),
    io:format("------------------------------------------------------------------------o--~n"),
    io:format("~s ~s   ~s ~s~n",[?LIBRARY, ?VERSION, ?COPYRIGHT, Message]),
    io:format("------------------------------------------------------------------------o--~n"),
    ok.
    
    
%%%****************************************************************************
%%% DEBUG UTILITY
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% alive - dump if a registered process is still registered
%%%----------------------------------------------------------------------------


alive(Name) ->
	Is = lists:member(Name, registered()),
	if Is -> Say = "is alive", Paint = "++++++"; true -> Say = "is gone", Paint = "XXXXX" end,
	io:format("~s~s ~s ~s ~s~n", [?INDENT, Paint, Name, Say, Paint]).
	

%%%                                   %0> 
%%%                                   (^)
%%% Easter egg                        ###                           End of art