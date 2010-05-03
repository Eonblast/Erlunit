%%%----------------------------------------------------------------------------
%%% File        : erlunit.hrl
%%% Description : Test function convenience macros
%%% Version     : 0.2.5/alpha
%%% Status      : alpha
%%% Copyright   : (c) 2010 Eonblast Corporation http://www.eonblast.com
%%% License     : MIT - see below 
%%% Author      : H. Diedrich <hd2010@eonblast.com>
%%% Created     : 01 May 2010
%%% Changed     : 03 May 2010 - see CHANGES
%%% Tested on   : Erlang R13B01
%%%----------------------------------------------------------------------------
%%%
%%% Include this file in your test program to use macros.
%%%

-define(ERLUNIT_PASS_MSG(F, M), erlunit:pass(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_PASS(F), erlunit:pass(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_FAIL_MSG(F, M), erlunit:fail(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_FAIL(F), erlunit:fail(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_EQUAL_MSG(F, R, M), erlunit:equal(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_EQUAL(F, R), erlunit:equal(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_EXACT_MSG(F, R, M), erlunit:exact(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_EXACT(F, R), erlunit:exact(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).
