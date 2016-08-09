%%% ==========================================================================
%%% Copyright 2015 Silent Circle
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% ==========================================================================


%%% ==========================================================================
%%% @author Sebastien Merle
%%% @author Edwin Fine <efine@silentcircle.com>
%%% @copyright 2015 Silent Circle
%%% @doc sc_util_app unit testing.
%%% @end
%%% @private
%%% ==========================================================================

-module(sc_util_app_SUITE).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Test cases
-export([
         merge_config/1,
         start_applications/1,
         get_app_info/1
        ]).

%%% Common test callbacks
-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

-define(assertMerge(Exp, Conf, Over),
        ?assertMatch(Exp, sc_util_app:merge_config(Conf, Over))).


%%% ==========================================================================
%%% Common Test Callbacks
%%% ==========================================================================

suite() ->
    [{timetrap, {seconds, 30}}].


all() ->
    [merge_config,
     start_applications,
     get_app_info].


init_per_suite(Config) ->
    Config.


end_per_suite(_Config) ->
    ok.


init_per_testcase(start_applications, Config) ->
    [{app_mgr, mock_app()} |Config];

init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(start_applications, Config) ->
    cleanup_app(?config(app_mgr, Config)),
    ok;

end_per_testcase(_TestCase, _Config) ->
    ok.



%%% ==========================================================================
%%% Test Case Functions
%%% ==========================================================================

merge_config(_Config) ->
    ?assertMerge([], [], []),

    ?assertMerge([{foo, 1}], [{foo, 1}], []),
    ?assertMerge([{foo, 1}, {bar, 2}], [{foo, 1}, {bar, 2}], []),
    ?assertMerge([{foo, [{spam, 1}]}, {bar, 2}],
                 [{foo, [{spam, 1}]}, {bar, 2}], []),

    ?assertMerge([{foo, 1}], [], [{foo, 1}]),
    ?assertMerge([{foo, 1}, {bar, 2}], [], [{foo, 1}, {bar, 2}]),
    ?assertMerge([{bar, 2}, {foo, [{spam, 1}]}],
                 [], [{bar, 2}, {foo, [{spam, 1}]}]),

    ?assertMerge([{foo, 2}], [{foo, 1}], [{foo, 2}]),
    ?assertMerge([{foo, 1}, {bar, 2}], [{foo, 1}], [{bar, 2}]),
    ?assertMerge([{bar, 2}, {foo, 1}], [{bar, 2}], [{foo, 1}]),
    ?assertMerge([{foo, 2}, {bar, 2}], [{foo, 1}, {bar, 2}], [{foo, 2}]),
    ?assertMerge([{bar, 2}, {foo, [{spam, 1}, {bacon, 3}]}],
                 [{bar, 2}, {foo, [{spam, 1}]}], [{foo, [{bacon, 3}]}]),
    ?assertMerge([{bar, 2}, {foo, [{spam, 3}]}],
                 [{bar, 2}, {foo, [{spam, 1}]}], [{foo, [{spam, 3}]}]),
    ?assertMerge([{foo, [{spam, 3}]}, {bar, 2}],
                 [{foo, [{spam, 1}]}], [{bar, 2}, {foo, [{spam, 3}]}]),

    ?assertMerge([{foo, [{spam, 1}]}, {bar, 2}],
                 [{foo, [{spam, 1}]}, {bar, 2}], [{foo, []}]),
    ok.


start_applications(_Config) ->
    ?assertMatch({ok, []}, sc_util_app:start_applications([])),
    ?assertMatch({ok, [{foo, 1}]},
                 sc_util_app:start_applications([{{foo, 1}, []}])),
    ?assertMatch({ok, [{foo, 2}, {foo, 3}, {foo, 4}]},
                 sc_util_app:start_applications([{{foo, 4}, []}])),

    application:load({bar, 2}),
    ?assertMatch({ok, [{bar, 1}, {bar, 2}, {bar, 3}]},
                 sc_util_app:start_applications([{{bar, 3}, []}])),

    ?assertMatch({ok, [{bar, 4}, {bar, 5}]},
                 sc_util_app:start_applications([{{bar, 5}, []},
                                                 {{bar, 3}, []},
                                                 {{bar, 4}, []}],
                                                [{logger, fun test_logger/3}])),

    ?assertMatch({error, _},
                 sc_util_app:start_applications([{{buz, 2}, []},
                                                 {dummy, []}])),

    ?assertMatch({ok, [{spam, 1}]},
                 sc_util_app:start_applications([{{spam, 1},
                                                  [{key1, val1},
                                                   {key2, val2}]}])),
    ?assertMatch({ok, val1}, application:get_env({spam, 1}, key1)),
    ?assertMatch({ok, val2}, application:get_env({spam, 1}, key2)),
    ?assertMatch(undefined, application:get_env({spam, 1}, key3)),

    ok.

get_app_info(Config) ->
    AppInfo = sc_util_app:get_app_info(),
    States = ['loading', 'loaded', 'starting',
              'started', 'start_p_false', 'running'],
    [begin
         true = is_tuple(Tuple),
         true = (tuple_size(Tuple) == 2),
         true = lists:member(element(1, Tuple), States),
         true = is_list(element(2, Tuple))
     end || Tuple <- AppInfo],
    Config.

%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

test_logger(Level, Format, Args) ->
    ct:log("~s: " ++ Format, [Level |Args]).


mock_app() ->
    Self = self(),
    AppMgr = spawn_link(fun() -> app_manager_proc() end),
    meck:new(application, [unstick]),
    meck:expect(application, info,
                fun() ->
                        Ref = make_ref(),
                        AppMgr ! {info, Self, Ref},
                        receive {Ref, Result} -> Result
                        after 1000 -> error("application:info timeout")
                        end
                end),
    meck:expect(application, set_env,
                fun(App, Key , Value) ->
                        Ref = make_ref(),
                        AppMgr ! {set_env, Self, Ref, App, Key, Value},
                        receive {Ref, Result} -> Result
                        after 1000 -> error("application:set_env timeout")
                        end
                end),
    meck:expect(application, load,
                fun(App) ->
                        Ref = make_ref(),
                        AppMgr ! {load, Self, Ref, App},
                        receive {Ref, Result} -> Result
                        after 1000 -> error("application:load timeout")
                        end
                end),
    meck:expect(application, start,
                fun(App) ->
                        Ref = make_ref(),
                        AppMgr ! {start, Self, Ref, App},
                        receive {Ref, Result} -> Result
                        after 1000 -> error("application:start timeout")
                        end
                end),
    meck:expect(application, get_env,
                fun(App, Key) ->
                        Ref = make_ref(),
                        AppMgr ! {get_env, Self, Ref, App, Key},
                        receive {Ref, Result} -> Result
                        after 1000 -> error("application:get_env timeout")
                        end
                end),
    AppMgr.


cleanup_app(AppMgr) ->
    meck:unload(application),
    unlink(AppMgr),
    MonRef = monitor(process, AppMgr),
    AppMgr ! stop,
    receive {'DOWN', MonRef, process, _, _} -> ok end.


app_manager_proc() ->
    app_manager_loop([], [], []).


app_manager_loop(Loaded, Started, Env) ->
    receive
        stop -> ok;
        {info, Pid, Ref} ->
            Result = [{started, [{App, temporary} || App <- Started]}],
            ct:pal("application:info() -> ~p~nLoaded: ~p~nStarted: ~p~nEnv: ~p~n",
                   [Result, Loaded, Started, Env]),
            Pid ! {Ref, Result},
            app_manager_loop(Loaded, Started, Env);
        {set_env, Pid, Ref, App, Key, Value} ->
            NewEnv = app_manager_set_env(App, Key, Value, Env),
            ct:pal("application:set_env(~p, ~p, ~p) -> ok~n"
                   "Loaded: ~p~nStarted: ~p~nEnv: ~p~n",
                   [App, Key, Value, Loaded, Started, NewEnv]),
            Pid ! {Ref, ok},
            app_manager_loop(Loaded, Started, NewEnv);
        {load, Pid, Ref, App} ->
            {NewLoaded, Result} = app_manager_load(App, Loaded),
            ct:pal("application:load(~p) -> ~p~n"
                   "Loaded: ~p~nStarted: ~p~nEnv: ~p~n",
                   [App, Result, NewLoaded, Started, Env]),
            Pid ! {Ref, Result},
            app_manager_loop(NewLoaded, Started, Env);
        {start, Pid, Ref, App} ->
            {NewStarted, Result} = app_manager_start(App, Loaded, Started),
            ct:pal("application:start(~p) -> ~p~n"
                   "Loaded: ~p~nStarted: ~p~nEnv: ~p~n",
                   [App, Result, Loaded, NewStarted, Env]),
            Pid ! {Ref, Result},
            app_manager_loop(Loaded, NewStarted, Env);
        {get_env, Pid, Ref, App, Key} ->
            Result = app_manager_get_env(App, Key, Env),
            Pid ! {Ref, Result},
            app_manager_loop(Loaded, Started, Env)
    end.


app_manager_set_env(App, Key, Value, Env) ->
    case lists:keytake(App, 1, Env) of
        false -> [{App, [{Key, Value}]} |Env];
        {value, {App, Props}, Env2} ->
            case lists:keytake(Key, 1, Props) of
                false -> [{App, [{Key, Value} |Props]} |Env2];
                {value, {Key, _Old}, Props2} ->
                    [{App, [{Key, Value} |Props2]} |Env2]
            end
    end.


app_manager_load({_, _} = App, Loaded) ->
    case lists:member(App, Loaded) of
        true -> {Loaded, {error, {already_loaded, App}}};
        false -> {[App |Loaded], ok}
    end;

app_manager_load(App, Loaded) ->
    {Loaded, {error, {"app not found", App}}}.


app_manager_start({_Name, 1} = App, Loaded, Started) ->
    case lists:member(App, Loaded) of
        false -> {Started, {error, {"not loaded", App}}};
        true ->
            case lists:member(App, Started) of
                true -> {Started, {error, {already_started, App}}};
                false -> {[App |Started], ok}
            end
    end;

app_manager_start({Name, Idx} = App, Loaded, Started) ->
    case lists:member(App, Loaded) of
        false -> {Started, {error, {"not loaded", App}}};
        true ->
            case lists:member(App, Started) of
                true -> {Started, {error, {already_started, App}}};
                false ->
                    case lists:member({Name, Idx - 1}, Started) of
                        true -> {[App |Started], ok};
                        false -> {Started, {error, {not_started, {Name, Idx - 1}}}}
                    end
            end
    end.


app_manager_get_env(App, Key, Env) ->
    case proplists:get_value(App, Env, undefined) of
        undefined -> undefined;
        AppEnv ->
            case proplists:get_value(Key, AppEnv, undefined) of
                undefined -> undefined;
                Value -> {ok, Value}
            end
    end.


trace() ->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(sc_util_app, x),
    ok.
