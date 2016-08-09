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
%%% @doc Utility functions to manage applications and their configurations.
%%% @end
%%% ==========================================================================

-module(sc_util_app).

%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([merge_config/2,
         get_app_info/0,
         start_applications/1,
         start_applications/2]).


-type app_name()       :: atom().
-type app_state()      :: 'loading' | 'loaded' | 'starting' | 'started' |
                          'start_p_false' | 'running'.
-type app_info()       :: {Application :: atom(),
                           Description :: string(),
                           Version :: string()}.
-type app_info_list()  :: [app_info()].
-type app_state_info() :: {app_state(), app_info_list()}.
-type app_states()     :: [app_state_info()].

%%% ==========================================================================
%%% API Functions
%%% ==========================================================================


%% -----------------------------------------------------------------
%% @doc Merge a list of application configurations and a list of overrides.
%% Guarantee the order of the keys; the keys from the configuration will be
%% first and in the same order they were specified, then the keys from
%% the override in the same order they were specified.
%
%% Note that this function is not very efficient and should not be used
%% intensively.
%
%% Configuration and override are proplists, e.g:
%%
%% ```
%% Config = [{app1, [{foo, 1}, {bar, [{spam, 2}]}, {buz, 3}]}],
%% Override = [{app1, [{foo, a}, {biz, b}, {bar, [{spam, c}, {eggs, d}]}]}],
%% sc_util_app:merge_config(Config, Override).
%% -> [{app1, [{foo, a}, {bar, [{spam, c}, {eggs, d}]}, {buz, 3}, {biz, b}]}]
%% '''
%% @end
%% -----------------------------------------------------------------
-spec merge_config(Config, Overrides) -> Config
    when Config :: proplists:proplist(), Overrides :: proplists:proplist().

merge_config([{A1, _}|_] = Config, []) when is_atom(A1) ->
    Config;

merge_config(Config, undefined) ->
    Config;

merge_config(undefined, Override) ->
    Override;

merge_config([{A1, _}|_] = Config, [{A2, _}|_] = Overrides)
  when is_atom(A1), is_atom(A2) ->
    [{K, merge_config(proplists:get_value(K, Config, undefined),
                      proplists:get_value(K, Overrides, undefined))}
     || K <- unique_keys(Config, Overrides)];

merge_config(_, Override) ->
    Override.


%% -----------------------------------------------------------------
%% @doc Starts a list of application. Same as start_applications(Apps, []).
%% @end
%% -----------------------------------------------------------------

-spec start_applications(Apps) ->
          {ok, StartedApps} | {error, Reason}
    when Apps :: proplists:proplist(),
         StartedApps :: [atom()],
         Reason :: term().

start_applications(Apps) ->
    start_applications(Apps, []).


%% -----------------------------------------------------------------
%% @doc Start a list of applications, using specified options.
%% The applications are specified as a proplist like
%% ```
%% [{app1_name, app1_config}, {app2_name, app2_config}]
%% '''
%% like in sys.config.
%%
%% <ul>
%%   <li>All the dependencies will be started in the proper order.</li>
%%   <li>If a dependency is not specified it will be started without setting
%%   any environment for it.</li>
%%   <li>If a specified application is already started, it will not be started
%%   again and its environment will not be changed; it will only log a warning.
%%   </li>
%% </ul>
%% @end
%% -----------------------------------------------------------------

-spec start_applications(Apps, Options) -> Result
    when Apps :: proplists:proplist(),
         Options :: [Option],
         Option :: {logger, Logger},
         Logger :: fun((Level, Format, Args) -> ok),
         Level :: info | warning,
         Format :: nonempty_string(),
         Args :: [term()],
         Result :: {ok, StartedApps} | {error, Reason},
         StartedApps :: [atom()],
         Reason :: term().

start_applications(Apps, Options) ->
    Started = [A || {A, _} <- proplists:get_value(started, get_app_info())],
    start_applications(Apps, Started, [], Options).

%% -----------------------------------------------------------------
%% @doc Return states of all applications.
%% @end
%% -----------------------------------------------------------------
-spec get_app_info() -> app_states().
get_app_info() ->
    application:info(). %% Undocumented API

%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

%% Keeps the key from the first proplist in there original order.
%% The new keys from the second list are appened at the end in order too.
-spec unique_keys(Prop1, Prop2) -> Result
    when Prop1 :: list(), Prop2 :: list(), Result :: list().
unique_keys(Prop1, Prop2) ->
    unique_keys(Prop1, Prop2, []).


-spec unique_keys(Prop1, Prop2, Acc) -> Result
    when Prop1 :: proplists:proplist(), Prop2 :: proplists:proplist(),
         Acc :: list(), Result :: list().
unique_keys([], [], Acc) ->
    lists:reverse(Acc);
unique_keys([], [{K, _} | Props2], Acc) ->
    unique_keys([], delete_all(K, Props2), [K | Acc]);
unique_keys([{K, _} | Props1], Props2, Acc) ->
    unique_keys(delete_all(K, Props1), delete_all(K, Props2), [K | Acc]).


-spec delete_all(Key, List) -> Result
    when Key :: any(), List :: proplists:proplist(),
         Result :: proplists:proplist().
delete_all(Key, List) ->
    [{K, V} || {K, V} <- List, K =/= Key].


-spec start_applications([{AppName, AppConfig} | Apps], Started, Acc, Options) -> Result
    when AppName :: app_name(), AppConfig :: list(), Apps :: [app_name()],
         Started :: [app_name()], Acc :: [app_name()],
         Options :: proplists:proplist(),
         Result :: {ok, [app_name()]} | {error, Reason}, Reason :: any().
start_applications([{AppName, AppConfig} | Apps], Started, Acc, Options) ->
    case lists:member(AppName, Started) of
        true ->
            log(Options, warning, "Application ~p already started, "
                "cannot change environment to:~n~p~n", [AppName, AppConfig]),
            start_applications(Apps, Started, Acc, Options);
        false ->
            start_applications(AppName, AppConfig, Apps, Started, Acc, Options)
    end;
start_applications([], _Started, Acc, _Options) ->
    {ok, lists:reverse(Acc)}.


-spec start_applications(AppName, AppConfig, Apps, Started, Acc,
                         Options) -> Result
    when AppName :: app_name(), AppConfig :: list(), Apps :: [app_name()],
         Started :: [app_name()], Acc :: [app_name()],
         Options :: proplists:proplist(),
         Result :: {'ok', [app_name()]} | {'error', _}.
start_applications(AppName, AppConfig, Apps, Started, Acc, Options) ->
    case start_application(AppName, AppConfig, Options) of
        ok ->
            start_applications(Apps, [AppName | Started], [AppName | Acc],
                               Options);
        {error, {not_started, NewAppName}} ->
            % Checks if the needed app has configuration.
            % Current app environment already set, no need to do it again later.
            NewApps = case lists:keytake(NewAppName, 1, Apps) of
                false ->
                    [{NewAppName, []}, {AppName, []} | Apps];
                {value, AppTuple, Apps2} ->
                    [AppTuple, {AppName, []} | Apps2]
            end,
            start_applications(NewApps, Started, Acc, Options);
        {error, _Reason} = Err -> Err
    end.

start_application(AppName, AppConfig, Options) ->
    case application:load(AppName) of
        {error, {already_loaded, AppName}} ->
            start_loaded_application(AppName, AppConfig, Options);
        ok ->
            start_loaded_application(AppName, AppConfig, Options);
        {error, _Reason} = Error -> Error
    end.


-spec start_loaded_application(atom(),[{atom(),_}],_) -> 'ok' | {'error',_}.
start_loaded_application(AppName, [], _Options) ->
    application:start(AppName);

start_loaded_application(AppName, AppConfig, Options) ->
    log(Options, info, "Setting application ~p environment: ~n~p~n",
        [AppName, AppConfig]),
    set_app_env(AppName, AppConfig),
    application:start(AppName).


-spec set_app_env(atom() | {'application',atom(),[{_,_}]},[{atom(),_}]) -> 'ok'.

set_app_env(_AppName, []) -> ok;

set_app_env(AppName, [{Key, Value} |AppConfig]) ->
    application:set_env(AppName, Key, Value),
    set_app_env(AppName, AppConfig).


-spec log(Options, Level, Format, Args) -> Result
    when Options :: list(), Level :: 'info' | 'warning',
         Format :: nonempty_string(), Args :: list(),
         Result :: any().

log(Options, Level, Format, Args) ->
    case proplists:get_value(logger, Options) of
        undefined -> ok;
        Logger -> Logger(Level, Format, Args)
    end.

%% ex: ts=4 sts=4 sw=4 et
