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
%%% @author Edwin Fine <efine@silentcircle.com>
%%% @copyright 2015 Silent Circle
%%% @doc Test suite for the 'sc_util_srv' module.
%%% @end
%%% ==========================================================================

-module(sc_util_srv_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("sc_util_srv.hrl").

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [
        {timetrap, {seconds, 30}}
    ].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_Case, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() ->
    [
        {
            srv,
            [],
            [
                pick_server_test,
                fetch_srv_rrs_test
            ]
        }
    ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
    [
        {group, srv}
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

% t_1(doc) -> ["t/1 should return 0 on an empty list"];
% t_1(suite) -> [];
% t_1(Config) when is_list(Config)  ->
%     ?line 0 = t:foo([]),
%     ok.

%%--------------------------------------------------------------------
%% srv group
%%--------------------------------------------------------------------
pick_server_test() ->
    [].

pick_server_test(doc) ->
    [
        "sc_util_srv:pick_server/1 should select a host correctly based"
        "on SRV record priority and weight"
    ];
pick_server_test(suite) ->
    [];
pick_server_test(_Config) ->
    ?assertEqual(sc_util_srv:pick_server(zero_srv_recs()), undefined),

    SrvInfo1 = to_srv_info(hd(one_srv_rec()), []),
    ?assertEqual(sc_util_srv:pick_server(one_srv_rec()), SrvInfo1),

    AllRecs = all_srv_recs(),
    ?assert(length(AllRecs) /= 0), % Not really a test

    % Go through all the recs and make sure Rest loses an element
    % each time until undefined terminates the fold.
    Rest = lists:foldl(
        fun(_, Acc) ->
                {_H, _P, _T, NewRest} = sc_util_srv:pick_server(Acc),
                ?assertEqual(length(NewRest), length(Acc) - 1),
                NewRest
        end,
        AllRecs, AllRecs
    ),

    ?assertEqual(Rest, []),
    ?assertEqual(sc_util_srv:pick_server(Rest), undefined),

    ok.

%% Note: This test uses mocks so that we don't actually have to go out to DNS
fetch_srv_rrs_test(_Config) ->
    RecordSet = all_srv_recs(),
    mock_inet(RecordSet),
    ServiceName = "_some-client._tcp.some.domain",
    {ok, RRs} = sc_util_srv:fetch_srv_rrs(ServiceName),
    {ok, RRs} = sc_util_srv:fetch_srv_rrs(list_to_binary(ServiceName)),
    ?assertEqual(RRs, RecordSet),
    unmock_inet(),

    %% Test missing SRV records
    mock_inet([]),
    NoRRsResult = sc_util_srv:fetch_srv_rrs(ServiceName),
    NoRRsResult = {error, {no_srv_records_for, ServiceName}},
    unmock_inet(),

    %% Test DNS error
    ErrReason = test_inet_res_error,
    mock_inet(RecordSet, ErrReason),
    ErrResult = {error, {dns_srv_resolution_error, {ErrReason, ServiceName}}},
    ErrResult = sc_util_srv:fetch_srv_rrs(ServiceName),
    unmock_inet(),
    ok.

%%====================================================================
%% Internal helper functions
%%====================================================================
to_rr({Prio, Weight, Port, Host}) ->
    to_rr(Prio, Weight, Port, Host, 86400).

to_rr(Prio, Weight, Port, Host, TTL) ->
    #rr{prio = Prio,
        weight = Weight,
        port = Port,
        host = Host,
        ttl = TTL}.

fm_rr(#rr{prio = Pr, weight = W, port = P, host = H}) ->
    {Pr, W, P, H}.

to_rrs(Tuples) ->
    [to_rr(T) || T <- Tuples].

fm_rrs(RRs) ->
    [fm_rr(RR) || RR <- RRs].

to_srv_info(#rr{prio = _Pr, weight = _W, port = P, host = H, ttl = T}, Rest) ->
    {H, P, T, Rest}.

zero_srv_recs() ->
    [].

one_srv_rec() ->
    [to_rr(hd(rec_set()))].

all_srv_recs() ->
    [to_rr(R) || R <- rec_set()].

rec_set() ->
    [
        {10,60,5060,"bigbox.example.com"},
        {10,20,5060,"smallbox1.example.com"},
        {10,10,5060,"smallbox2.example.com"},
        {10,10,5066,"smallbox2.example.com"},
        {20,0,5060,"backupbox.example.com"}
    ].

hostname_set(Prio, Recs) ->
    [H || {P, _, _, H} <- Recs, P =:= Prio].

mock_inet(RecordSet) ->
    mock_inet(RecordSet, undefined).

mock_inet(RecordSet, InetResError) ->
    meck:new(inet_res, [unstick, non_strict]),
    meck:new(inet_dns, [unstick, non_strict]),
    meck:expect(inet_res, resolve,
                fun(_Name, _Class, _Type, _Opts, _Timeout) ->
                        case InetResError of
                            undefined ->
                                {ok, {fake_dns_rec, RecordSet}};
                            Reason ->
                                {error, Reason}
                        end
                end),
    meck:expect(inet_dns, record_type,
                fun({fake_dns_rec, _}) -> fake_dns_rec;
                    (#rr{}) -> fake_dns_rr
                end),
    meck:expect(inet_dns, fake_dns_rec,
                fun({fake_dns_rec, RRs}) ->
                        [{anlist, RRs}]
                end),
    meck:expect(inet_dns, fake_dns_rr,
                fun(#rr{} = RR) ->
                        [
                            {data, fm_rr(RR)},
                            {ttl, RR#rr.ttl}
                        ]
                end),
    ok.

unmock_inet() ->
    meck:unload(inet_res),
    meck:unload(inet_dns),
    ok.

