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
%%% @doc Test suite for the 'sc_util' module.
%%% @end
%%% ==========================================================================

-module(sc_util_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(assertMsg(Cond, Fmt, Args),
    case (Cond) of
        true ->
            ok;
        false ->
            ct:fail("Assertion failed: ~p~n" ++ Fmt, [??Cond] ++ Args)
    end
).

-define(assert(Cond), ?assertMsg((Cond), "", [])).

-define(assertThrow(Expr, Class, Reason),
    begin
            ok = (fun() ->
                    try (Expr) of
                        Res ->
                            {unexpected_return, Res}
                    catch
                        C:R ->
                            case {C, R} of
                                {Class, Reason} ->
                                    ok;
                                _ ->
                                    {unexpected_exception, {C, R}}
                            end
                    end
            end)()
    end
).

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
            util,
            [],
            [
                bitstring_to_hex_test,
                ensure_module_loaded_test,
                find_first_error_test,
                get_req_props_test,
                hex_to_bitstring_test,
                opt_val_test,
                posix_time_0_test,
                posix_time_1_test,
                make_child_spec_test,
                xdigit_test,
                nybble_test,
                req_binary_or_s_test,
                req_s_test,
                req_val_binary_or_s_test,
                req_val_test,
                req_val_s_test,
                to_atom_test,
                to_bin_test,
                to_list_test,
                exported_types_test
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
        {group, util}
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
%% util group
%%--------------------------------------------------------------------
hex_to_bitstring_test() ->
    [].

hex_to_bitstring_test(doc) ->
    [
        "sc_util:hex_to_bitstring/1 should correctly convert a hexadecimal string"
        "to a bitstring"
    ];
hex_to_bitstring_test(suite) ->
    [];
hex_to_bitstring_test(_Config) ->
    Hex = test_hex_str(),
    Expected = test_hex_bin(),
    Actual = sc_util:hex_to_bitstring(Hex),
    Expected = Actual,
    ok.

bitstring_to_hex_test() ->
    [].

bitstring_to_hex_test(doc) ->
    [
        "sc_util:bitstring_to_hex/1 should correctly convert a bitstring"
        "to a hexadecimal string"
    ];
bitstring_to_hex_test(suite) ->
    [];
bitstring_to_hex_test(_Config) ->
    Bin = test_hex_bin(),
    Expected = test_hex_str(),
    Actual = sc_util:bitstring_to_hex(Bin),
    Expected = Actual,
    ok.

ensure_module_loaded_test() ->
    [].

ensure_module_loaded_test(doc) ->
    [
        "sc_util:ensure_module_loaded/1 test"
    ];
ensure_module_loaded_test(suite) ->
    [];
ensure_module_loaded_test(Config) ->
    true = sc_util:ensure_module_loaded(sc_util),
    ?assertThrow(sc_util:ensure_module_loaded(nosuchmodule),
                 throw,
                 {cannot_load_module, {nosuchmodule, nofile}}),
    Config.


find_first_error_test() ->
    [].

find_first_error_test(doc) ->
    [
        "sc_util:find_first_error/1 test"
    ];
find_first_error_test(suite) ->
    [];
find_first_error_test(Config) ->
    [] = sc_util:find_first_error([]),
    [] = sc_util:find_first_error([a,b,c,d,e,{f,g}]),
    [{error, x}|_] = sc_util:find_first_error([a,b,c,error,d,{error,x},{error,y}]),
    Config.

get_req_props_test() ->
    [].

get_req_props_test(doc) ->
    [
        "sc_util:get_req_props/2 test"
    ];
get_req_props_test(suite) ->
    [];
get_req_props_test(Config) ->
    Props1 = [{rk1, rv1}, {k1, v1}, {rk2, rv2}, {k2, v2}],
    {[], []} = sc_util:get_req_props([], []),
    {[], [rk1]} = sc_util:get_req_props([rk1], []),
    {[], [rk1]} = sc_util:get_req_props([rk1], [rk1]), % Props must be 2-tuples
    {[{rk1, rv1}, {rk2, rv2}], []} = get_sorted_req_props([rk1, rk2], Props1),
    {[{rk1, rv1}, {rk2, rv2}], [rk3]} = get_sorted_req_props([rk1, rk2, rk3], Props1),
    Config.

opt_val_test() ->
    [].

opt_val_test(doc) ->
    [
        "sc_util:opt_val/3 test"
    ];
opt_val_test(suite) ->
    [];
opt_val_test(Config) ->
    Props = [{k1, v1}, {k2, v2}],
    v1 = sc_util:opt_val(k1, Props, foo),
    foo = sc_util:opt_val(k9, Props, foo),
    foo = sc_util:opt_val(k9, [], foo),
    true = sc_util:opt_val(k9, [k9], foo),
    Config.

posix_time_0_test() ->
    [].

posix_time_0_test(doc) ->
    [
        "sc_util:posix_time/0 test"
    ];
posix_time_0_test(suite) ->
    [];
posix_time_0_test(Config) ->
    TS = os:timestamp(),
    PTActual = sc_util:posix_time(),
    {M, S, U} = TS,
    PTCalc = (M * 1000000) + S + if U + 500000 >= 1000000 -> 1; true -> 0 end,
    PTCalc = PTActual, % on the basis that two consecutive timestamps should be in the same second
    Config.

posix_time_1_test() ->
    [].

posix_time_1_test(doc) ->
    [
        "sc_util:posix_time test"
    ];
posix_time_1_test(suite) ->
    [];
posix_time_1_test(Config) ->
    TS0 = {1355,672572,499999},
    TS1 = {1355,672572,500001},
    BasePosixTime = 1355672572,
    IncPosixTime = BasePosixTime + 1,
    BasePosixTime = sc_util:posix_time(TS0),
    IncPosixTime = sc_util:posix_time(TS1),
    Config.

make_child_spec_test() ->
    [].

make_child_spec_test(doc) ->
    [
        "sc_util:make_child_spec/2 test"
    ];
make_child_spec_test(Config) ->
    Mod = test_mod,
    Name = test_name,
    SpecCfg = [],
    Opts = [{mod, Mod},
            {name, Name},
            {config, SpecCfg}],
    Timeout = 1234,
    Spec = sc_util:make_child_spec(Opts, Timeout),
    Spec = {Name, {Mod, start_link, [Name, SpecCfg]},
            permanent, Timeout, worker, [Mod]},
    Config.

xdigit_test() ->
    [].

xdigit_test(doc) ->
    [
        "sc_util:xdigit/1 test"
    ];
xdigit_test(Config) ->
    Hexdigs = lists:zip(lists:seq(0, 15), "0123456789abcdef"),
    _ = [XD = sc_util:xdigit(N) || {N, XD} <- Hexdigs],
    _ = [?assertThrow(sc_util:xdigit(X), throw, {invalid_nybble, X}) ||
         X <- [-1, 16, foo, <<"bar">>]],
    Config.


nybble_test() ->
    [].

nybble_test(doc) ->
    [
        "sc_util:nybble/1 test"
    ];
nybble_test(Config) ->
    Digs = lists:seq(0, 9),
    Hexdigs = lists:seq(10, 15),
    HexZip = lists:zip(Digs ++ Hexdigs ++ Hexdigs,
                       "0123456789abcdefABCDEF"),
    _ = [N = sc_util:nybble(XD) || {N, XD} <- HexZip],
    _ = [?assertThrow(sc_util:nybble(X), throw, {invalid_hex_char, X}) ||
         X <- [$g, $G, $a - 1, $A - 1, foo, <<"bar">>]],
    Config.


req_s_test() ->
    [].

req_s_test(doc) ->
    [
        "sc_util:req_s/1 test"
    ];
req_s_test(suite) ->
    [];
req_s_test(Config) ->
    req_s_test_impl(fun sc_util:req_s/1),
    Config.

req_binary_or_s_test() ->
    [].

req_binary_or_s_test(doc) ->
    [
        "sc_util:req_binary_or_s/1 test"
    ];
req_binary_or_s_test(suite) ->
    [];
req_binary_or_s_test(Config) ->
    req_s_test_impl(fun sc_util:req_binary_or_s/1),
    req_bin_s_test_impl(fun sc_util:req_binary_or_s/1),
    Config.

req_val_test() ->
    [].

req_val_test(doc) ->
    [
        "sc_util:req_val/2 test"
    ];
req_val_test(suite) ->
    [];
req_val_test(Config) ->
    ?assertThrow(sc_util:req_val(k, []), throw, {missing_required_key, k}),
    v = sc_util:req_val(k, [{foo,bar}, {k,v}, 1, 2, 3]),
    true = sc_util:req_val(flag, [{foo,bar}, flag, {k,v}, 1, 2, 3]),
    Config.

req_val_binary_or_s_test() ->
    [].

req_val_binary_or_s_test(doc) ->
    [
        "sc_util:req_val_binary_or_s/2 test"
    ];
req_val_binary_or_s_test(suite) ->
    [];
req_val_binary_or_s_test(Config) ->
    ?assertThrow(sc_util:req_val_binary_or_s(k, []), throw, {missing_required_key, k}),
    "v" = sc_util:req_val_binary_or_s(k, [{foo,bar}, {k, "v"}, 1, 2, 3]),
    <<"v">> = sc_util:req_val_binary_or_s(k, [{foo,bar}, {k, <<"v">>}, 1, 2, 3]),
    ?assertThrow(sc_util:req_val_binary_or_s(flag, [{foo,bar}, flag, {k,v}, 1, 2, 3]),
                 throw,
                 {not_a_string, true}),
    Config.

req_val_s_test() ->
    [].

req_val_s_test(doc) ->
    [
        "sc_util:req_val_s/2 test"
    ];
req_val_s_test(suite) ->
    [];
req_val_s_test(Config) ->
    ?assertThrow(sc_util:req_val_s(k, []), throw, {missing_required_key, k}),
    ?assertThrow(sc_util:req_val_s(k, [{k, 1}]), throw, {not_a_string, 1}),
    ?assertThrow(sc_util:req_val_s(k, [{k, "    "}]), throw,
                 {validation_failed, empty_string_not_allowed}),
    V = "v",
    V = sc_util:req_val_s(k, [{foo,bar}, {k,V}, 1, 2, 3]),
    Config.

to_atom_test() ->
    [].

to_atom_test(doc) ->
    [
        "sc_util:to_atom/1 test"
    ];
to_atom_test(suite) ->
    [];
to_atom_test(Config) ->
    Res = 'An Atom',
    Res = sc_util:to_atom(Res),
    Res = sc_util:to_atom("An Atom"),
    Res = sc_util:to_atom(<<"An Atom">>),
    ?assertThrow(sc_util:to_atom({}), error, function_clause),
    Config.

to_bin_test() ->
    [].

to_bin_test(doc) ->
    [
        "sc_util:to_bin/1 test"
    ];
to_bin_test(suite) ->
    [];
to_bin_test(Config) ->
    Res = <<"123">>,
    Res = sc_util:to_bin(Res),
    Res = sc_util:to_bin('123'),
    Res = sc_util:to_bin("123"),
    Res = sc_util:to_bin(123),
    ?assertThrow(sc_util:to_bin({}), error, function_clause),
    Config.

to_list_test() ->
    [].

to_list_test(doc) ->
    [
        "sc_util:to_list/1 test"
    ];
to_list_test(suite) ->
    [];
to_list_test(Config) ->
    Res = "123",
    Res = sc_util:to_list(Res),
    Res = sc_util:to_list('123'),
    Res = sc_util:to_list("123"),
    Res = sc_util:to_list(<<"123">>),
    Res = sc_util:to_list(123),
    ?assertThrow(sc_util:to_list({}), error, function_clause),
    Config.

exported_types_test(doc) ->
    [
        "sc_util:exported_types/1 test"
    ];
exported_types_test(suite) ->
    [];
exported_types_test(Config) ->
    %% We're going to generate some fake types with different arities.
    %% Then we'll generate a module and dynamically compile it.
    ArityLo = 0, ArityHi = 5,
    ModName = export_type_test_1,

    %% Test happy path
    Types = generate_types("test_type", ArityLo, ArityHi),
    {ModName, Beam} = generate_types_mod(ModName, Types),
    ct:log("beam_lib:chunks() -> ~p~n", [beam_lib:chunks(Beam, [abstract_code])]),

    {ok, {ModName, ExpTypes}} = sc_util:exported_types(Beam),

    ct:log("ExpTypes: ~p~n", [ExpTypes]),
    SortedTypes = lists:sort(Types),    % bind
    SortedTypes = lists:sort(ExpTypes), % assert

    %% Test when no abstract code is generated
    {ModName, NACBeam} = generate_types_mod_no_abstract_code(ModName, Types),
    {ok, {ModName, []}} = sc_util:exported_types(NACBeam),

    %% Test when call fails
    {error, beam_lib, _Reason} = sc_util:exported_types(<<>>),

    Config.

%%====================================================================
%% Internal helper functions
%%====================================================================
test_hex_str() ->
    "325e0015046b7d25d10888e503d3248be84b9e1de4ecadda9f0a16dbfc3f6b74".

test_hex_bin() ->
    <<
    50,94,0,21,4,107,125,37,209,8,136,229,3,211,36,139,232,75,
    158,29,228,236,173,218,159,10,22,219,252,63,107,116
    >>.

get_sorted_req_props(ReqKeys, Props) ->
    {L1, L2} = sc_util:get_req_props(ReqKeys, Props),
    {lists:sort(L1), lists:sort(L2)}.

req_s_test_impl(F) when is_function(F, 1) ->
    ?assertThrow(F("    "),
                 throw,
                 {validation_failed, empty_string_not_allowed}),
    ?assertThrow(F(123),
                 throw,
                 {not_a_string, 123}),
    "Good string!" = F(" \t\r\nGood string! \t\r\n ").

req_bin_s_test_impl(F) when is_function(F, 1) ->
    ?assertThrow(F(<<"    ">>),
                 throw,
                 {validation_failed, empty_string_not_allowed}),
    <<"Good string!">> = F(<<" \t\r\nGood string! \t\r\n ">>).

-spec generate_types(BaseTypeName, ArityLo, ArityHi) -> Result when
      BaseTypeName :: string(), ArityLo :: non_neg_integer(),
      ArityHi :: non_neg_integer(), Result :: [{atom(), non_neg_integer()}].
generate_types(BaseTypeName, ArityLo, ArityHi) ->
    [{list_to_atom(BaseTypeName ++ "_" ++ integer_to_list(Arity)), Arity} ||
     Arity <- lists:seq(ArityLo, ArityHi)].

-spec generate_types_mod(ModName, Types, CompilerOptions) -> Result when
      ModName :: string(), Types :: list(), CompilerOptions :: list(),
      Result :: {atom(), binary()}.
generate_types_mod(ModName, Types, CompilerOptions) ->
    S = generate_types_mod_source(ModName, Types),
    dynamic_compile:from_string(S, CompilerOptions).

-spec generate_types_mod(ModName, Types) -> Result when
      ModName :: string(), Types :: list(), Result :: {atom(), binary()}.
generate_types_mod(ModName, Types) ->
    generate_types_mod(ModName, Types, [debug_info]).

-spec generate_types_mod_no_abstract_code(ModName, Types) -> Result when
      ModName :: string(), Types :: list(), Result :: {atom(), binary()}.
generate_types_mod_no_abstract_code(ModName, Types) ->
    generate_types_mod(ModName, Types, []).

-spec generate_types_mod_source(ModName, Types) -> Result when
      ModName :: string(), Types :: list(), Result :: string().
generate_types_mod_source(ModName, Types) ->
    S = generate_module_attr(ModName) ++
        generate_export_type_attr(Types) ++
        string:join(generate_typespecs(Types), ""),
    ct:log("Generated module: ~s~n", [S]),
    S.

%% -> ["A","B","C","D",...]
mkarglist(N) when N =< 26 ->
    [[$A + I] || I <- lists:seq(0, N - 1)].

generate_module_attr(ModName) ->
    "-module(" ++ atom_to_list(ModName) ++ ").\n".

generate_export_type_attr(Types) ->
    "-export_type([\n" ++ generate_export_types(Types) ++ "\n]).\n".

generate_typespecs(Types) ->
    [generate_typespec(Type, Arity) || {Type, Arity} <- Types].

generate_typespec(Type, Arity) ->
    ArgList = mkarglist(Arity),
    "-type " ++ atom_to_list(Type) ++
    "(" ++ string:join(ArgList, ",") ++ ")" ++
    " :: " ++
    case Arity of
        0 ->
            "any()";
        _ ->
            "{" ++ string:join(ArgList, ",") ++ "}"
    end ++ ".\n".

%% -> "type0/0,type1/1,type2/2,..."
generate_export_types(Types) ->
    L = [atom_to_list(Type) ++ "/" ++ integer_to_list(Arity) ||
         {Type, Arity} <- Types],
    string:join(L, ",").

