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

%%%-------------------------------------------------------------------
%%% @author Edwin Fine <efine@silentcircle.com>
%%% @copyright 2012-2016 Silent Circle
%%% @doc
%%% General utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(sc_util).

-export([
         bitstring_to_hex/1,
         ensure_module_loaded/1,
         exported_types/1,
         find_first_error/1,
         get_req_props/2,
         hex_to_bitstring/1,
         make_child_spec/2,
         opt_val/3,
         posix_time/0,
         posix_time/1,
         req_s/1,
         req_binary_or_s/1,
         req_val/2,
         req_val_s/2,
         req_val_binary_or_s/2,
         strclean/2,
         strtrim/1,
         to_atom/1,
         to_bin/1,
         to_list/1,
         val/2,
         val/3
        ]).

%% Export for testing only
-export([
         xdigit/1,
         nybble/1
        ]).

-define(NAMED_CHILD(I, Mod, Type, Args, Timeout),
    {I, {Mod, start_link, Args}, permanent, Timeout, Type, [Mod]}
).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Convert list of hex digits to a bitstring.
%%
%% In other words, `"a"' will be converted to `<<10:4>>', not to `<<10>>'.
%% This is so that values like `"fff"' are unambiguously converted to
%% the 12-bit value `<<255, 15:4>>'. If you want a binary that is a
%% multiple of 8 bits, make sure the input hex string length is a
%% multiple of 2.
%% @end
%%--------------------------------------------------------------------
-type xdigit() :: 48..57 | % $0..$9
                  65..70 | % $A..$F
                  97..102. % $a..$f
-type hex_string() :: [xdigit()].
-type nybble() :: 0..15.

-spec hex_to_bitstring(hex_string()) -> bitstring().
hex_to_bitstring(L) when is_list(L) ->
    << <<(nybble(C)):4>> || C <- L >>.

%%--------------------------------------------------------------------
%% @doc Convert bitstring into a list of hex digits. The bit length
%% must be an integral multiple of 4.
%%
%% In other words, `<<10:4>>' will be converted to `"a"', `<<10>>' will be
%% converted to `"0a"', and `<<255,15:4>>' to `"fff"'.
%%
%% @end
%%--------------------------------------------------------------------
-spec bitstring_to_hex(bitstring()) -> hex_string().
bitstring_to_hex(<<B/binary>>) when bit_size(B) band 2#11 =:= 0 ->
    [xdigit(I) || <<I:4>> <= B].

-compile({inline, [{val, 2}, {val, 3}, {opt_val, 3}]}).
%%--------------------------------------------------------------------
%% @doc Get value from proplist `PL' corresponding to key `K'.
%% Return `undefined' if key not found, else return the value.
%% @end
%%--------------------------------------------------------------------
-spec val(K, PL) -> Result
    when K :: term(), PL :: sc_types:proplist(any(), any()),
         Result :: term() | undefined.
val(K, PL) ->
    proplists:get_value(K, PL).

%%--------------------------------------------------------------------
%% @doc Get value from proplist `PL' corresponding to key `K'.
%% Return `Def' if key not found, else return the value.
%% @end
%%--------------------------------------------------------------------
-spec val(K, PL, Def) -> Result
    when K :: term(), PL :: sc_types:proplist(any(), any()), Def :: term(),
         Result :: term() | undefined.
val(K, PL, Def) ->
    proplists:get_value(K, PL, Def).

%%--------------------------------------------------------------------
%% @doc Get optional value from proplist `PL' corresponding to key `K'.
%% Return `Def' if key not found, else return the value.
%% @end
%%--------------------------------------------------------------------
opt_val(K, PL, Def) ->
    val(K, PL, Def).

%%--------------------------------------------------------------------
%% @doc Get required value from proplist `PL' corresponding to key `K'.
%% @throws {missing_required_key, K :: term()}
%% @end
%%--------------------------------------------------------------------
req_val(K, PL) ->
    case val(K, PL) of
        undefined ->
            throw({missing_required_key, K});
        Val ->
            Val
    end.

%%--------------------------------------------------------------------
%% @doc Get required non-empty string value from proplist `PL',
%% corresponding to key `K'. The string is stripped of leading and
%% trailing whitespace and checked for zero length. If the string is
%% empty, an exception results.
%% @throws {missing_required_key, K :: term()} |
%%         {validation_failed, empty_string_not_allowed} |
%%         {not_a_string, S :: term()}
%% @end
%%--------------------------------------------------------------------
req_val_s(K, PL) ->
    req_s(req_val(K, PL)).

%%--------------------------------------------------------------------
%% @doc Check that `S' is a non-empty string.
%% The string is stripped of leading and trailing whitespace and checked
%% for zero length. If the string is empty, an exception results.
%% @throws {not_a_string, S :: term()}
%% @end
%%--------------------------------------------------------------------
req_s(S) ->
    strclean(S, fun validate_non_empty/1).

%%--------------------------------------------------------------------
%% @doc Get required non-empty string or binary string value from proplist
%% `PL',  corresponding to key `K'. The string is stripped of leading and
%% trailing whitespace and checked for zero length. If the string is
%% empty, an exception results.
%% @throws {missing_required_key, K :: term()} |
%%         {validation_failed, empty_string_not_allowed} |
%%         {not_a_string, S :: term()}
%% @end
%%--------------------------------------------------------------------
req_val_binary_or_s(K, PL) ->
    req_binary_or_s(req_val(K, PL)).

%%--------------------------------------------------------------------
%% @doc Check that `S' is a non-empty string or binary string.
%% The string is stripped of leading and trailing whitespace and checked
%% for zero length. If the string is empty, an exception results.
%% @throws {not_a_string, S :: term()}
%% @end
%%--------------------------------------------------------------------
req_binary_or_s(<<BinStr/binary>>) ->
    list_to_binary(req_binary_or_s(binary_to_list(BinStr)));
req_binary_or_s(S) ->
    strclean(S, fun validate_non_empty/1).

%%--------------------------------------------------------------------
%% @doc Check that `S' is a non-empty string. Return `S' if non-empty,
%% throw exception otherwise.
%% @throws {validation_failed, empty_string_not_allowed}
%% @end
%%--------------------------------------------------------------------
validate_non_empty([]) ->
     throw({validation_failed, empty_string_not_allowed});
validate_non_empty([_|_] = S) ->
            S.

%%--------------------------------------------------------------------
%% @doc Validate that `S' is a valid string, using the supplied function,
%% `Validate/1'.
%% The string is stripped of leading and trailing whitespace and `Validate/1'
%% is run on the result. Throw exception if `S' is not a list, otherwise
%% do what `Validate/1' does (return `S' or throw exception).
%% @throws {not_a_string, S :: term()}
%% @end
%%--------------------------------------------------------------------
strclean(S, Validate) when is_list(S), is_function(Validate, 1) ->
    Validate(strtrim(S));
strclean(S, Validate) when is_function(Validate, 1) ->
    throw({not_a_string, S}).

%%--------------------------------------------------------------------
%% @doc
%% Remove all leading and trailing whitespace characters.
%% @end
%%--------------------------------------------------------------------
-spec strtrim(Subject::string()) -> string().
strtrim(Subject) ->
    case re:run(Subject, "^\\s*([^\\s]+(\\s+[^\\s]+)*)\\s*$",
                [{capture, all_but_first, list},
                 global, dollar_endonly, unicode, dotall]) of
        {match, [[Trimmed|_]]} ->
            Trimmed;
        nomatch ->
            []
    end.


%%--------------------------------------------------------------------
%% @doc
%% Return the first tail of list `L' that begins with `{error, _}', or
%% the empty list if no such tail exists.
%% @end
%%--------------------------------------------------------------------
-spec find_first_error(L) -> Result when
      L :: [{error, any()} | any()], Result :: list().
find_first_error(L) ->
    lists:dropwhile(
        fun({error, _}) -> false;
            (_) -> true
        end, L).

%%--------------------------------------------------------------------
%% @doc
%% Return the binary conversion of `X'. If `X' is an atom, convert using
%% the `latin1' character set.
%% @end
%%--------------------------------------------------------------------
-spec to_bin(X) -> Result when
      X :: binary() | list() | atom() | integer(),
      Result :: binary().
to_bin(<<X/binary>>) ->
    X;
to_bin(X) when is_list(X) ->
    list_to_binary(X);
to_bin(X) when is_atom(X) ->
    atom_to_binary(X, latin1);
to_bin(X) when is_integer(X) ->
    to_bin(integer_to_list(X)).


%%--------------------------------------------------------------------
%% @doc
%% Return `X' as an atom.
%% @end
%%--------------------------------------------------------------------
-spec to_atom(X) -> Result when
      X :: atom() | binary() | list(), Result :: atom().
to_atom(X) when is_atom(X) ->
    X;
to_atom(X) when is_binary(X) ->
    to_atom(binary_to_list(X));
to_atom(X) when is_list(X) ->
    list_to_atom(X).

%%--------------------------------------------------------------------
%% @doc
%% Return `X' as a list.
%% @end
%%--------------------------------------------------------------------
-spec to_list(X) -> Result when
      X :: binary() | atom() | integer(), Result :: list().
to_list(X) when is_list(X) ->
    X;
to_list(<<X/binary>>) ->
    binary_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_integer(X) ->
    integer_to_list(X).

%%--------------------------------------------------------------------
%% @doc
%% Given a list of keys, look up each key in a proplist. Return a tuple
%% consisting of two lists: a list of matching properties, and a list
%% of missing keys.
%% @end
%%--------------------------------------------------------------------
-spec get_req_props(ReqKeys, Props) -> Result when
      ReqKeys :: [Key :: any()], Props :: proplists:proplist(),
      Result :: {Found :: [{Key :: any(), Value :: any()}],
                 NotFound :: [Key :: any()]}.
get_req_props(ReqKeys, Props) ->
    lists:foldl(
        fun(K, {FoundL, MissingL}) ->
                case lists:keyfind(K, 1, Props) of
                    false -> {FoundL, [K | MissingL]};
                    KV -> {[KV | FoundL], MissingL}
                end
        end, {[], []}, ReqKeys).

%%--------------------------------------------------------------------
%% @doc Ensure that module `Mod' is loaded. Throw an exception if it
%% cannot be loaded.
%% @throws {cannot_load_module, {Mod :: atom(), Reason :: any()}}
%% @end
%%--------------------------------------------------------------------
-spec ensure_module_loaded(Mod) -> true when
      Mod :: atom().
ensure_module_loaded(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, _} ->
            true;
        {error, Reason} ->
            throw({cannot_load_module, {Mod, Reason}})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Return the module name and a list of types that were exported by a
%% BEAM file, presumably using `-export_type'.
%%
%% If there is no abstract code in the BEAM file, return the module
%% name and an empty list.
%% @end
%%--------------------------------------------------------------------
-type beam_spec() :: module() | file:filename() | binary().
-type type_spec() :: {Type :: atom(), Arity :: non_neg_integer()}.
-spec exported_types(Beam) -> Result when
      Beam :: beam_spec(),
      Result :: {ok, {Mod :: module(), Types :: [type_spec()]}} |
                {error, beam_lib, Reason :: term()}.
exported_types(Beam) when is_list(Beam) orelse
                          is_atom(Beam) orelse
                          is_binary(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {Mod, ChunkData}} ->
            case proplists:get_value(abstract_code, ChunkData) of
                {_AbstVsn, Forms} ->
                    ExpTypeLists = [Form || {attribute, _, export_type, Form} <- Forms],
                    SetOfTypes = gb_sets:union([gb_sets:from_list(ExpTypes) ||
                                                ExpTypes <- ExpTypeLists]),
                    {ok, {Mod, gb_sets:to_list(SetOfTypes)}};
                no_abstract_code ->
                    {ok, {Mod, []}}
            end;
        Err ->
            Err
    end.

%%--------------------------------------------------------------------
%% @doc Return current POSIX time in seconds since the epoch.
%% This is a synonym for `erlang:system_time(seconds)'.
%% @end
%%--------------------------------------------------------------------
-spec posix_time() -> sc_types:posix_time().
posix_time() ->
    erlang:system_time(seconds).

%%--------------------------------------------------------------------
%% @doc Return POSIX time in seconds corresponding to `{M, S, U}' tuple.
%% Microseconds are rounded to the nearest second.
%% @end
%%--------------------------------------------------------------------

-spec posix_time(erlang:timestamp()) -> sc_types:posix_time().
posix_time({M,S,U}) ->
    M * 1000000 + S + if U < 500000 -> 0; true -> 1 end.


%%--------------------------------------------------------------------
%% @doc Return a worker child specification based on `Opts' and `Timeout'.
%% Microseconds are rounded to the nearest second.
%%
%% `Opts' is a proplist with 3 required values:
%% <ul>
%%   <li>`{mod, atom()}': Module name of worker.</li>
%%   <li>`{name, atom()}': Module ID.</li>
%%   <li>`{config, proplist()}': start_link args.</li>
%% </ul>
%%
%% `Timeout' is the child worker start timeout in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec make_child_spec(Opts, Timeout) -> Result when
      Opts :: [{mod, atom()} |
               {name, atom()} |
               {config, proplists:proplist()}],
      Timeout :: non_neg_integer(),
      Result :: {Name :: atom(), {Mod :: atom(), start_link, Args :: [any()]},
                 permanent, Timeout :: non_neg_integer(), worker,
                 [Mod :: atom()]}.
make_child_spec(Opts, Timeout) when is_list(Opts), is_integer(Timeout) ->
    Mod = req_val(mod, Opts),
    Name = req_val(name, Opts),
    Config = req_val(config, Opts),
    ?NAMED_CHILD(Name, Mod, worker, [Name, Config], Timeout).


%%====================================================================
%% Internals
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Return a lowercase hexadecimal digit corresponding to `Value'.
%% Throws exception when `Value' is not an integer or falls outside the range
%% 0..15.
%% @throws {invalid_nybble, N}
%% @end
%%--------------------------------------------------------------------
-spec xdigit(Value) -> Result when
      Value :: nybble(), Result :: xdigit().
xdigit(N) when N >= 0, N =< 9 ->
    $0 + N;
xdigit(N) when N >= 16#a, N =< 16#f ->
    $a - 10 + N;
xdigit(N) ->
    throw({invalid_nybble, N}).

%%--------------------------------------------------------------------
%% @doc Return an integer value in the range 0..15, corresponding to
%% `Hexdigit'. `Hexdigit' is a case-insensitive hexadecimal digit.
%% Throws exception when `C' is not a hexadecimal character.
%% @throws {invalid_hex_char, C}
%% @end
%%--------------------------------------------------------------------
-spec nybble(Hexdigit) -> Result when
      Hexdigit :: xdigit(), Result :: nybble().
nybble(C) when C >= $0, C =< $9 ->
    C - $0;
nybble(C) when C >= $a, C =< $f ->
    C + 10 - $a;
nybble(C) when C >= $A, C =< $F ->
    C + 10 - $A;
nybble(C) ->
    throw({invalid_hex_char, C}).

