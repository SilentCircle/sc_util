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
%%% @doc Assertion support
%%% @end
%%% ==========================================================================

-ifndef(sc_util_assert_hrl).
-define(sc_util_assert_hrl, true).

-define(fmtMsg(Fmt, Args), (lists:flatten(io_lib:format(Fmt, Args)))).
-define(throwMsg(Fmt, Args), throw(?fmtMsg(Fmt, Args))).
-define(throwExcMsg(ExcName, Fmt, Args), throw({ExcName, ?fmtMsg(Fmt, Args)})).
-define(throwExc(ExcName, ExtInfo), throw({ExcName, ExcInfo})).

-define(assertMsg(Cond, Fmt, Args),
        ((Cond) orelse
         ?throwMsg("Assertion failed: ~p~n" ++ Fmt, [??Cond] ++ Args))).

-define(assertExcMsg(Cond, Exc, Fmt, Args),
        ((Cond) orelse ?throwExcMsg(Exc, Fmt, Args))).

-define(assert(Cond), ?assertMsg((Cond), "", [])).

-define(assertBoolean(Name, Val),
        ?assertExcMsg(is_boolean(Val), expected_boolean,
                      "~p is expected to be boolean", [to_s(Name)])).

-define(assertNonEmptyStr(Name, Val),
        ?assertExcMsg(is_non_empty_bin(Val), expected_non_empty_string,
                      "~p is expected to be non-empty string", [to_s(Name)])).

-define(assertIntegerInRange(Name, Val, Lo, Hi),
        ?assertExcMsg(is_integer(Val) andalso Val >= Lo andalso Val =< Hi,
                      expected_integer_in_range,
                      "~p is expected to be integer between ~B and ~B",
                      [binary_to_list(Name), Lo, Hi])).

-compile({inline, [is_non_empty_bin/1]}).
-compile({nowarn_unused_function, [is_non_empty_bin/1]}).
is_non_empty_bin(B) -> is_binary(B) andalso byte_size(B) =/= 0.

-endif.
