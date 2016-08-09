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
%%% @doc Common type specifications.
%%% @end
%%% ==========================================================================

-module(sc_types).

-include("sc_util.hrl").

-export_type([
        atom_or_string/0,
        atom_or_binary/0,
        binary_or_string/0,
        exml/0,
        gen_result/0,
        posix_time/0,
        prop/2,
        proplist/2,
        reg_prop/0,
        reg_proplist/0,
        reg_result/0
    ]).

-type atom_or_string() :: atom() | string().
-type binary_or_string() :: binary() | string().
-type atom_or_binary() :: atom() | binary().
-type exml() :: #xmlelement{}.
-type gen_result() :: {result, exml()} | {error, exml()}.
-type posix_time() :: integer().
-type prop(KT, VT) :: {KT, VT}.
-type proplist(KT, VT) :: [prop(KT, VT)].
-type reg_prop() :: prop(atom(), atom_or_binary()).
-type reg_proplist() :: [reg_prop(), ...].
-type reg_result() :: ok | {error, term()}.
