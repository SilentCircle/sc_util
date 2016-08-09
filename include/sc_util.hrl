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
%%% @doc Records and defines.
%%% @end
%%% ==========================================================================

-ifndef(sc_util_hrl).
-define(sc_util_hrl, true).

%%--------------------------------------------------------------------
%% Defines
%%--------------------------------------------------------------------
-define(SECONDS, 1).
-define(MINUTES, (60 * ?SECONDS)).
-define(HOURS,   (60 * ?MINUTES)).
-define(DAYS,    (24 * ?HOURS)).
-define(WEEKS,   ( 7 * ?DAYS)).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(xmlcdata, {cdata = <<>>}).

-record(xmlelement, {
        name  = "" :: string(),
        attrs = [] :: list({string(), string()}),
        els   = [] :: list(#xmlelement{} | #xmlcdata{})
    }).

-endif.
