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
%%% @doc Common types.
%%% @end
%%% ==========================================================================

-ifndef(SC_UTIL_TYPES_HRL).
-define(SC_UTIL_TYPES_HRL, true).

-define(DEFAULT_DNS_TIMEOUT, 5000). % ms

-type srv_prio() :: 0..65535.
-type srv_weight() :: 0..65535.
-type srv_port() :: 0..65535.
-type srv_host() :: string().
-type dns_ttl() :: 0..2147483647. % RFC 2181

-record(rr, {prio :: srv_prio(),
             weight :: srv_weight(),
             port :: srv_port(),
             host :: srv_host(),
             ttl :: dns_ttl()}).

-endif.
