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

%% @author Ed Fine <efine@silentcircle.com>
%% @copyright 2015 Silent Circle
%% @doc
%%
%% == SRV records ==
%%
%% SRV records essentially allow transparent DNS-level redirects of services to another domain or port. A simple example is when you have an XMPP server and you want users to have addresses like username@example.com, but your XMPP server is really installed on xmpp.example.com. In principle they work the same way as MX records do for email.
%%
%% For a server `example.com' wanting to delegate its XMPP services to the server at 'xmpp.example.com', here are some example records:
%% ```
%% _xmpp-client._tcp.example.com. 18000 IN SRV 0 5 5222 xmpp.example.com.
%% _xmpp-server._tcp.example.com. 18000 IN SRV 0 5 5269 xmpp.example.com.
%% '''
%%
%% The target domain **MUST** be an existing A record of the target server; it cannot be an IP address, and cannot be a CNAME record.
%%
%% The 18000 in this example is the TTL (time-to-live), it tells other servers how long they should cache your record for - a higher number will reduce DNS traffic, and result in slightly faster connections (since DNS info will be more likely to be cached, and won't need to be re-fetched). A lower TTL is more useful if you are going to be changing your record, since you have to wait for the TTL until all caches have expired your old record.
%%
%% The 0 and 5 are the record's priority and weight. These values are specific to SRV records, and allow you to have multiple targets with different priorities (e.g. for load balancing or fallback in case of a down server) - lower priority targets are tried first. The weight is used to bias resolvers towards certain targets in case of a priority tie. Most services will not need to change these values, and 0 and 5 are sensible defaults.
%%
%% Next is the port the service is running on. Clients will typically connect to 5222.
%%
%% == How to use SRV records ==
%%
%% Clients resolve the SRV records for _xmpp-client._tcp.example.com. One or more SRV records will be returned. Clients then select a record based on priority and weight as described in the table below.
%%
%% ```
%% Priority     The priority of the server. Clients attempt to contact the server with the lowest priority.
%% Weight       A load-balancing mechanism that is used when selecting a target host from those that have the same priority. Clients randomly choose SRV records that specify target hosts to be contacted, with probability proportional to the weight
%% Port Number  The port where the server is listening for this service.
%% Target       The fully qualified domain name of the host computer.
%% '''
%%
%% === Example 1: Equal Priorities ===
%%
%% Let's say an SRV resolution request returns the following:
%%
%% ```
%% Priority   Weight    Port   Target
%%    0         30      5222   JHGAJSGHD.example.net
%%    0         40      5222   KJGOIUTRG.example.net
%%    0         15      5222   NBGDPRLGH.example.net
%%    0         15      5222   WMFPSNMGJ.example.net
%% '''
%% <ul>
%% <li>The lowest priority records are chosen. In this case, all of the records are at priority 0.</li>
%% <li>A record is randomly selected from the group such that its probability of selection is proportional to its relative weight. In this example, the weights add up nicely to 100, so they could be thought of as percentages. In this arrangement, KJGOIUTRG.example.net would be chosen 40% of the time, and NBGDPRLGH.example.net 15% of the time.</li>
%% </ul>
%%
%% === Example 2: Different Priorities ===
%%
%% ```
%% Priority   Weight  Port    Target
%%     0        30    5222    JHGAJSGHD.example.net
%%     1        40    5222    KJGOIUTRG.example.net
%%     2        15    5222    NBGDPRLGH.example.net
%%     3        15    5222    WMFPSNMGJ.example.net
%% '''
%%
%% Here, the weights are irrelevant. JHGAJSGHD.example.net will be chosen every time. If connection attempts to it fail, then the next highest priority record, KJGOIUTRG.example.net, is chosen, and so on.
%%
%% == What this module provides ==
%%
%% The idea is to call fetch_srv_rrs/1, then pick_server/1 using the `Rest' list, until a host is reachable.
%%
%% === Example ===
%% ```
%% RRs = sc_util_srv:fetch_srv_rrs("_some-client._tcp.some.domain"),
%% case try_connect(RRs) of
%%     {ok, Conn, SrvInfo} ->
%%         use_connection(Conn, SrvInfo);
%%     {error, Reason} ->
%%         handle_this(Reason)
%% end.
%%
%% try_connect([]) ->
%%     {error, no_more_hosts_to_try};
%% try_connect(RRs) ->
%%    case sc_util_srv:pick_server(RRs) of
%%        {Host, Port, _TTL, Rest} = SrvInfo ->
%%            case my_connect(Host, Port) of
%%                {ok, Connection} ->
%%                    {ok, Connection, SrvInfo};
%%                {error, connection_failure} ->
%%                    try_connect(Rest);
%%                {error, Reason} ->
%%                    handle_error(Reason)
%%            end;
%%        undefined ->
%%            {error, no_hosts_available}
%%    end.
%%
%% '''
%% @end
-module(sc_util_srv).

-export([
        fetch_srv_rrs/1,
        pick_server/1
    ]).

-include("sc_util_srv.hrl").

-export_type([
              srv_prio/0,
              srv_weight/0,
              srv_port/0,
              srv_host/0,
              dns_ttl/0,
              dns_srv_tuple/0,
              dns_rr_prop/0,
              dns_rr_props/0,
              rr/0,
              rrs/0,
              srv_info/0,
              service_name/0,
              weighted_rr/0,
              weighted_rrs/0,
              weighted_sums/0
             ]).


-type dns_srv_tuple() :: {srv_prio(), srv_weight(), srv_port(), srv_host()}.
-type dns_rr_prop() :: {domain, string()} |
                       {type, atom()} |
                       {class, atom()} |
                       {ttl, dns_ttl()} |
                       {data, dns_srv_tuple()}.

-type dns_rr_props() :: [dns_rr_prop()].

-type rr() :: #rr{}.
-type rrs() :: [rr()].

-type srv_info() :: {srv_host(), srv_port(), dns_ttl(), rrs()}.

-type service_name() :: binary() | string().
-type weighted_rr() :: {rr(), RunningTotal :: non_neg_integer()}.
-type weighted_rrs() :: [weighted_rr()].
-type weighted_sums() :: {weighted_rrs(), Sum :: non_neg_integer()}.

%%%--------------------------------------------------------------------
%%% API
%%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc Pick the server with the highest weighting and priority from
%% the list of RRs. Return `undefined' if RRs is an empty list.
%% @end
%%--------------------------------------------------------------------
-spec pick_server(RRs) -> SrvInfo
    when RRs :: rrs(), SrvInfo :: srv_info().
pick_server(RRs) ->
    SortedRRs = sort_rrs(RRs),
    select_host(SortedRRs).

-spec fetch_srv_rrs(ServiceName) -> Result
    when ServiceName :: service_name(), Result :: {ok, rrs()} | {error, any()}.
fetch_srv_rrs(<<ServiceName/binary>>) ->
    fetch_srv_rrs(binary_to_list(ServiceName));
fetch_srv_rrs(ServiceName) when is_list(ServiceName) ->
    case inet_res:resolve(ServiceName, in, srv, [], ?DEFAULT_DNS_TIMEOUT) of
        {ok, DnsMsg} ->
            % Convert opaque DNS record structures to proplists
            DnsProps = inet_dns:(inet_dns:record_type(DnsMsg))(DnsMsg),
            case sc_util:req_val(anlist, DnsProps) of
                [] ->
                    {error, {no_srv_records_for, ServiceName}};
                DnsRRs ->
                    RRRecType = inet_dns:record_type(hd(DnsRRs)),
                    Res = dns_props_to_rrs([inet_dns:RRRecType(RR)
                                           || RR <- DnsRRs]),
                    {ok, Res}
            end;
        {error, Reason} ->
            {error, {dns_srv_resolution_error, {Reason, ServiceName}}}
    end.


-spec select_host(SortedRRs) -> SrvInfo
    when SortedRRs :: rrs(), SrvInfo :: srv_info() | undefined.
select_host([_|_] = SortedRRs) ->
    RR = case select_highest_prio(SortedRRs) of
             [RR0] ->
                 RR0;
             [_|_] = L ->
                 select_randomly_by_weight(L)
         end,
    make_srv_info(RR, lists:delete(RR, SortedRRs));
select_host([]) ->
    undefined.


%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------
-compile({inline, [make_srv_info/2, make_srv_info/4]}).

-spec make_srv_info(RR, Rest) -> SrvInfo
    when RR :: rr(), Rest :: rrs(), SrvInfo :: srv_info().
make_srv_info(RR, Rest) ->
    make_srv_info(RR#rr.host, RR#rr.port, RR#rr.ttl, Rest).


-spec make_srv_info(H, P, TTL, Rest) -> SrvInfo
    when H :: srv_host(), P :: srv_port(), TTL :: dns_ttl(),
         Rest :: rrs(), SrvInfo :: srv_info().
make_srv_info(H, P, TTL, Rest) ->
    {H, P, TTL, Rest}.


-compile({inline, [select_highest_prio/1]}).

-spec select_highest_prio(SortedRRs) -> HighestRRs
    when SortedRRs :: rrs(), HighestRRs :: rrs().
select_highest_prio(SortedRRs) ->
    Highest = (hd(SortedRRs))#rr.prio,
    lists:takewhile(fun(#rr{prio = P}) -> P =:= Highest end, SortedRRs).


-spec select_randomly_by_weight(RRs) -> RR
    when RRs :: rrs(), RR :: rr().
select_randomly_by_weight(RRs) ->
    {WeightedRRs, Sum} = weighted_sums(RRs),
    Rnd = random:uniform(Sum + 1) - 1, % range 0..N-1
    find_first_greater_or_equal(Rnd, WeightedRRs).


-spec find_first_greater_or_equal(Num, Weighted) -> RR
    when Weighted :: weighted_rrs(),
    Num :: pos_integer(), RR :: rr().
find_first_greater_or_equal(Num, Weighted) ->
    {RR, _} = hd(lists:dropwhile(fun({_RR, S}) -> S < Num end, Weighted)),
    RR.


-spec weighted_sums(RRs) -> WeightedSums
    when RRs :: rrs(), WeightedSums :: weighted_sums().
weighted_sums(RRs) ->
    lists:mapfoldl(
        fun(#rr{} = RR, Sum0) ->
                Sum = Sum0 + RR#rr.weight,
                {{RR, Sum}, Sum}
        end, 0, RRs).

sort_rrs(RRs) ->
    lists:sort(RRs).

dns_props_to_rrs(ListOfDnsProps) ->
    [dns_prop_to_rr(DnsProps) || DnsProps <- ListOfDnsProps].

-spec dns_prop_to_rr(DnsProps) -> RR
    when DnsProps :: dns_rr_props(), RR :: rr().
dns_prop_to_rr(DnsProps) ->
    {Prio, Weight, Port, Host} = sc_util:req_val(data, DnsProps),
    TTL = sc_util:req_val(ttl, DnsProps),
    #rr{prio = Prio, weight = Weight, port = Port, host = Host, ttl = TTL}.

