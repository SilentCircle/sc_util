%%%-------------------------------------------------------------------
%%% @author Edwin Fine <efine@silentcircle.com>
%%% @copyright 2012,2013 Silent Circle LLC
%%% @doc
%%% Utility functions to decode SCIMP elements embedded in
%%% message stanzas.
%%% @end
%%%-------------------------------------------------------------------
-module(sc_scimp_util).

-export([
        get_scimp_op/1,
        msg_to_json/1,
        scimp_ops/0,
        scimp_type/1
    ]).

-include("sc_util.hrl").

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------
-type scimp_op() :: data | commit | confirm | dh1 | dh2.
-type json_term() :: list({binary(), json_term()})
    | list(json_term())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
%%====================================================================
%% API
%%====================================================================

-spec scimp_type(#xmlelement{}) -> scimp_op() | undefined.
scimp_type(Msg) ->
    get_scimp_op(msg_to_json(Msg)).

-spec msg_to_json(#xmlelement{}) -> json_term() | undefined.
msg_to_json(#xmlelement{name = "message", els = Els}) ->
    case get_element_cdata(Els, "x") of
        "?SCIMP:" ++ Rest ->
            try
                [Base64] = string:tokens(Rest, "."),
                jsx:decode(base64:decode(Base64))
            catch
                _:_ ->
                    undefined
            end;
        _ ->
            undefined
    end;
msg_to_json(_Other) ->
    undefined.

-spec get_scimp_op(json_term() | undefined) -> scimp_op() | undefined.
get_scimp_op(undefined) ->
    undefined;
get_scimp_op(JSON) ->
    get_scimp_op(JSON, scimp_ops()).

get_scimp_op(JSON, [{BKey, Key} | Keys]) ->
    case lists:keymember(BKey, 1, JSON) of
        true ->
            Key;
        false ->
            get_scimp_op(JSON, Keys)
    end;
get_scimp_op(_JSON, []) ->
    undefined.

-spec scimp_ops() -> [{binary(), atom()}].
scimp_ops() ->
    [
        {<<"data">>, data},
        {<<"commit">>, commit},
        {<<"confirm">>, confirm},
        {<<"dh1">>, dh1},
        {<<"dh2">>, dh2}
    ].

get_element([#xmlelement{name = Name} = El|_], Name) ->
    El;
get_element([_El|Rest], Name) ->
    get_element(Rest, Name);
get_element([], _Name) ->
    notfound.

get_cdata(L) ->
    binary_to_list(list_to_binary(get_cdata(L, []))).

get_cdata([#xmlcdata{cdata = CData} | L], S) ->
    get_cdata(L, [S, CData]);
get_cdata([_|L], S) ->
    get_cdata(L, S);
get_cdata([], S) ->
    S.

get_element_cdata(Els, Name) ->
    case get_element(Els, Name) of
        #xmlelement{els = SubEls} ->
            get_cdata(SubEls);
        notfound ->
            notfound
    end.

%% Example xmlelement{}:
%%
%% {xmlelement,"message",
%%             [{"type","chat"},
%%              {"to","aborigen@silentcircle.com"},
%%              {"id","1E8759A2-8DC5-4FAB-8911-30F9C265890F"}],
%%             [{xmlelement,"body",[],[]},
%%              {xmlelement,"x",
%%                          [{"xmlns","http://silentcircle.com"}],
%%                          [{xmlcdata,<<"?SCIMP:ewogICAgImRhdGEiOiB7CiAgICAgICAgInNlcSI6IDI2NzQxLAogICAgICAgICJtYWMiOiAiNmZpSEcyOXhSVG89IiwKICAgICAgICAibXNnIjogIlhycU9VbmtDQzZOZEJERXMrejhJWGUwT0ZsU3Y3bUpiQ1IxMVJUSWtyUnc1SE1qVmV4LytsTnFvREJINVFxbk14WHNxSTBpUWVxc1JrZEU4SzlvU1dpNnZaTGEyM2ZkY0QvL2cwcktaRENmakFJMklEMWM4bVJnTERXV3ZRWEM1R3ZuYklMRTVZLzhTVGRjUFdzYlZkSkwrUzc4NkRCVjNSelhneHc1RXpLTEVqcWc2bVZXZmlreXJzM3pYZjlIdFMwQTZhMEdKc2RaaXBOeHprZFQ0NTU4MmFaWWRNWm8zUzBuNm9yY3l2RWs9IgogICAgfQp9Cg==.">>}]}]})

