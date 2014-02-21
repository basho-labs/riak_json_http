
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(rj_wm_collection).
-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    content_types_provided/2,
    malformed_request/2,
    resource_exists/2,
    to_json/2,
    is_authorized/2
    ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
    collections,
    username
    }).

%%% -- webmachine hooks 

init(_) ->
    {ok, #ctx{}}.

service_available(ReqData, Context=#ctx{}) ->
    {
        rj_http_config:is_enabled(),
        ReqData,
        Context
    }.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

malformed_request(ReqData, Context) ->
    {false, ReqData, Context}.

resource_exists(ReqData, Context) ->
    C1 = ensure_collections(Context),
    case C1#ctx.collections of
        undefined -> {false, ReqData, C1};
        _ -> {true, ReqData, C1}
    end.

to_json(ReqData, Context) ->
    {mochijson2:encode(
        {struct,[{<<"collections">>, structify(Context#ctx.collections, [])}]}
        % {struct, [{<<"collections">>, {struct, Context#ctx.collections}}]}
        ), ReqData, Context}.

is_authorized(ReqData, Context) ->
    case rj_auth_util:authorize(ReqData) of
        {ok, Username} ->
            rj_auth_util:success(ReqData, Context#ctx{username=Username});
        {failed, _Reason} ->
            rj_auth_util:failure(ReqData, Context);
        {error, _Reason} ->
            rj_auth_util:error(ReqData, Context)
    end.

%%% =================================================== internal functions

ensure_collections(Context) ->
    case riak_json:get_collections() of
        [] -> Context;
        C -> 
            lager:debug("Collections: ~p~n", [C]),
            Context#ctx{collections = C}
    end.

structify([], Accum) -> lists:reverse(Accum);
structify([{name, C} | R], Accum) ->
    structify(R, [ {struct, [{name, C}]} | Accum]).