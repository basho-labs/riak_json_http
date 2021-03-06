
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

-module(rj_wm_schema).
-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    malformed_request/2,
    resource_exists/2,
    accept_json/2,
    to_json/2,
    delete_resource/2,
    is_authorized/2
    ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
    collection,
    schema_name,
    schema_content,
    username
    }).

init(_) -> 
    {ok, #ctx{}}.

service_available(ReqData, Context=#ctx{}) ->
    {
        rj_http_config:is_enabled(),
        ReqData,
        Context#ctx{
            schema_name = wrq:path_info(schema, ReqData),
            collection = wrq:path_info(collection, ReqData)
        }
    }.

%Schema delete is currently unsupported
allowed_methods(ReqData, Context) ->
    {['GET', 'PUT', 'DELETE'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_json}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

malformed_request(ReqData, Context=#ctx{collection=undefined}) ->
    {true, ReqData, Context};
malformed_request(ReqData, Context) ->
    {false, ReqData, Context}.

resource_exists(ReqData, Context) ->
    C1 = ensure_schema(Context),
    Result = case C1#ctx.schema_content of
        undefined -> {false, ReqData, C1};
        _ -> {true, ReqData, C1}
    end,

    lager:debug("Result: ~p~n", [Result]),

    Result.

accept_json(ReqData, Context) ->
    Result = store_schema(ReqData, Context),

    lager:debug("Result: ~p~n", [Result]),

    Result.

to_json(ReqData, Context) ->
    Result = {Context#ctx.schema_content, ReqData, Context},

    lager:debug("Result: ~p~n", [Result]),

    Result.

delete_resource(ReqData, Context) ->
    Result = delete_schema(ReqData, Context),

    lager:debug("Result: ~p~n", [Result]),

    Result.

is_authorized(ReqData, Context) ->
    Result = case rj_auth_util:authorize(ReqData) of
        {ok, Username} ->
            rj_auth_util:success(ReqData, Context#ctx{username=Username});
        {failed, _Reason} ->
            rj_auth_util:failure(ReqData, Context);
        {error, _Reason} ->
            rj_auth_util:error(ReqData, Context)
    end,

    lager:debug("Result: ~p~n", [Result]),

    Result.

%%% =================================================== internal functions

ensure_schema(Context) when Context#ctx.schema_name =:= undefined ->
    Result = case riak_json:get_default_schema(Context#ctx.collection) of
        {error, Reason} -> 
            lager:info("Couldn't find schema, reason: ~p~n", [Reason]),
            Context;
        SchemaContent -> 
            Context#ctx{schema_content = SchemaContent}
    end,

    lager:debug("Result: ~p~n", [Result]),

    Result;
ensure_schema(Context) ->
    Result = case riak_json:get_schema(Context#ctx.schema_name) of
        {error, Reason} -> 
            lager:info("Couldn't find schema with name: ~p, reason: ~p~n", [Context#ctx.schema_name, Reason]),
            Context;
        SchemaContent ->
            Context#ctx{schema_content = SchemaContent}
    end,

    lager:debug("Result: ~p~n", [Result]),

    Result.

store_schema(ReqData, Context) ->
    SchemaName = case Context#ctx.schema_name of
        undefined -> riak_json:get_schema_name(Context#ctx.collection);
        Name -> Name
    end,

    Response = riak_json:store_schema(SchemaName, wrq:req_body(ReqData)),

    Result = case Response of
        {error, Reason} -> 
            lager:info("Error storing schema: ~p", [Reason]),
            {error, Reason};
        ok ->
            riak_json:link_schema(Context#ctx.collection, SchemaName),
            {true, ReqData, Context};
        {ok, _} ->
            riak_json:link_schema(Context#ctx.collection, SchemaName),
            {true, ReqData, Context}
    end,

    lager:debug("Result: ~p~n", [Result]),

    Result.

delete_schema(ReqData, Context) ->
    Response = riak_json:delete_default_schema(Context#ctx.collection),

    Result = case Response of
        {error, Reason} ->
            {error, Reason};
        ok ->
            {true, ReqData, Context}
    end,

    lager:debug("Result: ~p~n", [Result]),

    Result.


