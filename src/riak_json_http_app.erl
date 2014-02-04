
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.
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

-module(riak_json_http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    add_routes(),
    {ok, self()}.

stop(_State) ->
    %TODO: Unregister Routes
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

add_routes() ->
    [webmachine_router:add_route(R) || R <- dispatch_table()].

props() ->
    [
        {auth_bypass, rj_http_config:auth_bypass()},
        {auth_module, rj_http_config:auth_module()}
    ].

routes() ->
    [
        {[rj_http_config:document_name(),"collection", collection], rj_wm_document, props()},
        {[rj_http_config:document_name(),"collection", collection, key], rj_wm_document, props()},
        {[rj_http_config:document_name(),"collection", collection, "query", '*'], rj_wm_query, props()},
        {[rj_http_config:document_name(),"collection", collection, "schema"], rj_wm_schema, props()},
        {[rj_http_config:document_name(),"collection", collection, "schema", schema], rj_wm_schema, props()}
    ].