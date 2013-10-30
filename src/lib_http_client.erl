%%% @author Roowe <bestluoliwe@gmail.com>
%%% @copyright (C) 2013, Roowe
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2013 by Roowe <bestluoliwe@gmail.com>

-module(lib_http_client).

-include("define_logger.hrl").
-include("define_http_client.hrl").

-export([request/1,
         post/2,
         get/1,
         async_post/2
         ]).

request(Url) ->
    ibrowse:send_req(Url, [], get, [], ?HTTP_CLIENT_OPTIONS, ?HTTP_CLIENT_TIMEOUT).
    %% httpc:request(Url, get_httpc_rand_pid()).

get(Url) ->
    ibrowse:send_req(Url, [], get, [], ?HTTP_CLIENT_OPTIONS, ?HTTP_CLIENT_TIMEOUT).
    %% httpc:request(Url, get_httpc_rand_pid()).

post(Url, Body) ->
    ibrowse:send_req(Url, [], post, Body, ?HTTP_CLIENT_OPTIONS, ?HTTP_CLIENT_TIMEOUT).
    %%httpc:request(Method, Request, HttpOptions, Options, get_httpc_rand_pid()). 

async_post(Url, Body) ->
    ibrowse:send_req(Url, [], post, Body, ?HTTP_CLIENT_OPTIONS ++ [{stream_to, self()}], ?HTTP_CLIENT_TIMEOUT).
    %%httpc:request(Method, Request, HttpOptions, Options, get_httpc_rand_pid()). 
