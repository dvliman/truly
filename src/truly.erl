-module(truly).
-behavior(application).

-export([start/2,
         stop/1,
         config/2]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/query", query_handler, []},
            {"/number", number_handler, []}
        ]}]),

    Port = truly:config(http_port, 8080),

    {ok, _} = cowboy:start_clear(http, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),

    sup:start_link().

stop(_State) ->
    ok.

config(Key, Default) ->
    application:get_env(truly, Key, Default).
