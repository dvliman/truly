-module(truly).
-behavior(application).

-export([start/2,
         stop/1,
         config/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/query", query_handler, []},
            {"/number", number_handler, []}
        ]}]),

    Port = truly:config(http_port),

    {ok, _} = cowboy:start_clear(http, [{port, Port}],
        #{env => #{dispatch => Dispatch}}),

    sup:start_link().

stop(_State) ->
    ok.

config(Key) ->
    {ok, Value} = application:get_env(truly, Key),
    Value.