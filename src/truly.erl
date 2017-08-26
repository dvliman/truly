-module(truly).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/query", query_handler, []},
            {"/number", number_handler, []}
        ]}]),

    {ok, _} = cowboy:start_clear(http, [{port, 8080}],
        #{env => #{dispatch => Dispatch}}),

    sup:start_link().

stop(_State) ->
    ok.