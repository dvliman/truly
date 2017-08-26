-module(db).
-behavior(gen_server).

-record(state, {data = dict:new()}).

-export([normalize/1]).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(DataPath) ->
    case file:open(DataPath, [read]) of
        {ok, Fd} ->
            {ok, Data} = ecsv:process_csv_file_with(Fd,
                fun parse_file/2, maps:new()),

            {ok, #state{data = Data}};

        {error, Reason} ->
            io:format("db:init/1, error reading csv:~p", [Reason]),
            init:stop()
    end.

parse_file({eof}, Acc) ->
    Acc;
parse_file({newline, [Number, Context, Name] = Line}, Acc) ->
    Key   = {normalize(Number), Context},
    maps:put(Key, Name, Acc).

% normalize to E.164 format
% https://en.wikipedia.org/wiki/E.164
normalize("+" ++ Rest) ->
    ct:pal("rest:~p", [Rest]),
    Rest;
normalize(Number) ->
    ct:pal("number:~p", [Number]),
    Number.

handle_call(Msg, From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.