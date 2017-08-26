-module(db).
-behavior(gen_server).

-record(state, {data = dict:new()}).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

get_timestamp() ->
    {Megasecs, Secs, Microsecs} = erlang:now(),
    (Megasecs * 1000000) + Secs + (Microsecs / 1000000).


init(DataPath) ->
    case file:open(DataPath, [read]) of
        {ok, Fd} ->

            Begin = get_timestamp(),
            {ok, Data} = ecsv:process_csv_file_with(Fd,
                fun parse/2, {maps:new(), 0}),
            End = get_timestamp(),

            Duration = End - Begin,

            io:format("Duration: ~p item/s~n", [Duration]),
            file:close(Fd),

            {ok, #state{data = Data}};

        {error, Reason} ->
            io:format("db:init, error reading csv:~p~n", [Reason]),
            init:stop()
    end.

parse({eof}, Acc) ->
    Acc;
parse({newline, Line}, {Acc, N}) ->
    io:format("parsing:~p~n", [N]),

    [Number, Context, Name] = string_line_to_binary_line(Line),

    case util:to_e164(Number) of
        {notok, NonE164} ->
            io:format("db:parse, invalid-number:~p on line:~p~n", [NonE164, Line]),
            {Acc, N + 1};
        {ok, Normalized} ->
            {maps:put({Normalized, Context}, Name, Acc), N + 1}
    end.

string_line_to_binary_line([First, Second, Third]) ->
    [list_to_binary(First),
     list_to_binary(Second),
     list_to_binary(Third)].

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