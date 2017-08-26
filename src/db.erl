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

init(DataPath) ->
    case file:read_file(DataPath) of
        {ok, Bin} ->
            {ok, Data} = ecsv:process_csv_string_with(binary_to_list(Bin),
                fun parse/2, maps:new()),

            {ok, #state{data = Data}};

        {error, Reason} ->
            io:format("db:init, error reading csv:~p~n", [Reason]),
            init:stop()
    end.

parse({eof}, Acc) ->
    Acc;
parse({newline, Line}, Acc) ->
    [Number, Context, Name] = string_line_to_binary_line(Line),

    case util:to_e164(Number) of
        {notok, NonE164} ->
            io:format("db:parse, invalid-number:~p on line:~p~n", [NonE164, Line]),
            Acc;
        {ok, N} ->
            maps:update_with({N, Context},
                fun(OldValue) ->
                    ct:pal("duplicate:~p, value:~p", [{N, Context}, OldValue]),
                    Name
                end, Name, Acc)

%%            maps:put({N, Context}, Name, Acc)
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