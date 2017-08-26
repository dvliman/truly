-module(db).
-behavior(gen_server).

-record(state, {data}).

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
            {ok, Lines} = ecsv:process_csv_string_with(
                binary_to_list(Bin), fun parse/2),

            Data = lists:foldl(fun build_index/2, maps:new(), Lines),

            {ok, #state{data = Data}};

        {error, Reason} ->
            io:format("db:init, error reading csv:~p~n", [Reason]),
            init:stop()
    end.

parse({eof}, Acc) ->
    Acc;
parse({newline, [Number, Context, Name]}, Acc) ->
    [[list_to_binary(Number),
      list_to_binary(Context),
      list_to_binary(Name)] | Acc];
parse(_, Acc) ->
    Acc. % skip if not 3 columns, don't need to crash

build_index([Number, Context, Name], Acc) ->
    case util:to_e164(Number) of
        {notok, BadNumber} ->
            io:format("db:parse, invalid-number:~p", [BadNumber]),
            Acc;
        {ok, N} ->
            maps:put({N, Context}, Name, Acc)
    end.

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