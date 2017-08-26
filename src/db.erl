-module(db).
-behavior(gen_server).

-record(state, {data}).

-export([get/1, put/3]).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

get(Number) ->
    gen_server:call(?MODULE, {get, Number}).

put(Number, Context, Name) ->
    gen_server:call(?MODULE, {put, Number, Context, Name}).

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
            maps_put(N, {Context, Name}, Acc)
    end.

maps_put(K, V, Map) ->
    maps:update_with(K,
        fun(OldValue) ->
            sets:add_element(V, OldValue)
        end, sets_new(V), Map).

sets_new(Element) ->
    sets:add_element(Element, sets:new()).

handle_call({get, Number}, _, #state{data = Data} = State) ->
    Reply = case maps:find(Number, Data) of
                {ok, SetValue} ->
                    sets:to_list(SetValue);
                error ->
                    undefined
            end,

    {reply, Reply, State};

handle_call({put, Number, Context, Name}, _, #state{data = Data} = State) ->
    NewData = maps_put(Number, {Context, Name}, Data),
    {reply, ok, State#state{data = NewData}};

handle_call(reset, _, State) ->
    {reply, ok, State#state{data = maps:new()}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.