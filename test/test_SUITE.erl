-module(test_SUITE).

-export([all/0]).

-export([normalize_e164_test/1,
         valid_e164_test/1,
         csv_parser_test/1,
         phones_multiple_context_test/1,
         number_handler_non_e164_format/1,
         end_to_end/1]).

all() ->
    make:all([load]),

    ExportedFuns = test_SUITE:module_info(exports),

    TestFuns = lists:filtermap(
        fun({Fun, _}) when Fun =:= module_info; Fun =:= all ->
                false;
           ({Fun, _}) ->
                {true, Fun}
        end, ExportedFuns),

    lists:map(fun(F) -> {F, erlang:apply(?MODULE, F, [[]])} end, TestFuns).

normalize_e164_test(_TestConfig) ->
    {ok, <<"+13058224036">>} = util:to_e164(<<"+13058224036">>),
    {ok, <<"+16094914267">>} = util:to_e164(<<"(609) 491-4267">>),
    {notok, Input} = util:to_e164((Input = <<"1 (609) 491-4267">>)).

valid_e164_test(_TestConfig) ->
    true = util:is_valid_e164("+13058224036"),
    false = util:is_valid_e164("(305) 822 4036"),
    false = util:is_valid_e164("(305) 822-4036"),
    false = util:is_valid_e164("+1 (305) 822-4036").

csv_parser_test(_TestConfig) ->
    TwoLines = "+13058224036,zendesk,'Nowlin Saul'\n(609) 491-4267,home,Rubino Lennoxlove\n",

    % assert ecsv lib doing its job:
    %  - parse \n char
    %  - parse fields separated by comma (field value can be enclosed by single and double quote)
    %  - ignore \r if any
    %  - surface proper tagged value to caller (i.e newline, eof, etc)
    {ok, Lines} = ecsv:process_csv_string_with(TwoLines,
        fun(Line, Acc) -> [Line | Acc] end),

    [{newline,["+13058224036","zendesk","'Nowlin Saul'"]},
     {newline,["(609) 491-4267","home","Rubino Lennoxlove"]},
     {eof}]
        = lists:reverse(Lines).

phones_multiple_context_test(_) ->
    reset_db(),

    Number = <<"+7142532851">>,
    ok = db:put(Number, <<"facebook">>, <<"david liman">>),
    ok = db:put(Number, <<"github">>, <<"dvliman">>),

    equals_xs([{<<"facebook">>, <<"david liman">>},
               {<<"github">>, <<"dvliman">>}], db:get(Number)).

number_handler_non_e164_format(_) ->
    reset_db(),

    Payload = #{number => '1 (714) 253-2851',
                context => truly,
                name => david},

    {ok, "400", _, Body} = ibrowse:send_req(endpoint("number"),
        header(), post, jiffy:encode(Payload)),

    #{<<"error">>  := <<"badrequest">>,
      <<"reason">> := <<"number_not_e164_format">>}
        = jiffy:decode(Body, [return_maps]).

end_to_end(_) ->
    reset_db(),

    Payload = #{<<"number">> => <<"+17142532851">>,
                <<"context">> => <<"truly">>,
                <<"name">> => <<"david">>},

    ibrowse:send_req(endpoint("number"),
        header(), post, jiffy:encode(Payload)),

    {ok, "200", _, Res} = ibrowse:send_req(
        endpoint("query?number=%2B17142532851"), header(), get),

    #{<<"results">> := [Payload]} = jiffy:decode(Res, [return_maps]).

header() ->
    [{<<"Content-Type">>, <<"application/json">>}].

endpoint(Path) when is_list(Path) ->
    lists:flatten(io_lib:format("http://localhost:~p/~s",
        [truly:config(http_port), Path])).

reset_db() ->
    gen_server:call(db, reset).

equals_xs(Xs1, Xs2) ->
    true = lists:sort(Xs1) =:= lists:sort(Xs2).