-module(test_SUITE).

-export([all/0]).

-export([normalize_e164_test/1,
         valid_e164_test/1,
         csv_parser_test/1,
         phones_multiple_context_test/1]).

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

reset_db() ->
    gen_server:call(whereis(db), reset).

equals_xs(Xs1, Xs2) ->
    true = lists:sort(Xs1) =:= lists:sort(Xs2).