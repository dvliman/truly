-module(test_SUITE).

-export([normalize_e164_test/1,
         valid_e164_test/1]).

normalize_e164_test(_Config) ->
    {ok, <<"+13058224036">>} = util:to_e164(<<"+13058224036">>),
    {ok, <<"+16094914267">>} = util:to_e164(<<"(609) 491-4267">>),
    {notok, Input} = util:to_e164((Input = <<"1 (609) 491-4267">>)).

valid_e164_test(_Config) ->
    true = util:is_valid_e164("+13058224036"),
    false = util:is_valid_e164("(305) 822 4036"),
    false = util:is_valid_e164("(305) 822-4036"),
    false = util:is_valid_e164("+1 (305) 822-4036").
