-module(query_handler).

-export([init/2,
         allowed_methods/2,
         content_types_provided/2,
         query/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"*">>, <<"*">>, []}, query}], Req, State}.

query(Req, State) ->
    case catch cowboy_req:match_qs([number], Req) of
        {'EXIT', {validation_failed, _}} ->
            {badrequest(qs_number_required), Req, State};

        #{number := Number} ->
            case util:to_e164(Number) of
                {notok, _} ->
                    {badrequest(number_not_e164_format), Req, State};

                {ok, N} ->
                    case db:get(N) of
                        undefined ->
                            {badrequest(not_found), Req, State};
                        Xs ->
                            {serialize(N, Xs), Req, State}
                    end
            end
    end.

badrequest(Reason) ->
    Reply = #{error => badrequest,
              reason => Reason},
    jiffy:encode(Reply).

serialize(Number, Xs) ->
    Results = lists:map(
        fun({Context, Name}) ->
            #{name => Name,
              number => Number,
              context => Context}
        end, Xs),
    jiffy:encode(#{results => Results}).
