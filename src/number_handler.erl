-module(number_handler).

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         add_caller_id/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, add_caller_id}], Req, State}.

add_caller_id(Req, State) ->
    {ok, Payload, Req1} = cowboy_req:read_body(Req),

    #{<<"name">> := Name,
      <<"number">> := Number,
      <<"context">> := Context} = Data = jiffy:decode(Payload, [return_maps]),

    case util:is_valid_e164(Number) of
        false ->
            Reply = #{error => badrequest,
                      reason => number_not_e164_format},

            Req2 = cowboy_req:set_resp_body(jiffy:encode(Reply), Req1),
            {false, Req2, State};
        true ->
            ok = db:put(Number, Context, Name),

            Reply = #{result => ok,
                      data => Data},

            Req2  = cowboy_req:set_resp_body(jiffy:encode(Reply), Req1),
            {true, Req2, State}
    end.

