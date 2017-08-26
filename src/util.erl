-module(util).

-export([to_e164/1,
         is_valid_e164/1]).

% normalize to E.164 format (see https://en.wikipedia.org/wiki/E.164)
% This is not a comprehensive E.164 parser. For production use,
% we might want to use googlei18n/libphonenumber. It is much more mature.

% The goal here is just to index by {number, context} => name as much as we can
% Caller should just log and drop the data instead of crashing. We apply the following basic rules:
%   if prefixed with + sign, and up to max 15 digits, assume valid E.164
%   if national number, assume US phone number (append country code '1'), convert to E.164
%   otherwise, not ok
to_e164(<<$+, Rest/binary>> = N) when byte_size(Rest) =< 14 -> % 15 - (+sign) = 14
    {ok, N};

to_e164(<<$(, Area:3/binary, $), _, Local1:3/binary, $-, Local2/binary>>) ->
    Normalized = <<"+1", Area/binary, Local1/binary, Local2/binary>>,
    {ok, Normalized};

to_e164(Number) ->
    {notok, Number}.

is_valid_e164(Number) ->
    case re:run(Number, "^\\+?\\d{10,14}$") of
        {match, _} -> true;
        nomatch -> false
    end.
