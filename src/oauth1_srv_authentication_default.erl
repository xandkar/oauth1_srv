-module(oauth1_srv_authentication_default).

-behaviour(oauth1_srv_authentication).

-export(
    [ is_client/1
    , is_owner/1
    ]).


is_client(Req) ->
    % This should be fine for a default behavior, since client identity will be
    % verified at the OAuth level.
    {ok, {true, Req}}.

is_owner(Req) ->
    % TODO: Define a default user in app config and check creds here.
    {ok, {true, Req}}.
