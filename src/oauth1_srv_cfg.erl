-module(oauth1_srv_cfg).

-export(
    [ get/1
    , get/2
    , get_ssl_dir/0
    ]).


-define(APP, oauth1_srv).


get(Name) ->
    {ok, Value} = application:get_env(?APP, Name),
    Value.

get(Name, Default) ->
    application:get_env(?APP, Name, Default).


-spec get_ssl_dir() ->
    string().
get_ssl_dir() ->
    PrivSSL = priv_ssl,
    case get(dir_ssl_data, PrivSSL)
    of  PrivSSL       -> filename:join(code:priv_dir(?APP), "ssl")
    ;   {custom, Dir} -> Dir
    end.
