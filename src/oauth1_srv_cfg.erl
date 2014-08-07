-module(oauth1_srv_cfg).

-export(
    [ get/1
    , get/2
    ]).


-define(APP, oauth1_srv).


get(Name) ->
    {ok, Value} = application:get_env(?APP, Name),
    Value.

get(Name, Default) ->
    application:get_env(?APP, Name, Default).
