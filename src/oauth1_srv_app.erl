-module(oauth1_srv_app).

-behaviour(application).

-export(
    [ start/2
    , stop/1
    ]).


start(_StartType, _StartArgs) ->
    oauth1_srv_sup:start_link().


stop(_State) ->
    ok.
