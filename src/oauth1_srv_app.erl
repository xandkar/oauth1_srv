-module(oauth1_srv_app).

-behaviour(application).

-export(
    [ start/2
    , stop/1
    ]).


-define(cfg, oauth1_srv_cfg).


%% ============================================================================
%% Callbacks
%% ============================================================================

start(_StartType, _StartArgs) ->
    ok = start_server(),
    oauth1_srv_sup:start_link().

stop(_State) ->
    ok.


%% ============================================================================
%% Helpers (Cowboy setup)
%% ============================================================================

-spec start_server() ->
    ok.
start_server() ->
    Routes   = routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Middlewares =
        [ cowboy_router
        , cowboy_handler
        | ?cfg:get(middlewares)
        ],
    ListenerName = ?cfg:get(listener_name),
    ListenPort   = ?cfg:get(listen_port),
    ListenIP     = ?cfg:get(listen_ip),
    TransOpts =
        [ {port , ListenPort}
        , {ip   , ListenIP}
        | opts_trans_cert()
        ],
    NumberOfAcceptors = ?cfg:get(number_of_acceptors),
    ProtoOpts =
        [ {env         , [{dispatch, Dispatch}]}
        , {middlewares , Middlewares}
        ],
    Res = cowboy:start_https( ListenerName
                                , NumberOfAcceptors
                                , TransOpts
                                , ProtoOpts
                                ),
    io:format("cowboy:start_https(...) -> ~p~n", [Res]),
    {ok, _} = Res,
    ok.

-spec opts_trans_cert() ->
    [{atom(), binary() | string()}].
opts_trans_cert() ->
    DirSSL     = ?cfg:get_ssl_dir(),
    FileCertCA  = filename:join(DirSSL, ?cfg:get(file_cert_ca)),
    FileCert    = filename:join(DirSSL, ?cfg:get(file_cert)),
    FileKey     = filename:join(DirSSL, ?cfg:get(file_key)),
    Password    = ?cfg:get(password),
    [ {cacertfile , FileCertCA}
    , {certfile   , FileCert}
    , {keyfile    , FileKey}
    , {password   , Password}
    ].

-spec routes() ->
    [ { Path          :: binary()
      , HandlerModule :: atom()
      , Options       :: [term()]
      }
    ].
routes() ->
    [].
