-module(oauth1_srv_SUITE).

-include_lib("oauth1_core/include/oauth1_parameter_names.hrl").

%% Callbacks
-export(
    [ all/0
    , groups/0
    , init_per_suite/1
    , end_per_suite/1
    ]).

%% Tests
-export(
    [ t_initiate_ok/1
    ]).


-define(GROUP, oauth1_srv).


%%=============================================================================
%% Callbacks
%%=============================================================================

all() ->
    [{group, ?GROUP}].

groups() ->
    Tests =
        [ t_initiate_ok
        ],
    Properties = [],
    [ {?GROUP, Properties, Tests}
    ].

init_per_suite(Cfg) ->
    ok = oauth1_srv_app:start_dev(),
    ok = application:start(inets),
    Cfg.

end_per_suite(_Cfg) ->
    application:stop(oauth1_srv).


%%=============================================================================
%% Tests
%%=============================================================================

t_initiate_ok(_Cfg) ->
    ClientID = register_client(),
    Realm           = <<"oblivion">>,
    ReqURL          = "https://localhost:8443/initiate",
    Callback        = <<"https://client/ready">>,
    % TODO: Verify signature with hueniverse guide.
    Signature       = <<"PZSLye5xlqQFa5YkRW1wbST2DI4=">>,
    SignatureMethod = <<"HMAC-SHA1">>,
    Timestamp       = <<"123456789">>,
    Nonce           = <<"foofoobahbah">>,
    Params =
        [ {?PARAM_REALM            , Realm}
        , {?PARAM_CONSUMER_KEY     , ClientID}
        , {?PARAM_SIGNATURE        , Signature}
        , {?PARAM_SIGNATURE_METHOD , SignatureMethod}
        , {?PARAM_TIMESTAMP        , Timestamp}
        , {?PARAM_NONCE            , Nonce}
        , {?PARAM_CALLBACK         , Callback}
        ],
    AuthorizationBin = oauth1_parameters:to_http_header_authorization(Params),
    Authorization    = binary_to_list(AuthorizationBin),
    ReqURL = "https://localhost:8443/initiate",
    ReqHeaders =
        [ {"Authorization", Authorization}
        ],
    ReqMethod = post,
    ReqContentType = "application/x-www-form-urlencoded",
    ReqBody = "",
    Request = {ReqURL, ReqHeaders, ReqContentType, ReqBody},
    ReqOptions = [],
    ReqHTTPOptions = [{ssl, [{verify, verify_none}]}],
    {ok, Response} = httpc:request(ReqMethod, Request, ReqHTTPOptions, ReqOptions),
    {Status, Headers, BodyRaw} = Response,
    {_HttpVsn, StatusCode, _StatusMsg} = Status,
    ct:log("Status: ~p", [Status]),
    ct:log("Headers: ~p", [Headers]),
    ct:log("BodyRaw: ~s", [BodyRaw]),
    ParseQS = hope_result:lift_exn(fun cow_qs:parse_qs/1),
    BodyParseResult = ParseQS(list_to_binary(BodyRaw)),
    ct:log("BodyParseResult: ~p", [BodyParseResult]),
    {ok, BodyParsed} = BodyParseResult,
    {some, _} = hope_kv_list:get(BodyParsed, <<"oauth_token">>),
    {some, _} = hope_kv_list:get(BodyParsed, <<"oauth_token_secret">>),
    200 = StatusCode,
    ok.


%%=============================================================================
%% Helpers
%%=============================================================================

register_client() ->
    ClientID     = <<"hero-of-kvatch">>,
    ClientSecret = <<"thereisnosword">>,
    ClientCreds  = oauth1_credentials:cons(client, ClientID, ClientSecret),
    {ok, ok}     = oauth1_credentials:store(ClientCreds),
    ClientID.
