-module(oauth1_srv_handler_authorize).

-export(
    % Cowboy
    [ init/3

    % Cowboy REST
    , allowed_methods/2
    , content_types_provided/2
    , is_authorized/2
    , rest_terminate/2
    , rest_init/2

    % Content providers
    , content_provider/2
    ]).


-record(state,
    {
    }).


%% ============================================================================
%% Cowboy
%% ============================================================================

init(_Transport, _R, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


%% ============================================================================
%% Cowboy REST
%% ============================================================================

rest_init(R1, _Opts) ->
    S1 = #state{},
    {ok, R1, S1}.

rest_terminate(_R, #state{}) ->
    ok.

allowed_methods(R, #state{}=S) ->
    AllowedMethods = [<<"GET">>],
    lager:info("AllowedMethods: ~p", [AllowedMethods]),
    {AllowedMethods, R, S}.

content_types_provided(R, #state{}=S) ->
    ContentType = {<<"application">>, <<"x-www-form-urlencoded">>, '*'},
    Handler = content_provider,
    ContentTypesAccepted = [{ContentType, Handler}],
    lager:info("ContentTypesAccepted: ~p", [ContentTypesAccepted]),
    {ContentTypesAccepted, R, S}.

is_authorized(R1, #state{}=S) ->
    {ok, {IsAuthorized, R2}} = oauth1_srv_authentication:is_client(R1),
    lager:info("IsAuthorized: ~p", [IsAuthorized]),
    {IsAuthorized, R2, S}.


%% ============================================================================
%% Content handlers
%% ============================================================================

content_provider(R1, #state{}=S) ->
    QueryParam = <<"oauth_token">>,
    case cowboy_req:qs_val(QueryParam, R1)
    of  {undefined, R2} ->
            lager:info("Error: query paremeter missing: ~p", [QueryParam]),
            ErrorData =
                [ {<<"error">>
                , {<<"query_parameter_missing">>, QueryParam}}
                ],
            Body = jsx:encode(ErrorData),
            R3 = cowboy_req:set_resp_body(Body, R2),
            {false, R3, S}
    ;   {TmpToken, R2} ->
            lager:info("OK: got tmp token: ~p", [TmpToken]),
            ReplyOnIOError =
                fun (E) ->
                    lager:error("IO error: ~p", [E]),
                    {ok, R3} = cowboy_req:reply(503, R2),
                    {halt , R3, S}
                end,
            case oauth1_server:authorize(TmpToken)
            of  {error, {io_error, _}}=Error     -> ReplyOnIOError(Error)
            ;   {error, low_entropy}=Error       -> ReplyOnIOError(Error)
            ;   {error, {unauthorized, _}=Error} ->
                    lager:info("Request error: ~p", [Error]),
                    Body = oauth1_server:error_to_bin(Error),
                    R3 = cowboy_req:set_resp_body(Body, R2),
                    {ok, R4} = cowboy_req:reply(401, R3),
                    {halt , R4, S}
            ;   {ok, CallbackURI} ->
                    CallbackBin = oauth1_uri:to_bin(CallbackURI),
                    CB = CallbackBin,
                    R3 = cowboy_req:set_resp_header(<<"location">>, CB, R2),
                    {ok, R4} = cowboy_req:reply(303, R3),
                    {halt , R4, S}
            end
    end.
