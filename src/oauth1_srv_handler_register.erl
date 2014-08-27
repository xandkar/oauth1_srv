-module(oauth1_srv_handler_register).

-export(
    % Cowboy
    [ init/3

    % Cowboy REST
    , allowed_methods/2
    , content_types_provided/2
    , is_authorized/2
    , rest_terminate/2
    , rest_init/2

    % Content
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

content_types_provided(R, S) ->
    ContentType = {<<"application">>, <<"json">>, '*'},
    ContentProvider = content_provider,
    ContentTypesProvided = [{ContentType, ContentProvider}],
    lager:info("ContentTypesProvided: ~p", [ContentTypesProvided]),
    {ContentTypesProvided, R, S}.

is_authorized(R1, #state{}=S) ->
    {ok, {IsAuthorized, R2}} = oauth1_srv_authentication:is_client(R1),
    lager:info("IsAuthorized: ~p", [IsAuthorized]),
    {IsAuthorized, R2, S}.


%% ============================================================================
%% Content handlers
%% ============================================================================

content_provider(R1, #state{}=S) ->
    % TODO: Match IO Errors explicitly.
    case oauth1_server:register_new_client()
    of  {error, _}=IOError ->
            lager:error("IO error: ~p", [IOError]),
            {ok, R2} = cowboy_req:reply(503, R1),
            {halt, R2, S}
    ;   {ok, {{client, <<ID/binary>>}, {client, <<Secret/binary>>}}} ->
            NewClientCreds =
                [ {<<"id">>     , ID}
                , {<<"secret">> , Secret}
                ],
            % TODO: Perhaps encode as query string instead, for uniformity?
            Body = jsx:encode(NewClientCreds),
            {Body, R1, S}
    end.
