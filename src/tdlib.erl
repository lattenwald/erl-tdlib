-module(tdlib).

-behaviour(gen_server).

-export([start_link/0, start_link/2]).
-export([handle_call/3, handle_cast/2, init/1, handle_info/2, code_change/3, terminate/2]).
-export([register_handler/2, config/2, send/2, execute/2, method/2, send_sync/2, send_sync/3]).
-export([phone_number/2, auth_code/2, auth_code/3, auth_code/4, auth_password/2]).

-export([get_handlers/1, get_auth_state/1, get_config/1, get_version/1]).

-export([remove_extra/2]).

-define(RECEIVE_TIMEOUT, 5.0).
-define(SEND_SYNC_TIMEOUT, 5000).

-record(state, {
    tdlib_version = null,
    tdlib_commit = null,
    tdlib = null,
    handlers,
    auth_state = null,
    config = null,
    extra = #{},
    extra_index = 0
}).

%%====================================================================
%% API functions
%%====================================================================

%%====================================================================
%% @doc Start new tdlib instance.
%%
%% @returns <code>{ok, Pid}</code>
%%====================================================================
start_link() ->
    gen_server:start_link(?MODULE, null, []).

%%====================================================================
%% @doc Start new tdlib instance, register it and send config when ready.
%%
%% @see config/2
%% @returns <code>{ok, Pid}</code>
%%====================================================================
start_link(Name, Config) ->
    gen_server:start_link(Name, ?MODULE, Config, []).

%%====================================================================
%% @doc Add handler to tdlib instance.
%%
%% @param Pid tdlib gen_server pid
%% @param Handler should be pid, which will receive messages
%% <code>{incoming, JsonMessage}</code>
%%====================================================================
register_handler(Pid, Handler) ->
    gen_server:call(Pid, {register_handler, Handler}).

%%====================================================================
%% @doc Send tdlib configuration.
%%
%% @param Pid tdlib gen_server pid
%% @param Cfg keyword list, should have at least keys <code>api_id</code>,
%% <code>api_hash</code> and <code>database_directory</code>. Any other
%% key will override
%% default configuration.
%%====================================================================
config(Pid, Cfg) ->
    try
        Config = create_config(Cfg),
        gen_server:call(Pid, {config, Config})
    catch
        _:Err = {missing_param, _} ->
            {error, Err}
    end.

%%====================================================================
%% @doc Send tdlib request.
%%
%% @param Pid tdlib gen_server pid
%% @param Request binary message or term to be JSON-encoded.
%%====================================================================
send(Pid, Request) when is_binary(Request) ->
    gen_server:cast(Pid, {send, Request});
send(Pid, Request) ->
    Msg = jsx:encode(Request),
    send(Pid, Msg).

%%====================================================================
%% @doc Send tdlib request and block until response is received.
%%
%% @see send_sync/3
%% @end
%%====================================================================
send_sync(Pid, Request) ->
    send_sync(Pid, Request, ?SEND_SYNC_TIMEOUT).

%%====================================================================
%% @doc Send tdlib request and block until response is received.
%%
%% Inside it's a <code>gen_server:call</code>, so expect
%% <code>{timeout, _}</code> error if you need to handle this.
%%
%% @param Pid tdlib gen_server pid
%% @param Request list of pairs to be JSON-encoded.
%% @param Timeout timeout
%% @returns decoded response
%%====================================================================
send_sync(Pid, Request, Timeout) ->
    gen_server:call(Pid, {send_sync, Request, Timeout}, Timeout).

%%====================================================================
%% @doc Execute synchronous tdlib request.
%%
%% @param Pid tdlib gen_server pid
%% @param Request binary or term to be JSON-encoded
%% @returns <code>{ok, JsonData}</code> or <code>null</code> if
%% tdlib was unable to parse request. See
%% <a href="https://core.telegram.org/tdlib/docs/td__json__client_8h.html#a6d6c76380793072d4a9ce3c71ba0f1cf"><tt>tdlib execute method documentation</tt></a>
%%====================================================================
execute(Pid, Request) when is_binary(Request) ->
    gen_server:call(Pid, {execute, Request});
execute(Pid, Request) ->
    Msg = jsx:encode(Request),
    execute(Pid, Msg).

%%====================================================================
%% @doc Send phone number.
%%
%% @param Pid tdlib gen_server pid
%% @param PhoneNumber should be binary.
%%====================================================================
phone_number(Pid, PhoneNumber) when is_binary(PhoneNumber) ->
    gen_server:cast(Pid, {phone_number, PhoneNumber}).

%%====================================================================
%% @doc Send authentication code.
%%
%% @param Pid tdlib gen_server pid
%% @param Code should be binary.
%%====================================================================
auth_code(Pid, Code) when is_binary(Code) ->
    auth_code(Pid, Code, null, null).

%%====================================================================
%% @doc Send authentication code for unregistered user.
%%
%% @param Pid tdlib gen_server pid
%% @param Code should be binary.
%% @param FirstName should be binary
%%====================================================================
auth_code(Pid, Code, FirstName) ->
    auth_code(Pid, Code, FirstName, null).

%%====================================================================
%% @doc Send authentication code for unregistered user.
%%
%% @param Pid tdlib gen_server pid
%% @param Code should be binary.
%% @param FirstName should be binary
%% @param LastName should be binary
%%====================================================================
auth_code(Pid, Code, FirstName, LastName) ->
    Request = method(
        <<"checkAuthenticationCode">>,
        lists:filter(
            fun({_, Val}) -> Val /= null end,
            [
                {<<"code">>, Code},
                {<<"first_name">>, FirstName},
                {<<"last_name">>, LastName}
            ]
        )
    ),
    send_sync(Pid, Request).

%%====================================================================
%% @doc Send password.
%%
%% @param Pid tdlib gen_server pid
%% @param Password should be binary.
%%====================================================================
auth_password(Pid, Password) when is_binary(Password) ->
    Request = method(<<"checkAuthenticationPassword">>, [{<<"password">>, Password}]),
    send_sync(Pid, Request).

%%====================================================================
%% @doc Get list of current handlers.
%%
%% @param Pid tdlib gen_server pid
%%====================================================================
get_handlers(Pid) ->
    gen_server:call(Pid, get_handlers).

%%====================================================================
%% @doc Get current auth state of tdlib.
%%
%% @param Pid tdlib gen_server pid
%%====================================================================
get_auth_state(Pid) ->
    gen_server:call(Pid, get_auth_state).

%%====================================================================
%% @doc Get tdlib configuration.
%%
%% @param Pid tdlib gen_server pid
%%====================================================================
get_config(Pid) ->
    gen_server:call(Pid, get_config).

%% @private
remove_extra(Pid, Index) ->
    gen_server:cast(Pid, {remove_extra, Index}).

%%====================================================================
%% @doc Get tdlib version.
%%
%% @param Pid tdlib gen_server pid
%%====================================================================
get_version(Pid) ->
    gen_server:call(Pid, get_version).

%%====================================================================
%% callbacks
%%====================================================================

%% @private
init(Config) ->
    self() ! init,
    {ok, #state{config = Config, handlers = sets:new()}}.

%% @private
handle_info(init, State) ->
    {ok, Tdlib} = tdlib_nif:new(),
    self() ! poll,
    {noreply, State#state{tdlib = Tdlib}};
handle_info(poll, State = #state{tdlib = Tdlib}) ->
    Parent = self(),
    spawn(fun() ->
        Resp = tdlib_nif:recv(Tdlib, ?RECEIVE_TIMEOUT),
        Parent ! {received, Resp}
    end),
    {noreply, State};
handle_info({received, null}, State) ->
    self() ! poll,
    {noreply, State};
handle_info({received, {ok, Msg}}, State = #state{extra = Extra, handlers = Handlers}) ->
    lager:debug("[~p] recv: ~p", [self(), Msg]),
    Data = jsx:decode(Msg, [{return_maps, true}]),

    {SyncReply, NewExtra} =
        case maps:get(<<"@extra">>, Data, null) of
            null ->
                {null, Extra};
            E ->
                case maps:take(E, Extra) of
                    error -> {null, Extra};
                    Other -> Other
                end
        end,

    lager:debug("[~p] Received: ~ts", [self(), Msg]),

    case SyncReply of
        null ->
            case maps:get(<<"@type">>, Data) of
                <<"updateAuthorizationState">> ->
                    handle_auth(self(), Data);
                _ ->
                    ok
            end,

            lists:foreach(
                fun(Handler) ->
                    Handler ! {incoming, Data}
                end,
                sets:to_list(Handlers)
            );
        {ReplyTo, TRef} ->
            lager:debug("[~p] ReplyTo: ~p, TRef: ~p", [self(), ReplyTo, TRef]),
            timer:cancel(TRef),
            gen_server:reply(ReplyTo, Data)
    end,

    self() ! poll,

    NewState = case Data of
                   #{<<"@type">> := <<"updateOption">>,
                    <<"name">> := OptionName,
                    <<"value">> := #{<<"value">> := OptionValue}} ->
                       update_option(State, OptionName, OptionValue);
                   _ ->
                       State
               end,

    {noreply, NewState#state{extra = NewExtra}};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
handle_call(
    {config, Cfg},
    _From,
    State = #state{auth_state = <<"authorizationStateWaitTdlibParameters">>}
) ->
    DefaultConfig = [
        {<<"application_version">>, <<"Unknown">>},
        {<<"device_model">>, <<"Unknown">>},
        {<<"enable_storage_optimizer">>, true},
        {<<"files_directory">>, null},
        {<<"ignore_file_names">>, true},
        {<<"system_language_code">>, <<"en">>},
        {<<"system_version">>, <<"Unknown">>},
        {<<"use_chat_info_database">>, true},
        {<<"use_file_database">>, true},
        {<<"use_message_database">>, true},
        {<<"use_secret_chats">>, false},
        {<<"use_test_dc">>, false}
    ],

    Config = lists:foldl(
        fun({Key, Val}, Acc) ->
            Key_ =
                case Key of
                    _ when is_atom(Key) -> atom_to_binary(Key, utf8);
                    _ when is_list(Key) -> list_to_binary(Key);
                    _ when is_binary(Key) -> Key
                end,
            lists:keystore(Key_, 1, Acc, {Key_, Val})
        end,
        DefaultConfig,
        Cfg
    ),

    Request = method(setTdlibParameters, Config),

    send(self(), Request),

    {reply, Cfg, State#state{config = Config}};
handle_call({execute, Data}, _From, State = #state{tdlib = Tdlib}) ->
    Resp = tdlib_nif:execute(Tdlib, Data),
    {reply, Resp, State};
handle_call(
    {send_sync, Request, Timeout},
    From,
    State = #state{extra = Extra, extra_index = Index}
) ->
    RequestWithExtra = [{<<"@extra">>, Index} | Request],
    NewIndex = Index + 1,

    {ok, TRef} = timer:apply_after(Timeout, ?MODULE, remove_extra, [self(), Index]),
    NewExtra = maps:put(Index, {From, TRef}, Extra),
    send(self(), RequestWithExtra),
    {noreply, State#state{extra = NewExtra, extra_index = NewIndex}};
handle_call({register_handler, Handler}, _From, State = #state{handlers = Handlers}) ->
    NewHandlers = sets:add_element(Handler, Handlers),
    {reply, ok, State#state{handlers = NewHandlers}};
handle_call(get_handlers, _From, State = #state{handlers = Handlers}) ->
    {reply, Handlers, State};
handle_call(get_auth_state, _From, State = #state{auth_state = AuthState}) ->
    {reply, AuthState, State};
handle_call(get_config, _From, State = #state{config = Config}) ->
    {reply, Config, State};
handle_call(get_version, _From, State = #state{tdlib_version = Version, tdlib_commit = Commit}) ->
    {reply, #{version => Version, commit => Commit}, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(send_config, State = #state{config = Config}) when Config /= null ->
    Parent = self(),
    spawn(fun() -> config(Parent, Config) end),
    {noreply, State};
handle_cast({auth_state, AuthState}, State) ->
    {noreply, State#state{auth_state = AuthState}};
handle_cast({send, Request}, State = #state{tdlib = Tdlib}) ->
    lager:debug("[~p] Sending: ~ts", [self(), Request]),
    tdlib_nif:send(Tdlib, Request),
    {noreply, State};
handle_cast(
    {phone_number, PhoneNumber},
    State = #state{auth_state = <<"authorizationStateWaitPhoneNumber">>}
) ->
    Settings = method(<<"phoneNumberAuthenticationSettings">>,
                      [{<<"allow_flash_call">>, false},
                       {<<"is_current_phone_number">>, false},
                       {<<"allow_sms_retriever_api">>, false}]),
    send(
      self(),
      method(
        <<"setAuthenticationPhoneNumber">>,
        [
         {<<"phone_number">>, PhoneNumber},
         {<<"settings">>, Settings}
        ]
       )
     ),
    {noreply, State};
handle_cast({auth_code, Code}, State = #state{auth_state = <<"authorizationStateWaitCode">>}) ->
    send(self(), method(<<"checkAuthenticationCode">>, [{<<"code">>, Code}])),
    {noreply, State};
handle_cast(
    {auth_password, Password},
    State = #state{auth_state = <<"authorizationStateWaitPassword">>}
) ->
    send(self(), method(<<"checkAuthenticationPassword">>, [{<<"password">>, Password}])),
    {noreply, State};
handle_cast({remove_extra, Index}, State = #state{extra = Extra}) ->
    NewExtra = maps:remove(Index, Extra),
    {noreply, State#state{extra = NewExtra}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @private
terminate(_, _) -> ok.

%%====================================================================
%% helpers
%%====================================================================

%% @private
method(Type, Params) when is_map(Params) ->
    Params#{'@type' => Type};
method(Type, Params) when is_list(Params) ->
    [{'@type', Type} | Params].

%% @private
handle_auth(Pid, Data) ->
    AuthState = maps:get(<<"authorization_state">>, Data),
    AuthStateType = maps:get(<<"@type">>, AuthState),
    set_auth_state(Pid, AuthStateType),
    case AuthStateType of
        <<"authorizationStateWaitTdlibParameters">> ->
            send_config(Pid);
        <<"authorizationStateWaitEncryptionKey">> ->
            send(
                Pid,
                method(
                    <<"checkDatabaseEncryptionKey">>,
                    [{<<"encryption_key">>, null}]
                )
            );
        <<"setAuthenticationPhoneNumber">> ->
            lager:info("[~p] Waiting for phone number", [self()]);
        <<"checkAuthenticationCode">> ->
            lager:info("[~p] Waiting for authentication code", [self()]);
        <<"checkAuthenticationPassword">> ->
            lager:info("[~p] Waiting for password", [self()]);
        _ ->
            ok
    end.

%% @private
set_auth_state(Pid, AuthStateType) ->
    lager:info("[~p] setting auth state to ~p", [Pid, AuthStateType]),
    gen_server:cast(Pid, {auth_state, AuthStateType}).

%% @private
send_config(Pid) ->
    gen_server:cast(Pid, send_config).

%% @private
update_option(State, <<"version">>, Version) ->
    State#state{tdlib_version = Version};
update_option(State, <<"commit_hash">>, Commit) ->
    State#state{tdlib_commit = Commit};
update_option(State, _, _) ->
    State.

%% @private
create_config(null) ->
    null;
create_config(Cfg) when is_map(Cfg) ->
    create_config(maps:to_list(Cfg));
create_config(Cfg) ->
    DefaultConfig = [
        {<<"application_version">>, <<"Unknown">>},
        {<<"device_model">>, <<"Unknown">>},
        {<<"enable_storage_optimizer">>, true},
        {<<"files_directory">>, null},
        {<<"ignore_file_names">>, true},
        {<<"system_language_code">>, <<"en">>},
        {<<"system_version">>, null},
        {<<"use_chat_info_database">>, true},
        {<<"use_file_database">>, true},
        {<<"use_message_database">>, true},
        {<<"use_secret_chats">>, false},
        {<<"use_test_dc">>, false}
    ],

    Config = lists:foldl(
               fun({Key, Val}, Acc) ->
                       Key_ =
                       case Key of
                           _ when is_atom(Key) -> atom_to_binary(Key, utf8);
                           _ when is_list(Key) -> list_to_binary(Key);
                           _ when is_binary(Key) -> Key
                       end,
                       lists:keystore(Key_, 1, Acc, {Key_, Val})
               end,
               DefaultConfig,
               Cfg
              ),

    lists:foreach(
      fun(Name) ->
              case lists:keyfind(Name, 1, Config) of
                  false -> throw({missing_param, api_id});
                  _ -> ok
              end
      end, [<<"api_id">>, <<"api_hash">>, <<"database_directory">>]),

    Config.
