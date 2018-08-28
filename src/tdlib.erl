-module(tdlib).

-behaviour(gen_server).

-export([start_link/0, start_link/2]).
-export([handle_call/3, handle_cast/2, init/1, handle_info/2, code_change/3, terminate/2]).
-export([register_handler/2, config/2, send/2, execute/2, method/2]).
-export([phone_number/2, auth_code/2, auth_password/2]).

-define(RECEIVE_TIMEOUT, 5.0).

-record(state, {tdlib = null, handlers = [], auth_state = null, config = null}).

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
    case lists:keyfind(api_id, 1, Cfg) of
      false -> throw({missing_param, api_id});
      _ -> ok
    end,

    case lists:keyfind(api_hash, 1, Cfg) of
      false -> throw({missing_param, api_hash});
      _ -> ok
    end,

    case lists:keyfind(database_directory, 1, Cfg) of
      false -> throw({missing_param, database_directory});
      _ -> ok
    end,

    gen_server:call(Pid, {config, Cfg})
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
  gen_server:cast(Pid, {auth_code, Code}).

%%====================================================================
%% @doc Send password.
%%
%% @param Pid tdlib gen_server pid
%% @param Password should be binary.
%%====================================================================
auth_password(Pid, Password) when is_binary(Password) ->
  gen_server:cast(Pid, {auth_password, Password}).


%%====================================================================
%% callbacks
%%====================================================================

%% @private
init(Config) ->
  self() ! init,
  {ok, #state{config = Config}}.


%% @private
handle_info(init, State) ->
  {ok, Tdlib} = tdlib_nif:new(),
  self() ! poll,
  {noreply, State#state{tdlib = Tdlib}};

handle_info(poll, State=#state{tdlib = Tdlib, handlers = Handlers}) ->
  Parent = self(),
  spawn(fun() ->
            Resp = tdlib_nif:recv(Tdlib, ?RECEIVE_TIMEOUT),
            io:format("recv: ~p~n", [Resp]), %% XXX debug
            case Resp of
              {ok, Msg} ->
                Data = jsx:decode(Msg),

                case lists:keyfind(<<"@type">>, 1, Data) of
                  {_, <<"updateAuthorizationState">>} ->
                    handle_auth(Parent, Data);
                  _ -> ok
                end,

                lists:foreach(
                  fun(Handler) -> Handler ! {incoming, Data} end,
                  Handlers );

              null -> ok
            end,
            Parent ! poll
        end),
  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.


%% @private
handle_call({config, Cfg}, _From, State=#state{auth_state = <<"authorizationStateWaitTdlibParameters">>}) ->
  DefaultConfig =
    [ {<<"application_version">>, <<"Unknown">>},
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
      {<<"use_test_dc">>, false} ],

  Config = lists:foldl(fun({Key, Val}, Acc) ->
                           Key_ = case Key of
                                    _ when is_atom(Key) -> atom_to_binary(Key, utf8);
                                    _ when is_list(Key) -> list_to_binary(Key);
                                    _ when is_binary(Key) -> Key
                                  end,
                           lists:keystore(Key_, 1, Acc, {Key_, Val})
                       end,
                       DefaultConfig, Cfg),

  Request = method(setTdlibParameters, [{parameters, Config}]),

  send(self(), Request),

  {reply, Cfg, State};

handle_call({execute, Data}, _From, State=#state{tdlib = Tdlib}) ->
  Resp = tdlib_nif:execute(Tdlib, Data),
  {reply, Resp, State};

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.


%% @private
handle_cast(send_config, State=#state{config = Config}) when Config /= null ->
  Parent = self(),
  spawn(fun() -> config(Parent, Config) end),
  {noreply, State};

handle_cast({auth_state, AuthState}, State) ->
  {noreply, State#state{auth_state = AuthState}};

handle_cast({send, Request}, State=#state{tdlib = Tdlib}) ->
  tdlib_nif:send(Tdlib, Request),
  {noreply, State};

handle_cast({phone_number, PhoneNumber}, State=#state{auth_state = <<"authorizationStateWaitPhoneNumber">>}) ->
  send(self(), method(<<"setAuthenticationPhoneNumber">>,
                      [{<<"phone_number">>, PhoneNumber},
                       {<<"allow_flash_call">>, false}])),
  {noreply, State};

handle_cast({auth_code, Code}, State=#state{auth_state = <<"authorizationStateWaitCode">>}) ->
  send(self(), method(<<"checkAuthenticationCode">>, [{<<"code">>, Code}])),
  {noreply, State};

handle_cast({auth_password, Password}, State=#state{auth_state = <<"authorizationStateWaitPassword">>}) ->
  send(self(), method(<<"checkAuthenticationPassword">>, [{<<"password">>, Password}])),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


%% @private
code_change(_OldVsn, State, _Extra) -> { ok, State }.

%% @private
terminate(_, _) -> ok.

%%====================================================================
%% helpers
%%====================================================================

%% @private
method(Type, Params) ->
    [{'@type', Type} | Params].

%% @private
handle_auth(Pid, Data) ->
  {_, AuthState} = lists:keyfind(<<"authorization_state">>, 1, Data),
  {_, AuthStateType} = lists:keyfind(<<"@type">>, 1, AuthState),
  set_auth_state(Pid, AuthStateType),
  case AuthStateType of
    <<"authorizationStateWaitTdlibParameters">> ->
      send_config(Pid);

    <<"authorizationStateWaitEncryptionKey">> ->
      send(Pid, method(<<"checkDatabaseEncryptionKey">>,
                       [{<<"encryption_key">>, null}]));

    <<"setAuthenticationPhoneNumber">> ->
      io:format("Waiting for phone number~n");

    <<"checkAuthenticationCode">> ->
      io:format("Waiiting for authentication code~n");

    <<"checkAuthenticationPassword">> ->
      io:format("Waiiting for password~n");

    _ -> ok
  end.

%% @private
set_auth_state(Pid, AuthStateType) ->
  gen_server:cast(Pid, {auth_state, AuthStateType}).

%% @private
send_config(Pid) ->
  gen_server:cast(Pid, send_config).
