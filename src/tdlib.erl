-module(tdlib).

-behaviour(gen_server).

-export([start_link/0]).
-export([handle_call/3, handle_cast/2, init/1, handle_info/2, code_change/3, terminate/2]).
-export([register_handler/2, config/2, send/2, execute/2, method/2]).
-export([phone_number/2, auth_code/2, auth_password/2]).

-define(RECEIVE_TIMEOUT, 5.0).

-record(state, {tdlib = null, handlers = [], auth_state = null}).

%%====================================================================
%% API functions
%%====================================================================


start_link() ->
  gen_server:start_link(?MODULE, [], []).


register_handler(Pid, Handler) ->
  gen_server:call(Pid, {register_handler, Handler}).


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


send(Pid, Data) when is_binary(Data) ->
  gen_server:cast(Pid, {send, Data});

send(Pid, Data) ->
  Msg = jsx:encode(Data),
  send(Pid, Msg).


execute(Pid, Data) when is_binary(Data) ->
  gen_server:call(Pid, {execute, Data});

execute(Pid, Data) ->
  Msg = jsx:encode(Data),
  execute(Pid, Msg).


phone_number(Pid, PhoneNumber) when is_binary(PhoneNumber) ->
  gen_server:cast(Pid, {phone_number, PhoneNumber}).

auth_code(Pid, Code) when is_binary(Code) ->
  gen_server:cast(Pid, {auth_code, Code}).

auth_password(Pid, Password) when is_binary(Password) ->
  gen_server:cast(Pid, {auth_password, Password}).


%%====================================================================
%% callbacks
%%====================================================================

init([]) ->
  self() ! init,
  {ok, #state{}}.


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
                  _ ->
                    lists:foreach(
                      fun(Handler) -> Handler ! {incoming, Data} end,
                      Handlers )
                end;
              null -> ok
            end,
            Parent ! poll
        end),
  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.


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


code_change(_OldVsn, State, _Extra) -> { ok, State }.


terminate(_, _) -> ok.

%%====================================================================
%% helpers
%%====================================================================

method(Type, Params) ->
    [{'@type', Type} | Params].

handle_auth(Pid, Data) ->
  {_, AuthState} = lists:keyfind(<<"authorization_state">>, 1, Data),
  {_, AuthStateType} = lists:keyfind(<<"@type">>, 1, AuthState),
  set_auth_state(Pid, AuthStateType),
  case AuthStateType of
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

set_auth_state(Pid, AuthStateType) ->
  gen_server:cast(Pid, {auth_state, AuthStateType}).
