-module(tdlib).

-behaviour(gen_server).

-export([start_link/0]).
-export([handle_call/3, handle_cast/2, init/1, handle_info/2, code_change/3, terminate/2]).
-export([register_handler/2, config/2, send/2]).

-record(state, {tdlib = null, handlers = [], config_set = false}).

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


%%====================================================================
%% callbacks
%%====================================================================

init([]) ->
  self() ! init,
  {ok, #state{}}.


handle_info(init, State) ->
  {ok, Tdlib} = tdlib_nif:new(),
  {noreply, State#state{tdlib = Tdlib}};

handle_info(_Msg, State) ->
  {noreply, State}.


handle_call({config, Cfg}, _From, State=#state{config_set = false}) ->
  DefaultConfig =
    [ {application_version, <<"Unknown">>},
      {device_model, <<"Unknown">>},
      {enable_storage_optimizer, true},
      {files_directory, null},
      {ignore_file_names, true},
      {system_language_code, <<"en">>},
      {system_version, <<"Unknown">>},
      {use_chat_info_database, true},
      {use_file_database, true},
      {use_message_database, true},
      {use_secret_chats, false},
      {use_test_dc, false} ],

  Config = lists:foldl(fun({Key, Val}, Acc) ->
                           lists:keyreplace(Key, 1, Acc, {Key, Val})
                       end,
                       DefaultConfig, Cfg),

  Request = method(setTdlibParameters, [{parameters, Config}]),

  send(self(), Request),

  {reply, Cfg, State};

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.


handle_cast({send, Request}, State=#state{tdlib = Tdlib}) ->
  tdlib_nif:send(Tdlib, Request),
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
