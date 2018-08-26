-module(tdlib_nif).

%% API exports
-export([new/0, send/2, execute/2, recv/2]).

-on_load(init/0).

init() ->
  PrivDir = code:priv_dir(tdlib), %% TODO use some module argument
  File = filename:join([PrivDir, "crates", "tdlib-nif", "libtdlib_nif.so"]),

  erlang:load_nif(filename:rootname(File), 0).

%%====================================================================
%% API functions
%%====================================================================

new() ->
  exit(nif_lirary_not_loaded).

send(_, _) ->
  exit(nif_lirary_not_loaded).

execute(_, _) ->
  exit(nif_lirary_not_loaded).

recv(_, _) ->
  exit(nif_lirary_not_loaded).

%%====================================================================
%% Internal functions
%%====================================================================
