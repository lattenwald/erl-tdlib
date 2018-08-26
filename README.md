tdlib
=====

Erlang library interfacing with [tdlib](https://github.com/tdlib/td), a telegram client library created by Telegram.

Build
-----

Have `libtdjson.so` from [tdlib](https://github.com/tdlib/td) installed in your library path.

Have [Rust](https://rustup.rs/) installed.

Have [rebar3](https://www.rebar3.org/) installed.

Clone, `cd` to repository and compile:

    $ rebar3 get-deps
    $ rebar3 compile


Usage
-----

Fetch `app_id` and `app_hash` at https://my.telegram.org/apps

There we will consider use following placeholders:

 * `app_id` : `34567`
 * `app_hash` : `qwertyasdfgh123456qweytr`
 * telegram account phone number: `+1234567890`
 * database directory (will be created by tdlib when needed) : `tdlib_db`
 * code Telegram sends you after sending them phone number : `23456`
 * password : `pAssWorD..1`

Example session:

    $ rebar3 shell

    1> {ok, Pid} = tdlib:start_link().
	2> tdlib:config(Pid, [{api_id, <<"34567">>}, {api_hash, <<"qwertyasdfgh123456qweytr">>}, {database_directory, <<"tdlib_db">>}]).
	3> tdlib:phone_number(Pid, <<"+1234567890">>).
	4> tdlib:auth_code(Pid, <<"23456">>).
	5> tdlib:auth_password(Pid, <<"pAssWorD..1">>).

When authenticated, next run with same db directory will authenticate automatically, you don't need to enter phone, code or password.
