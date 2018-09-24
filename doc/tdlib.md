

# Module tdlib #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#auth_code-2">auth_code/2</a></td><td>Send authentication code.</td></tr><tr><td valign="top"><a href="#auth_password-2">auth_password/2</a></td><td>Send password.</td></tr><tr><td valign="top"><a href="#config-2">config/2</a></td><td>Send tdlib configuration.</td></tr><tr><td valign="top"><a href="#execute-2">execute/2</a></td><td>Execute synchronous tdlib request.</td></tr><tr><td valign="top"><a href="#get_auth_state-1">get_auth_state/1</a></td><td>Get current auth state of tdlib.</td></tr><tr><td valign="top"><a href="#get_config-1">get_config/1</a></td><td>Get tdlib configuration.</td></tr><tr><td valign="top"><a href="#get_handlers-1">get_handlers/1</a></td><td>Get list of current handlers.</td></tr><tr><td valign="top"><a href="#phone_number-2">phone_number/2</a></td><td>Send phone number.</td></tr><tr><td valign="top"><a href="#register_handler-2">register_handler/2</a></td><td>Add handler to tdlib instance.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send tdlib request.</td></tr><tr><td valign="top"><a href="#set_log_file_path-0">set_log_file_path/0</a></td><td>Set log logging behaviour to default.</td></tr><tr><td valign="top"><a href="#set_log_file_path-1">set_log_file_path/1</a></td><td>Set log file path.</td></tr><tr><td valign="top"><a href="#set_log_max_file_size-1">set_log_max_file_size/1</a></td><td>Set max log file size.</td></tr><tr><td valign="top"><a href="#set_log_verbosity_level-1">set_log_verbosity_level/1</a></td><td>Set tdlib log verbosity level.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start new tdlib instance.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Start new tdlib instance, register it and send config when ready.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="auth_code-2"></a>

### auth_code/2 ###

`auth_code(Pid, Code) -> any()`

Send authentication code.

<a name="auth_password-2"></a>

### auth_password/2 ###

`auth_password(Pid, Password) -> any()`

Send password.

<a name="config-2"></a>

### config/2 ###

`config(Pid, Cfg) -> any()`

Send tdlib configuration.

<a name="execute-2"></a>

### execute/2 ###

`execute(Pid, Request) -> any()`

Execute synchronous tdlib request.

<a name="get_auth_state-1"></a>

### get_auth_state/1 ###

`get_auth_state(Pid) -> any()`

Get current auth state of tdlib.

<a name="get_config-1"></a>

### get_config/1 ###

`get_config(Pid) -> any()`

Get tdlib configuration.

<a name="get_handlers-1"></a>

### get_handlers/1 ###

`get_handlers(Pid) -> any()`

Get list of current handlers.

<a name="phone_number-2"></a>

### phone_number/2 ###

`phone_number(Pid, PhoneNumber) -> any()`

Send phone number.

<a name="register_handler-2"></a>

### register_handler/2 ###

`register_handler(Pid, Handler) -> any()`

Add handler to tdlib instance.

<a name="send-2"></a>

### send/2 ###

`send(Pid, Request) -> any()`

Send tdlib request.

<a name="set_log_file_path-0"></a>

### set_log_file_path/0 ###

`set_log_file_path() -> any()`

Set log logging behaviour to default.

By default TDLib writes logs to stderr or an OS specific log. Use this
method restore such behaviour.

__See also:__ [set_log_file_path/1](#set_log_file_path-1).

<a name="set_log_file_path-1"></a>

### set_log_file_path/1 ###

`set_log_file_path(Path) -> any()`

Set log file path.

Sets the path to the file where the internal TDLib log will be written.
By default TDLib writes logs to stderr or an OS specific log. Use this
method to write the log to a file instead.

__See also:__ [set_log_file_path/0](#set_log_file_path-0).

<a name="set_log_max_file_size-1"></a>

### set_log_max_file_size/1 ###

`set_log_max_file_size(Size) -> any()`

Set max log file size.

Sets maximum size of the file to where the internal TDLib log is
written before the file will be auto-rotated. Unused if log is not
written to a file. Defaults to 10 MB.

<a name="set_log_verbosity_level-1"></a>

### set_log_verbosity_level/1 ###

`set_log_verbosity_level(Level) -> any()`

Set tdlib log verbosity level.

Sets the verbosity level of the internal logging of TDLib. By default
the TDLib uses a log verbosity level of 5.

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

Start new tdlib instance.

<a name="start_link-2"></a>

### start_link/2 ###

`start_link(Name, Config) -> any()`

Start new tdlib instance, register it and send config when ready.

__See also:__ [config/2](#config-2).

