

# Module tdlib #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#auth_code-2">auth_code/2</a></td><td>Send authentication code.</td></tr><tr><td valign="top"><a href="#auth_password-2">auth_password/2</a></td><td>Send password.</td></tr><tr><td valign="top"><a href="#config-2">config/2</a></td><td>Send tdlib configuration.</td></tr><tr><td valign="top"><a href="#execute-2">execute/2</a></td><td>Execute synchronous tdlib request.</td></tr><tr><td valign="top"><a href="#phone_number-2">phone_number/2</a></td><td>Send phone number.</td></tr><tr><td valign="top"><a href="#register_handler-2">register_handler/2</a></td><td>Add handler to tdlib instance.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send tdlib request.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start new tdlib instance.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Start new tdlib instance and register it.</td></tr></table>


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

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

Start new tdlib instance.

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Name) -> any()`

Start new tdlib instance and register it.
====================================================================

