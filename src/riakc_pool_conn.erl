%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(riakc_pool_conn).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([
	query/3
]).

%% Poolboy callbacks
-export([
	start_link/1
]).

%% Gen Server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% Types
-record(state, {
	conn :: pid()
}).

%% =============================================================================
%% API
%% =============================================================================

-spec query(pid(), function(), list()) -> any().
query(Pid, Fun, Args) ->
	gen_server:call(Pid, {query, Fun, Args}, infinity).

%% =============================================================================
%% Poolboy callbacks
%% =============================================================================

-spec start_link(list()) -> {ok, pid()} | {error, {already_started, pid()} | any()}.
start_link(Conn) ->
	proc_lib:start_link(?MODULE, init, [{self(), Conn}]).

%% =============================================================================
%% Gen Server callbacks
%% =============================================================================

init({Parent, Conn}) ->
	process_flag(trap_exit, true),
	case catch handle_connect(Conn) of
		{ok, Pid} ->
			proc_lib:init_ack(Parent, {ok, self()}),
			gen_server:enter_loop(?MODULE, [], #state{conn = Pid});
		{error, Reason} ->
			proc_lib:init_ack(Parent, {error, Reason}),
			error(Reason);
		{'EXIT', Reason} ->
			proc_lib:init_ack(Parent, {error, Reason}),
			error(Reason);
		Else ->
			Reason = {bad_return_value, Else},
			proc_lib:init_ack(Parent, {error, Reason}),
			exit(Reason)
	end.

handle_call({query, Fun, Args}, _From, #state{conn = Conn} = State) ->
	{reply, handle_query(Conn, Fun, Args), State};
handle_call(_Req, _From, State) ->
	{noreply, State}.

handle_cast(_Req, State) ->
	{noreply, State}.

handle_info({'EXIT', _Pid, Reason}, _State) ->
	error_logger:error_report([{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, {disconnected, Reason}]),
	exit(Reason);
handle_info(_Req, State) ->
	{noreply, State}.

terminate(_Reason, #state{conn = Pid}) ->
	riakc_pb_socket:stop(Pid).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec validate_host(map()) -> list().
validate_host(#{host := Val}) when is_list(Val) -> Val;
validate_host(#{host := Val})                   -> throw({invalid_host, Val});
validate_host(_)                                -> throw(missing_host).

-spec validate_port(map()) -> non_neg_integer().
validate_port(#{port := Val}) when is_integer(Val), Val > 0 -> Val;
validate_port(#{port := Val})                               -> throw({invalid_port, Val});
validate_port(_)                                            -> throw(missing_port).

-spec handle_connect(map()) -> {ok, pid()}.
handle_connect(Conn) ->
	riakc_pb_socket:start_link(
		validate_host(Conn),
		validate_port(Conn),
		maps:get(options, Conn, [])).

-spec handle_query(pid(), atom(), list()) -> any().
handle_query(Conn, Fn, Args) ->
	apply(riakc_pb_socket, Fn, [Conn|Args]).

