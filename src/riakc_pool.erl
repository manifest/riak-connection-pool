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

-module(riakc_pool).

%% API
-export([
	query/3,
	child_spec/1
]).

%% Definitions
-define(APP, ?MODULE).

%% =============================================================================
%% API
%% =============================================================================

-spec query(atom(), function(), list()) -> any().
query(Pool, Fun, Args) ->
	%% Prevent killing overflow connections
	%% after their maximum is reached
	Pid = poolboy:checkout(Pool, false),
	try riakc_pool_conn:query(Pid, Fun, Args)
	after
		ok = poolboy:checkin(Pool, Pid)
	end.

-spec child_spec(map()) -> supervisor:child_spec().
child_spec(#{name := Name, size := Size, connection := Conn} = M) ->
	poolboy:child_spec(
		Name,
		[ {worker_module, riakc_pool_conn},
			{name, {local, Name}},
			{size, Size},
			{max_overflow, maps:get(max_overflow, M, Size)},
			{strategy, maps:get(strategy, M, lifo)} ],
		Conn).

