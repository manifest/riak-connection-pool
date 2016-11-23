# Riak connection pool

[![Build Status][travis-img]][travis]



### How To Use

Build and run the docker container with Riak KV within it.

```bash
$ ./run-docker.sh
```

To build and start playing with the library, execute following commands in the shell:

```bash
$ make app shell
```

Here is a minimal example:

```erlang
%% Creating a pool and adding it to the supervision tree.
Pool =
  #{name => default,
    size => 5,
    connection =>
      #{host => "192.168.99.100",
        port => 8087,
        options => [queue_if_disconnected]}},
ChildSpec = riakc_pool:child_spec(Pool),
supervisor:start_child(whereis(riakc_pool_sup), ChildSpec).

%% Getting a connection and locking it to the current process.
%% If process dies, connection will be released.
Pid = riakc_pool:lock(default),
%% Putting an object and getting it back.
riakc_pb_socket:put(Pid, riakc_obj:new(<<"groceries">>, <<"mine">>, <<"eggs & bacon">>)),
riakc_pb_socket:get(Pid, <<"groceries">>, <<"mine">>),
%% Releasing the connection.
riakc_pool:unlock(default, Pid).
```

To learn more about Riak client library refer to its [documentation][riakc-docs].



### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/riak-connection-pool?branch=master
[travis-img]:https://secure.travis-ci.org/manifest/riak-connection-pool.png
[riakc-docs]:https://github.com/basho/riak-erlang-client
