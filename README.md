# Riak connection pool

[![Build Status][travis-img]][travis]



### How To Use

Build and run the docker container

```bash
$ ./run-docker.sh
```

Execute following commands in container's shell:

```bash
$ make app shell
```

Starting a pool:

```erlang
Pool =
  #{name => default,
    size => 10,
    connection =>
      #{host => "localhost",
        port => 8087,
        options => [queue_if_disconnected]}},
ChildSpec = riakc_pool:child_spec(Pool),
supervisor:start_child(whereis(riakc_pool_sup), ChildSpec).
```

Executing queries:

```erlang
riakc_pool:query(default, put, [riakc_obj:new(<<"groceries">>, <<"mine">>, <<"eggs & bacon">>)]).
riakc_pool:query(default, get, [<<"groceries">>, <<"mine">>]).
%% {ok,{riakc_obj,<<"groceries">>,<<"mine">>,
%%                <<107,206,97,96,96,96,204,96,202,5,82,60,147,164,15,105,
%%                  123,155,237,13,129,8,37,...>>,
%%                [{{dict,2,16,16,8,80,48,
%%                        {[],[],[],[],[],[],[],[],[],[],[],[],...},
%%                        {{[],[],[],[],[],[],[],[],[],[],...}}},
%%                  <<"eggs & bacon">>}],
%%                undefined,undefined}}
```



### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/riak-connection-pool?branch=master
[travis-img]:https://secure.travis-ci.org/manifest/riak-connection-pool.png

