PROJECT = riakc_pool
PROJECT_DESCRIPTION = Riak connection pool
PROJECT_VERSION = 0.1.0

DEPS = \
	poolboy \
	riakc

dep_poolboy = git git://github.com/devinus/poolboy.git 1.5.1
dep_riakc = git https://github.com/basho/riak-erlang-client.git 2.4.2

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start

include erlang.mk
