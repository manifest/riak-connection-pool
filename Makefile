PROJECT = riakc_pool
PROJECT_DESCRIPTION = Riak connection pool

DEP_PLUGINS = \
	version.mk

DEPS = \
	poolboy \
	riakc

dep_poolboy = git git://github.com/manifest/poolboy.git feature/worker-args-any
dep_riakc = git https://github.com/basho/riak-erlang-client.git 2.5.0

BUILD_DEPS = version.mk
dep_version.mk = git git://github.com/manifest/version.mk.git master

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start

include erlang.mk
