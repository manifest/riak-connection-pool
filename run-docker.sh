#!/bin/bash

PROJECT='riak-connection-pool'
PROJECT_DIR="/opt/sandbox/${PROJECT}"
DOCKER_CONTAINER_NAME="sandbox/${PROJECT}"
DOCKER_CONTAINER_COMMAND=${DOCKER_CONTAINER_COMMAND:-'/bin/bash'}
ULIMIT_FD=262144

read -r DOCKER_RUN_COMMAND <<-EOF
	service rsyslog start \
	&& riak start \
	&& riak-admin wait-for-service riak_kv
EOF

docker build -t ${DOCKER_CONTAINER_NAME} .
docker run -ti --rm \
	-v $(pwd):${PROJECT_DIR} \
	--ulimit nofile=${ULIMIT_FD}:${ULIMIT_FD} \
	-p 8098:8098 \
	-p 8087:8087 \
	-p 8093:8093 \
	-p 8985:8985 \
	${DOCKER_CONTAINER_NAME} \
	/bin/bash -c "set -x && cd ${PROJECT_DIR} && ${DOCKER_RUN_COMMAND} && set +x && ${DOCKER_CONTAINER_COMMAND}"
