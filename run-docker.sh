#!/bin/bash

RIAKC_POOL='/opt/sandbox/riak-connection-pool'

read -r DOCKER_RUN_COMMAND <<-EOF
	riak start \
	&& riak-admin wait-for-service riak_kv
EOF

docker build -t sandbox/riak-connection-pool .
docker run -ti --rm \
	-v $(pwd):${RIAKC_POOL} \
	-p 8098:8098 \
	-p 8087:8087 \
	sandbox/riak-connection-pool \
	/bin/bash -c "${DOCKER_RUN_COMMAND} && cd ${RIAKC_POOL} && /bin/bash"
