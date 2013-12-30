#!/bin/sh

set -e

# export RIAK_PATH=rel/riak/bin
HOST=$1

${RIAK_PATH}riak-admin bucket-type list
${RIAK_PATH}riak-admin bucket-type create md \
    '{"props":{"search_index":"md_index"}}'
${RIAK_PATH}riak-admin bucket-type activate md
${RIAK_PATH}riak-admin bucket-type list

curl -i "http://${HOST}/types/md/props"
curl -i "http://${HOST}/search/index/md_index?wt=json&q=*:*"
