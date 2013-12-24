#!/bin/sh

HOST=$1

echo setting up md_index at $HOST
curl -X PUT http://${HOST}/search/index/md_index
echo "setting relation md_index to ssmix, rezept bucket"
curl -X PUT http://${HOST}/buckets/ssmix/props -H 'Content-type:application/json' -d '{"props":{"yz_index":"md_index"}}'
curl -X PUT http://${HOST}/buckets/rezept/props -H 'Content-type:application/json' -d '{"props":{"yz_index":"md_index"}}'

curl http://${HOST}/search/index?index=true
curl http://${HOST}/buckets/ssmix/props
curl http://${HOST}/buckets/rezept/props
