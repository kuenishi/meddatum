#!/bin/sh

HOST=$1

set -e

echo checking yz works or not...
echo "http_proxy="$http_proxy
echo "https_proxy="$https_proxy
echo "HTTP_PROXY"=$HTTP_PROXY
echo "HTTPS_PROXY"=$HTTPS_PROXY

echo creating index "test_index" ...
curl -i -X PUT http://${HOST}/yz/index/test_index

echo setting index to test_bucket ...
R=`curl -i -X PUT http://${HOST}/buckets/test_bucket/props \
  -H 'Content-type:application/json' \
  -d '{"props":{"yz_index":"test_index"}}' | head -n 1`
echo $R

R=`curl http://${HOST}/buckets/test_bucket/props`
echo $R

sleep 1

echo putting test data...
R=`curl -i -X PUT http://${HOST}/buckets/test_bucket/keys/test \
  -H 'Content-type:application/json' \
  -d '{"props":{"yz_index":"test_index"}}' | head -n 1`
echo $R

sleep 1

echo trying search ...
R=`curl "http://${HOST}/search/test_index?q=*:*&wt=json"`
echo $R

curl -X DELETE http://${HOST}/buckets/test_bucket/keys/test
curl -X DELETE http://${HOST}/buckets/test_bucket/props \
  -H 'Content-type:application/json' \
  -d '{"props":{"yz_index":"test_index"}}'
curl -X DELETE http://${HOST}/yz/index/test_index

echo "\nAll tests ran successfully."
