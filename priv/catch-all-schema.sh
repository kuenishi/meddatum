#!/bin/sh

set -e

DST=src/meddatum_catch_all_schema.erl

cat <<EOF > $DST
-module(meddatum_catch_all_schema).

-compile(export_all).

binary() ->
EOF

erl -eval 'io:format("~w.", [element(2, file:read_file("priv/catch-all-schema.xml"))])' -noshell -s init stop >> $DST

## Footer
cat <<EOF >> $DST
EOF
