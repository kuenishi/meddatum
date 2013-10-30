# meddatum
==========

hl7/e-rezept parser and importer to Riak database with search tools.

Erlang/OTP library to handle [SS-MIX](http://www.hci-bc.com/ss-mix/ssmix/) data (Japanese local standard including HL7 format) and [electric rezepts](http://www.ssk.or.jp/rezept/). This toolkit includes:

- [x] SS-MIXed HL7 parser, converter and importer to Riak
- [x] electric rezept parser, converter and importer to Riak
- [x] backend query system to be installed into Riak cluster
- [x] forntend query tools
- [ ] pluggable to other datastore like Mongo,MySQL,Hive,Solr,??

This stores parsed JSON data into `/buckets/rezept` and `/buckets/ssmix`

# Prequisites

- NKF command should be in path
- run Riak with Yokozuna enabled somewhere [install](https://github.com/basho/yokozuna/blob/master/docs/INSTALL.md)
- make, gcc, erlang > 15B01 (download Erlang from [here](http://erlang-users.jp) )
- rezept file's csv must have LF ('\n', or CRLF) at each end of line

# Compile

```
$ make
```

# How to use

**Riak must listen on 8098 for http and on 8087 for pb.**

## upload SS-MIX docs to Riak

```sh
$ ERL_LIBS=deps rel/hl7parser path/to/ssmixroot
```

TODO: set Riak place configurable...

## upload e-rezept docs to Riak

```sh
$ ERL_LIBS=deps rel/rezeptparser path/to/11_REZEPTINFO_MED.CSV
```

## search docs in Riak

```sh
$ ERL_LIBS=deps rel/search localhost "*:*"
```
The search query is compatible with Solr [link](http://lucene.apache.org/core/2_9_4/queryparsersyntax.html). This command output the main json data to STDOUT, so typical usage is:

```
$ export ERL_LIBS=deps
$ rel/search localhost "*:*" > data.json
```
note: the JSON is just a concatination of several JSON Objects. Take care when parsing.

# TODO

- document data conversion spec (original -> JSON)
- do we store original data? (or just point to the file)
- test more on real data
- parallel push
- apply node_package installation scheme?

# misc

- [JAHIS standards](http://www.jahis.jp/jahis_hyojyun/seiteizumi_hyojyun/)
- hope this helps [SS-MIX open source project](http://iryogakusei.com/portfolio-item/ss-mix%E3%82%AA%E3%83%BC%E3%83%97%E3%83%B3%E3%82%BD%E3%83%BC%E3%82%B9%E5%8C%96/)


# license

Apache 2.0, see LICENCE file.
