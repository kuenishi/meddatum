# meddatum
==========

A set of parsers of several medical record data format, combined with importers to Riak database.

Erlang/OTP library to handle [SS-MIX](http://www.hci-bc.com/ss-mix/ssmix/) data (Japanese local standard including HL7 format) and [electric rezepts](http://www.ssk.or.jp/rezept/). This toolkit includes:

- [x] SS-MIXed HL7 parser, converter and importer to Riak
- [x] electric recept parser, converter and importer to Riak
- [x] DPC Survey data format
- [x] backend query system to be installed into Riak cluster
- [x] forntend query tools
- [ ] pluggable to other datastore like Mongo,MySQL,Hive,Solr,??

# Prequisites

- NKF command should be in path
- run Riak with Yokozuna enabled somewhere [install](https://github.com/basho/yokozuna/blob/master/docs/INSTALL.md)
- make, gcc, erlang > R16B02 (download Erlang from [here](http://erlang-users.jp) )
- recept file's csv must have LF ('\n', or CRLF) at each end of line

# Compile

```
$ make
```

# subcommands

```
$ meddatum create-config
$ meddatum check-config
$ meddatum import-ssmix <hospital-id> <path/to/directory>
$ meddatum import-recept <path/to/file>
$ meddatum import-dpcs ...
$ meddatum parse-ssmix <ssmix-file>
$ meddatum parse-recept <recept-file>
$ meddatum parse-dpcs ...
$ meddatum delete-all-ssmix <hospital-id>
$ meddatum delete-recept <recept-file>
$ meddatum [help]
```

# misc

- [JAHIS standards](http://www.jahis.jp/jahis_hyojyun/seiteizumi_hyojyun/)
- [Recept formats](http://www.ssk.or.jp/rezept/iryokikan/iryokikan_02.html)
- [DPC Survey data format](http://www.prrism.com/dpc/setsumei_20140808.pdf)
- hope this helps [SS-MIX open source project](http://iryogakusei.com/portfolio-item/ss-mix%E3%82%AA%E3%83%BC%E3%83%97%E3%83%B3%E3%82%BD%E3%83%BC%E3%82%B9%E5%8C%96/)


# license

Apache 2.0, see LICENCE and AUTHORS file.
