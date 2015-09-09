jhn_stdlib [(γ)][5] [![Build Status](https://secure.travis-ci.org/JanHenryNystrom/jhn_stdlib.png)](http://travis-ci.org/JanHenryNystrom/jhn_stdlib)
==========

A few thought experiments solidified as code.

  * [Introduction](#introduction)
  * [Features/Modules](#features)
  * [Build](#build)
  * [Install](#install)
  * [Contribute](#contribute) - Read if you're planning to submit patches

<a name='introduction'>

Introduction
------------

This library consists mainly of code that arose out of my curiosity, either
about Erlang and coding in general or concerning a pericular protocol or
technique. So there is little of cohesion in purposes between the different
library modules. But having published these I will continue to support these
since the road to enlightenment is one without terminus.

<a name='features'>

Features/Modules
--------

  * String Processing Functions for binary encoded strings
    * blist -- drop in replacement for the lists module in stdlib
    * bstring -- drop in replacement for the string module in stdlib
  * Protocols
    * Encoding/decoding JSON/Erlang  -- json [rfc4627][6], [rfc7159][7]
    * Encoding/decoding/evaluation JSON Pointer/Erlang  -- json [rfc6901][8]
    * Evaluation JSON Patch -- json [rfc6902][9]
    * Validation JSON schema -- json [draft-zyp-json-schema-04][10]
    * MessagePack -- msgpack [MessagePack][11]/Erlang
  * Standards
    * Encoding/decoding URI/Erlang -- uri [rfc3986][12]
  * Pull oriented data source abstraction
    * lazy -- abstracts different data sources as uniform lazy data
  * Data structures
    * Property lists -- plist

<a name='build'>

Build
-----

meck requires [rebar][1] to build, but provides make support to download and
install rebar. To build jhn_stdlib, go to the jhn_stdlib directory and type:

```sh
make
```

To make sure jhn_stdlib works on your platform, run the tests:

```sh
make test
```

Two things might seem alarming when running the tests:

  1. Warnings emitted by cover
  2. En exception printed by SASL

Both are expected due to the way Erlang currently prints errors. The
important line you should look for is `All XX tests passed`, if that
appears all is correct.


<a name='install'>

Install
-------

If you want to install your own built version of jhn_stdlib add the ebin
directory to your Erlang code path or move the jhn_stdlib folder into your
release folder and make sure that folder is in your `ERL_LIBS`
environment variable.


<a name='contribute'>

Contribute
----------

Should you find yourself using jhn_stdlib and have issues, comments or
feedback please [create an issue here on GitHub.] [2]

Patches are greatly appreciated, but since these libraries reflect my
learning process and I have rather peculiar notions of code hygiene
I may do extensive rewrites that does not in any way diminish the
appreciation I feel or indeed [express.] [3]

For a much nicer history, please [write good commit messages][4].
I know I really should.

  [1]: http://github.com/rebar/rebar
       "Rebar - A build tool for Erlang"
  [2]: http://github.com/JanHenryNystrom/jhn_stdlib/issues
       "jhn_stdlib issues"
  [3]: http://github.com/JanHenryNystrom/jhn_stdlib/blob/master/THANKS
       "thanks"
  [4]: http://github.com/erlang/otp/wiki/Writing-good-commit-messages
       "Erlang/OTP commit messages"
  [5]: http://en.wikipedia.org/wiki/Software_release_life_cycle
       "Software release life cycle"
  [6]: http://www.ietf.org/rfc/rfc4627.txt
       "The application/json Media Type for JavaScript Object Notation (JSON)"
  [7]: http://www.ietf.org/rfc/rfc7159.txt
       "The JavaScript Object Notation (JSON) Data Interchange Format"
  [8]: http://www.ietf.org/rfc/rfc6901.txt
       "JavaScript Object Notation (JSON) Pointer"
  [9]: http://www.ietf.org/rfc/rfc6902.txt
       "JavaScript Object Notation (JSON) Patch"
  [10]: http://tools.ietf.org/id/draft-zyp-json-schema-04.txt
       "JSON Schema: core definitions and terminology"
  [11]: http://msgpack.org/
       "An efficient binary serialization format"
  [12]: http://www.ietf.org/rfc3986.txt
       "Uniform Resource Identifier (URI): Generic Syntax"

