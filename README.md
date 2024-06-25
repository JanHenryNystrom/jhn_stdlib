jhn_stdlib [(γ)][5]
==========

A few thought experiments solidified as code.

  * [Introduction](#introduction)
  * [Behaviours](#behaviours)
  * [Features/Modules](#features)
  * [Deprecated Features/Modules](#deprecated)
  * [Build](#build)
  * [Install](#install)
  * [Contribute](#contribute) - Read if you're planning to submit patches

<a name='introduction'>

Introduction
------------

This library consists mainly of code that arose out of my curiosity, either
about Erlang and coding in general or concerning a perticular protocol or
technique. So there is little of cohesion in purposes between the different
library modules. But having published these I will continue to support these
since the road to enlightenment is one without terminus.

Behaviours
--------

  * jhn_server -- A generic server
  * jhn_fsm -- A FSM with the ability to defer acting on events until
    a later state

<a name='features'>

Features/Modules
--------

  * String Processing Functions for binary encoded strings
    * jhn_blist -- drop in replacement for the lists module in stdlib
    * jhn_bstring -- drop in replacement for the string module in stdlib
    * jhn_mustache -- [Mustache (template system)][37]
  * Protocols
    * Encoding/decoding JSON/Erlang  -- jhn_json [rfc8259][30]
    * Encoding/decoding/evaluation JSON Pointer/Erlang  -- jhn_json [rfc6901][8]
    * Patch JSON -- jhn_json [rfc6902][31]
    * Merge patch JSON -- jhn_json [rfc7396][32]
    * MessagePack -- jhn_msgpack [MessagePack][12]/Erlang
    * Bencoding -- jhn_bencoding
    * Encoding/decoding Syslog/Erlang -- jhn_syslog [rfc5424][13],
                                                    [rfc5427][14],
                                                    [rfc6012][29]
  * Standards
    * Encoding/decoding URI/Erlang -- jhn_uri [rfc3986][15]
    * Encoding/decoding IP addresses/Erlang -- jhn_ip_addr [rfc4291][16],
                                                           [rfc5952][17],
                                                           [rfc4632][18]
    * Generating/encoding/decoding Timestamps Posix/Erlang -- jhn_timestamp
                                                              [rfc3339][19],
                                                              [rfc7231][21]
  * Clients
    * A simple HTTP client -- jhn_shttpc [rfc7230][20], [rfc7231][21],
                                         [rfc7538][22], [rfc5789][23],
                                         [rfc2818][24]
    * Server/client for Syslog -- jhn_syslog [rfc5425][25], [rfc5426][26],
                                             [rfc6587][27]
  * Pull oriented data source abstraction
    * lazy -- jhn_lazy abstracts different data sources as uniform lazy data
  * Data structures
    * Bloom filters -- jhn_bloom [Bloom Filters][36]
    * Property lists -- jhn_plist
    * Prefix trees -- jhn_p_tree [Prefix Tree][38]
    * Binary(UTF-8) Prefix trees -- jhn_pb_tree [Prefix Tree][38]
                                                [UTF-8][39]
    * Range trees -- jhn_r_tree [Range Tree][40]
    * T-trees -- t_tree [T-tree][41]
  * Algorithms
    * Levenshtein distance -- jhn_math:levenshtein/2 [Levenshtein][35]
    * CRC32-C checksum -- jhn_hash:crc32c/1 [rfc9260][33]
    * xxHash-32 checksum -- jhn_hash:xxh32/1/2 [xxHash][34]
    * Consistent Hashing -- jhn_chash [J. Lamping, Eric Veach][28]

<a name='deprecated'>

Deprecated Features/Modules
--------

  * Replaced
    * json -- use jhn_json
    * jstream -- use jhn_json
    * levenshtein -- use jhn_math:levenshtein/2
  * Renamed
    * bencoding -- use jhn_bencoding
    * blist -- use jhn_blist
    * bloom -- use jhn_bloom
    * bstring -- use jhn_bstring
    * ip_addr -- use jhn_ip_addr
    * lazy -- use jhn_lazy
    * msgpack -- use jhn_msgpack
    * mustache -- use jhn_mustache
    * p_tree -- use jhn_p_tree
    * pb_tree -- use jhn_pb_tree
    * plist -- use jhn_plist
    * r_tree -- use jhn_r_tree
    * shttpc -- use jhn_shttpc
    * syslog -- use jhn_syslog
    * t_tree -- use jhn_t_tree
    * timestamp -- use jhn_timestamp
    * uri -- use jhn_uri
  * Removed
    * Validation JSON schema -- json [draft-zyp-json-schema-04][10],
                                     [draft-fge-json-schema-validation-00][11]

<a name='build'>

Build
-----

jhn_stdlib requires [rebar3][1] to build, but provides make support to download
and install rebar. To build jhn_stdlib, go to the jhn_stdlib directory and type:

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
appreciation I feel or indeed express

For a much nicer history, please [write good commit messages][4].
I know I really should.

  [1]: https://github.com/erlang/rebar3
       "Rebar3 - A build tool for Erlang"
  [2]: http://github.com/JanHenryNystrom/jhn_stdlib/issues
       "jhn_stdlib issues"
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
  [10]: http://tools.ietf.org/id/draft-zyp-json-schema-04.txt
       "JSON Schema: core definitions and terminology"
  [11]: http://tools.ietf.org/id/draft-fge-json-schema-validation-00.txt
       "JSON Schema: interactive and non interactive validation"
  [12]: http://msgpack.org/
       "An efficient binary serialization format"
  [13]: http://www.ietf.org/rfc/rfc5424.txt
       "The Syslog Protocol"
  [14]: http://www.ietf.org/rfc/rfc5427.txt
       "Textual Conventions for Syslog Management"
  [15]: http://www.ietf.org/rfc/rfc3986.txt
       "Uniform Resource Identifier (URI): Generic Syntax"
  [16]: http://www.ietf.org/rfc/rfc4291.txt
       "IP Version 6 Addressing Architecture"
  [17]: http://www.ietf.org/rfc/rfc5952.txt
       "A Recommendation for IPv6 Address Text Representation"
  [18]: http://www.ietf.org/rfc/rfc4632.txt
       "Classless Inter-domain Routing (CIDR): The Internet Address Assignment and Aggregation Plan"
  [19]: http://www.ietf.org/rfc/rfc3339.txt
       "Date and Time on the Internet: Timestamps"
  [20]: http://www.ietf.org/rfc/rfc7230.txt
       "Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing"
  [21]: http://www.ietf.org/rfc/rfc7231.txt
       "Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content"
  [22]: http://www.ietf.org/rfc/rfc7538.txt
       "The Hypertext Transfer Protocol Status Code 308 (Permanent Redirect)"
  [23]: http://www.ietf.org/rfc/rfc5789.txt
       "PATCH Method for HTTP"
  [24]: http://www.ietf.org/rfc/rfc2818.txt
       "HTTP Over TLS (rfc2818)"
  [25]: http://www.ietf.org/rfc/rfc5425.txt
       "Transport Layer Security (TLS) Transport Mapping for Syslog"
  [26]: http://www.ietf.org/rfc/rfc5426.txt
       "Transmission of Syslog Messages over UDP"
  [27]: http://www.ietf.org/rfc/rfc6587.txt
       "Transmission of Syslog Messages over TCP"
  [28]: https://arxiv.org/ftp/arxiv/papers/1406/1406.2294.pdf
       "A Fast, Minimal Memory, Consistent Hash Algorithm, John Lamping, Eric Veach"
  [29]: http://www.ietf.org/rfc/rfc6012.txt
        "Datagram Transport Layer Security (DTLS) Transport Mapping for Syslog"
  [30]: http://www.ietf.org/rfc/rfc8259.txt
        "The JavaScript Object Notation (JSON) Data Interchange Format"
  [31]: http://www.ietf.org/rfc/rfc6902.txt
        "JavaScript Object Notation (JSON) Patch"
  [32]: http://www.ietf.org/rfc/rfc7396.txt
        "JSON Merge Patch"
  [33]: http://www.ietf.org/rfc/rfc9260.txt
       "[Castagnoli93] G. Castagnoli, S. Braeuer and M. Herrman \"Optimization
                       of Cyclic Redundancy-Check Codes with 24 and 32 Parity
                       Bits\", IEEE Transact. on Communications, Vol. 41, No.
                       6, June 1993."
  [34]: http://github.com/Cyan4973/xxHash
        "xxHash - Extremely fast hash algorithm"
  [35]: https://en.wikipedia.org/wiki/Levenshtein_distance
        "Levenshtein distance"
  [36]: https://en.wikipedia.org/wiki/Bloom_filter
        "Bloom filter"
  [37]: https://en.wikipedia.org/wiki/Mustache_(template_system)
        "Mustache (template system)"
  [38]: https://en.wikipedia.org/wiki/Trie
        "Trie"
  [39]: https://en.wikipedia.org/wiki/UTF-8
        "UTF-8"
  [40]: https://en.wikipedia.org/wiki/Range_tree
        "Range tree"
  [41]: https://en.wikipedia.org/wiki/T-tree
        "T-tree"