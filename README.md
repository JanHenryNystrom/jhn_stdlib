jhn_stdlib [(γ)][5]
==========

A few thought experiments solidified as code: CBOR, JSON, MessagePack, syslog,
URI, IP addresses, Timestamps, UUIDs

  * [Introduction](#introduction)
  * [Behaviours](#behaviours)
  * [Features/Modules](#features)
  * [Deprecated Features/Modules](#deprecated)
  * [Build](#build)
  * [Install](#install)
  * [Contribute](#contribute) - Read if you're planning to submit patches

<a name='introduction'/>

Introduction
------------

This library consists mainly of code that arose out of my curiosity, either
about Erlang and coding in general or concerning a perticular protocol or
technique. So there is little of cohesion in purposes between the different
library modules. But having published these I will continue to support these
since the road to enlightenment is one without terminus.

Behaviours
--------

  * ```jhn_server``` -- A generic server
  * ```jhn_fsm``` -- A FSM with the ability to defer acting on events until
    a later state

<a name='features'/>

Features/Modules
--------

  * String Processing Functions for binary encoded strings
    * ```jhn_blist``` -- drop in replacement for the lists module in stdlib
    * ```jhn_bstring``` -- drop in replacement for the string module in stdlib
    * ```jhn_mustache``` -- [Mustache (template system)][37]
  * Protocols
    * Bencoding -- encoding/decoding -- ```jhn_bencoding```
    * CBOR -- encoding/decoding -- ```jhn_cbor``` [rfc8949][43], [rfc9542][45]
    * JSON  -- encoding/decoding -- ```jhn_json``` [rfc8259][30]
    * JSON Pointer -- encoding/decoding/evaluation -- ```jhn_json``` [rfc6901][8]
    * JSON Patch -- evaluation -- ```jhn_json``` [rfc6902][31]
    * JSON Merge Patch  -- evaluation -- ```jhn_json``` [rfc7396][32]
    * MessagePack -- encoding/decoding -- ```jhn_msgpack``` [MessagePack][12]/Erlang
    * Syslog -- encoding/decoding  -- ```jhn_syslog``` [rfc5424][13], [rfc5427][14], [rfc6012][29]
  * Standards
    * IP Addresses -- encoding/decoding -- ```jhn_ip_addr``` [rfc4291][16], [rfc5952][17], [rfc4632][18]
    * Timestamps -- generating/encoding/decoding -- ```jhn_timestamp``` [rfc3339][19], [rfc7231][21]
    * URI -- encoding/decoding -- ```jhn_uri``` [rfc3986][15]
    * UUID -- generating/encoding/decoding -- ```jhn_uuid``` [rfc9562][42][rfc8144]
  * Clients
    * HTTP -- client -- ```jhn_shttpc``` [rfc7230][20], [rfc7231][21], [rfc7538][22], [rfc5789][23], [rfc2818][24]
    * Syslog -- server/client -- ```jhn_syslog``` [rfc5425][25], [rfc5426][26], [rfc6587][27]
  * Pull oriented data source abstraction
    * lazy -- ```jhn_lazy``` abstracts different data sources as uniform lazy data
  * Data structures
    * Bloom filters -- ```jhn_bloom``` [Bloom Filters][36]
    * Property lists -- ```jhn_plist```
    * Prefix trees -- ```jhn_p_tree``` [Prefix Tree][38]
    * Binary(UTF-8) Prefix trees -- ```jhn_pb_tree``` [Prefix Tree][38], [UTF-8][39]
    * Range trees -- ```jhn_r_tree``` [Range Tree][40]
    * T-trees -- ```jhn_t_tree``` [T-tree][41]
  * Algorithms
    * Levenshtein distance -- ```jhn_math:levenshtein/2``` [Levenshtein][35]
    * Check digit
       * Luhn algorithm -- ```jhn_math:levenshtein/2``` [Luhn][46]
       * Verhoeff algorithm -- ```jhn_math:verhoeff/2``` [Verhoeff][47]
       * Damm check algorithm -- ```jhn_math:damm/2``` [Damm][48]
    * Checksums
       * CRC32-C checksum -- ```jhn_hash:crc32c/1``` [rfc9260][33]
       * xxHash-32 checksum -- ```jhn_hash:xxh32/1```, ```jhn_hash:xxh32/2``` [xxHash][34]
    * Consistent Hashing -- ```jhn_chash:jump/2``` [J. Lamping, Eric Veach][28]

<a name='deprecated'/>

Deprecated Features/Modules
--------

<a name='build'/>

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

<a name='install'/>

Install
-------

If you want to install your own built version of jhn_stdlib add the ebin
directory to your Erlang code path or move the jhn_stdlib folder into your
release folder and make sure that folder is in your `ERL_LIBS`
environment variable.


<a name='contribute'/>

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

  [1]: https://github.com/erlang/rebar3 "Rebar3 - A build tool for Erlang"
  [2]: http://github.com/JanHenryNystrom/jhn_stdlib/issues "jhn_stdlib issues"
  [4]: http://github.com/erlang/otp/wiki/Writing-good-commit-messages "Erlang/OTP commit messages"
  [5]: http://en.wikipedia.org/wiki/Software_release_life_cycle "Software release life cycle"
  [6]: http://www.ietf.org/rfc/rfc4627.txt "The application/json Media Type for JavaScript Object Notation (JSON)"
  [7]: http://www.ietf.org/rfc/rfc7159.txt "The JavaScript Object Notation (JSON) Data Interchange Format"
  [8]: http://www.ietf.org/rfc/rfc6901.txt "JavaScript Object Notation (JSON) Pointer"
  [10]: http://tools.ietf.org/id/draft-zyp-json-schema-04.txt "JSON Schema: core definitions and terminology"
  [11]: http://tools.ietf.org/id/draft-fge-json-schema-validation-00.txt "JSON Schema: interactive and non interactive validation"
  [12]: http://msgpack.org/ "An efficient binary serialization format"
  [13]: http://www.ietf.org/rfc/rfc5424.txt "The Syslog Protocol"
  [14]: http://www.ietf.org/rfc/rfc5427.txt "Textual Conventions for Syslog Management"
  [15]: http://www.ietf.org/rfc/rfc3986.txt "Uniform Resource Identifier (URI): Generic Syntax"
  [16]: http://www.ietf.org/rfc/rfc4291.txt "IP Version 6 Addressing Architecture"
  [17]: http://www.ietf.org/rfc/rfc5952.txt "A Recommendation for IPv6 Address Text Representation"
  [18]: http://www.ietf.org/rfc/rfc4632.txt "Classless Inter-domain Routing (CIDR): The Internet Address Assignment and Aggregation Plan"
  [19]: http://www.ietf.org/rfc/rfc3339.txt "Date and Time on the Internet: Timestamps"
  [20]: http://www.ietf.org/rfc/rfc7230.txt "Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing"
  [21]: http://www.ietf.org/rfc/rfc7231.txt "Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content"
  [22]: http://www.ietf.org/rfc/rfc7538.txt "The Hypertext Transfer Protocol Status Code 308 (Permanent Redirect)"
  [23]: http://www.ietf.org/rfc/rfc5789.txt "PATCH Method for HTTP"
  [24]: http://www.ietf.org/rfc/rfc2818.txt "HTTP Over TLS (rfc2818)"
  [25]: http://www.ietf.org/rfc/rfc5425.txt "Transport Layer Security (TLS) Transport Mapping for Syslog"
  [26]: http://www.ietf.org/rfc/rfc5426.txt "Transmission of Syslog Messages over UDP"
  [27]: http://www.ietf.org/rfc/rfc6587.txt "Transmission of Syslog Messages over TCP"
  [28]: https://arxiv.org/ftp/arxiv/papers/1406/1406.2294.pdf "A Fast, Minimal Memory, Consistent Hash Algorithm, John Lamping, Eric Veach"
  [29]: http://www.ietf.org/rfc/rfc6012.txt "Datagram Transport Layer Security (DTLS) Transport Mapping for Syslog"
  [30]: http://www.ietf.org/rfc/rfc8259.txt "The JavaScript Object Notation (JSON) Data Interchange Format"
  [31]: http://www.ietf.org/rfc/rfc6902.txt "JavaScript Object Notation (JSON) Patch"
  [32]: http://www.ietf.org/rfc/rfc7396.txt "JSON Merge Patch"
  [33]: http://www.ietf.org/rfc/rfc9260.txt "[Castagnoli93] G. Castagnoli, S. Braeuer and M. Herrman \"Optimization of Cyclic Redundancy-Check Codes with 24 and 32 Parity Bits\", IEEE Transact. on Communications, Vol. 41, No. 6, June 1993."
  [34]: http://github.com/Cyan4973/xxHash "xxHash - Extremely fast hash algorithm"
  [35]: https://en.wikipedia.org/wiki/Levenshtein_distance "Levenshtein distance"
  [36]: https://en.wikipedia.org/wiki/Bloom_filter "Bloom filter"
  [37]: https://en.wikipedia.org/wiki/Mustache_(template_system) "Mustache (template system)"
  [38]: https://en.wikipedia.org/wiki/Trie "Trie"
  [39]: https://en.wikipedia.org/wiki/UTF-8 "UTF-8"
  [40]: https://en.wikipedia.org/wiki/Range_tree "Range tree"
  [41]: https://en.wikipedia.org/wiki/T-tree "T-tree"
  [42]: http://www.ietf.org/rfc/rfc9562.txt "Universally Unique IDentifiers (UUIDs)"
  [43]: http://www.ietf.org/rfc/rfc8949.txt "Concise Binary Object Representation (CBOR)"
  [44]: http://www.ietf.org/rfc/rfc9542.txt "IANA Considerations and IETF Protocol and Documentation Usage for IEEE 802 Parameters"
  [45]: http://www.ietf.org/rfc/rfc8141.txt "Uniform Resource Names (URNs)"
  [46]: https://en.wikipedia.org/wiki/Luhn_algorithm "Luhn algorithm"
  [47]: https://en.wikipedia.org/wiki/Verhoeff_algorithm "Verhoeff algorithm"
  [48]: https://en.wikipedia.org/wiki/Damm_algorithm "Damm algorithm"