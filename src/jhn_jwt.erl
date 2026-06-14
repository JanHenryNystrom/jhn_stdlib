%% JWT
%% rfc7519: JSON Web Token (JWT)
%% rfc7797: JSON Web Signature (JWS) Unencoded Payload Option
%% rfc8725: JSON Web Token Best Current Practices
%% ?? rfc7523: JSON Web Token (JWT) Profile
%%               for OAuth 2.0 Client Authentication and Authorization Grants
%% ?? rfc7638: JSON Web Key (JWK) Thumbprint
%% JWS
%% rfc7515: JSON Web Signature (JWS)
%%
%% JWA
%% rfc7518: JSON Web Algorithms (JWA)
%% rfc9864: Fully-Specified Algorithms for
%%          JSON Object Signing and Encryption (JOSE) and
%%          CBOR Object Signing and Encryption (COSE)
%%
%% JWE
%% rfc7516: JSON Web Encryption (JWE)
%%
%% JWK
%% rfc7517: JSON Web Key (JWK)
%%
%% https://www.iana.org/assignments/jose/jose.xhtml
%% JSON Object Signing and Encryption (JOSE)
%%
%%  X.509:
%%  Internet X.509 Public Key Infrastructure Certificate
%%             and Certificate Revocation List (CRL) Profile
%%
%%
%%
%%
-module(jhn_jwt).

-export([encode/1, decode/1])

-include_lib("public_key/include/public_key.hrl").

%% RFC7517
-type jkws() :: {keys := [jwk()]}.

-type jkw() :: {%% rfc7517
                kty := kty(),
                use => use(),
                key_ops => [key_op()],
                alg => alg(),
                kid => binary(),
                x5u => jhn_uri:uri(),
                x5c => [pkix_certificate()],
                x5t => x_509_thumb(),
                x5t256 => x_509_thumb()
               }.

-type kty() ::
        %% rfc7518 (EC, RSA, oct)
        ec | rsa | oct |
        %% rfc8037 (OKP)
        okp |
        %% rfc9964 (AKP)
        akp.

-type use() :: sig | enc.

-type key_op() :: sign | verify | encrypt | decrypt |
                  wrap_key | unwrap_key |
                  derive_key | derive_bits.

-type alg() :: rfc7518.

-type kid() :: binary().

%% rfc5280
-type pkix_certificate() :: #Certificate{}.

-type x_509_thumb() :: binary().

encode(_) -> tbd.

decode(_) -> tbd.

encode_kty(ec) -> ~"EC";
encode_kty(rsa) -> ~"RSA";
encode_kty(oct) -> ~"oct";
encode_kty(okp) -> ~"OKP";
encode_kty(akp) -> ~"AKP".

encode_use(sig) -> ~"sig";
encode_use(enc) -> ~"enc".

encode_key_op(sign) -> ~"sign";
encode_key_op(verify) -> ~"verify";
encode_key_op(encrypt) -> ~"encrypt";
encode_key_op(decrypt) -> ~"decrypt";
encode_key_op(wrap_key) -> ~"wrapKey";
encode_key_op(unwrap_key) -> ~"UnwrapKey";
encode_key_op(derive_key) -> ~"deriveKey";
encode_key_op(derive_bits) -> ~"deriveBits".

encode_x5u(URI) -> jhn_uri:encode(URI).

encode_x5c(Chain) ->
    [base64:encode(public_key:der_encode('Certificate', C)) || C <- Chain].

encode_x5t(Thumb) -> base64:encode(Thumb, [#{mode => urlsafe}]).

encode_x5t256(Thumb) -> base64:encode(Thumb, [#{mode => urlsafe}]).

decode_kty(~"EC") -> ec;
decode_kty(~"RSA") -> rsa;
decode_kty(~"oct") -> oct;
decode_kty(~"OKP") -> okp;
decode_kty(~"AKP") -> akp.

decode_user(~"sig") -> sig;
decode_user(~"enc") -> enc.

decode_key_op(~"sign") -> sign;
decode_key_op(~"verify") -> verify;
decode_key_op(~"encrypt") -> encrypt;
decode_key_op(~"decrypt") -> decrypt;
decode_key_op(~"wrapKey") -> wrap_key;
decode_key_op(~"unwrapKey") -> unwrap_key;
decode_key_op(~"deriveKey") -> derive_key;
decode_key_op(~"deriveBits") -> derive_bits.

decode_x5u(B) -> jhn_uri:decode(B).

decode_x5c(Chain) ->
    [public_key:der_decode(base64:decode('Certificate', C)) || C <- Chain].

decode_x5t(Thumb) -> base64:decode(Thumb, [#{mode => urlsafe}]).

decode_x5t256(Thumb) -> base64:decode(Thumb, [#{mode => urlsafe}]).
