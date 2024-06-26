%% -*-erlang-*-
%%==============================================================================
%% Copyright 2020-2024 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%% Defines

%% Records
-record(sos, {attrs = [] :: [attr()]}).

-record(elt, {tag :: atom() | binary(),
              attrs = [] :: [attr()],
              children = [] :: [elt() | cdata() | binary()]}).
-record(cdata, {data = <<>> :: binary()}).

-record(pi, {target :: atom() | binary(), data = <<>> :: binary()}).
-record(comment, {text = <<>> :: binary()}).
-record(decl, {standalone = no :: yes | no}).

-record(child, {type :: choice | seq,
                reg = none :: '?' | '*' | '+' | none,
                children = [] :: [atom() | binary() | #child{}]}).
-record(elt_decl, {name :: atom() | binary(),
                   pcdata = false :: boolean(),
                   content = empty :: empty | any |
                                      %% Mixed
                                      [atom() | binary()] |
                                      %% Children
                                      #child{}}).
-record(attr_decl, {}).
-record(entity_decl, {}).
-record(notation_decl, {}).
-record(pe_ref, {name :: atom() | binary()}).
-record(doc, {name :: atom() | binary(),
              system :: undefined | atom() | binary(),
              public :: undefined | atom() | binary(),
              set = [] ::[#elt_decl{} | #attr_decl{} | #entity_decl{} |
                          #pe_ref{} | #notation_decl{} | #pi{} | #comment{}]}).

%% Types
-type attr() :: {atom() | binary(), atom() | binary()}.
-type elt() :: #elt{}.
-type cdata() :: #cdata{}.

-type sos() :: #sos{}.
-type xml() :: eos | sos() |
               #elt{} | #cdata{} | #pi{} | #comment{} | #decl{} | #doc{}.
