#==============================================================================
# Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#==============================================================================

REBAR="./rebar"
.PHONY: default build xref compile test doc clean dist-clean real-clean \
        get-deps update-deps

default: build

build: get-deps compile

rebar:
	mkdir -p deps
	(cd deps && git clone https://github.com/rebar/rebar)
	(cd deps/rebar && ./bootstrap)
	cp deps/rebar/rebar .

compile: rebar
	@$(REBAR) -j compile

xref: compile
	@$(REBAR) -jk skip_deps=true xref

test: build
	@rm -rf .eunit
	@$(REBAR) -jk eunit skip_deps=true

doc: rebar
	@$(REBAR) -j doc skip_deps=true

clean: rebar
	@$(REBAR) -j clean

dist-clean: clean
	@$(REBAR) -j delete-deps

real-clean: dist-clean
	rm -f rebar
	rm -fr deps
	rm -fr ebin

get-deps: rebar
	@$(REBAR) -j get-deps

update-deps: rebar
	@$(REBAR) -j update-deps
	@$(REBAR) -j get-deps

update-rebar: rebar
	(cd deps/rebar && git pull && ./bootstrap)
	cp deps/rebar/rebar ${REBAR}
