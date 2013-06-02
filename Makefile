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

BASE := $(strip $(wildcard deps/makefiles/erlang.mk))

ifeq (${BASE},)
  DUMMY := $(shell mkdir -p deps)
  DUMMY := $(shell cd deps && \
                   git clone https://github.com/JanHenryNystrom/makefiles)
endif

include deps/makefiles/erlang.mk
