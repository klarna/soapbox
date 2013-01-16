%% List of prim_type.

%%%
%%%   Copyright 2011-2013 Klarna AB
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%

-module(listobj_type_test).
-behaviour(soapbox_type_list).

-export([ name/1
        , parameters/0
        , normalize/2
        , element_type/1
        , convert/2
        ]).

name(_)           -> listobj_type.
parameters()      -> [].
normalize(Val, _) -> Val.
element_type(_)   -> obj_int_type_test.
convert(Val, _)   -> Val.

%%% eof
