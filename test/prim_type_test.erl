%% Primitive, parameterless.

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

-module(prim_type_test).
-behaviour(soapbox_type_primitive).

-export([ name/1
        , parameters/0
        , normalize/2
        , validate/2
        , convert/2
        ]).

name(_)         -> prim_type.
parameters()    -> [].

normalize(0, _) -> 2;
normalize(1, _) -> 2;
normalize(X, _) -> X.

validate(2, _)  -> true.

convert(Val, _) -> Val.

%%% eof
