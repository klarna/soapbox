%% Object, recursive.

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

-module(obj_rtype_test).
-behaviour(soapbox_type_object).

-export([ name/1
        , parameters/0
        , normalize/2
        , spec/2
        , convert/2
        ]).

name(_)           -> obj_rtype.
parameters()      -> [n].
convert(Val, _)   -> Val.
normalize(Val, _) -> Val.

spec(Val, Params) ->
  Result = soapbox_obj:get(Val, result),
  N      = soapbox_obj:get(Params, n),
  if N =:= 0 -> [result, {Result, prim_type_test}];
     N  >  0 -> [result, { soapbox_obj:from_list([result, Result-1])
                         , obj_rtype_test
                         , [n, N-1]
                         }]
  end.

%%% eof
