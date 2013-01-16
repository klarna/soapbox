%%% @doc Soapbox type for soapbox_method_router internal routing functions
%%% @copyright 2012 Klarna AB
-module(soapbox_type_routefn).
-behaviour(soapbox_type_primitive).

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

%%%_* Exports ==========================================================
-export([ parameters/0
        , name/1
        , normalize/2
        , validate/2
        , convert/2
        ]).

%%%_* Includes =========================================================

%%%_* Code =============================================================
parameters() -> [ {name, routefn}
                ].

name(Params) -> soapbox_obj:get(Params, name).

normalize(left,  _Params) -> fun(_Arg) -> left  end;
normalize(right, _Params) -> fun(_Arg) -> right end;
normalize(F,     _Params) -> F.

validate(Val, _Params) when is_function(Val, 1) -> true.

convert(Val, _Params) -> Val.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
