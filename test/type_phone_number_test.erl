%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Example type.
%%% @copyright 2011 Klarna AB
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

-module(type_phone_number_test).
-behaviour(soapbox_type_primitive).

-export([ name/1
        , convert/2
        , parameters/0
        , validate/2
        , normalize/2
        ]).

-include_lib("tulib/include/guards.hrl").

name(_Params) -> phone_number.

parameters() -> [{has_trunk_code, false}].

validate(Val, _Params) when is_list(Val) -> true.

normalize(Val, _Params) -> string:tokens(string:strip(Val), " -").

convert(Val, Params) ->
  case soapbox_obj:get(Params, has_trunk_code) of
    true  -> Val;
    false -> ["1"|Val]
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
