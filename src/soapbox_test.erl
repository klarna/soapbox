%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Test support.
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

%%%_* Module declaration ===============================================
%% @private
-module(soapbox_test).

%%%_* Exports ==========================================================
-export([ method/4
        , type/0
        ]).

%%%_* Includes =========================================================
%%-include_lib("").

%%%_* Code =============================================================
-type check() :: fun((_) -> ok | no_return()).

-spec method(module(), [_], check(), check()) -> true | no_return().
%% @doc
method(Method, Args, Ok, Error) ->
  case soapbox:do_call(Method, Args) of
    {ok, Res}    -> true = Ok(Res);
    {error, Rsn} -> true = Error(Rsn)
  end.


-spec type() -> boolean().
%%@doc
type() -> throw(nyi).

%%%_* Tests ============================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
