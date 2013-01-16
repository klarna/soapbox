%%% @doc Parameterized module for creating an anonymous soapbox method.
%%% @copyright 2012 Klarna AB
-module(soapbox_method_anon, [Spec, Verify, Call]).
%% -behaviour(soapbox_method). %wont pass check because parameterized

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
%% Method Callbacks
%% Callbacks
-export([ call/1
        , spec/1
        , verify/1
        ]).

%%%_* Includes =========================================================

%%%_* Code =============================================================
%%%_* Callbacks --------------------------------------------------------
spec(Val)    -> Spec(Val).
verify(Args) -> Verify(Args).
call(Args)   -> Call(Args).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
