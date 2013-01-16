%%% @doc Parameterized module for creating a soapbox method that returns
%%%      a constant value in its call.
%%% @copyright 2012 Klarna AB
-module(soapbox_method_constant, [Value]).
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
spec(_Val)    -> [].
verify(_Args) -> ok.
call(_Args)   -> {ok, Value}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
