%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc SoapBox stub behaviour.
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
-module(soapbox_stub).

%%%_* Exports ==========================================================
-export([behaviour_info/1]).

%%%_* Includes =========================================================
%%-include_lib("").

%%%_* Code =============================================================
%% -type m() :: atom(). %method
%% -type a() :: [_].    %argument list
%% -type s() :: _.      %state record

%% -callback eval(fun(), fun(), m(), a(), s()) -> _.
%% -callback method(m())                       -> module().
%% -callback unpack(_)                         -> _.
%% -callback ok(_, m(), a(), s())              -> _.
%% -callback error(_, m(), a(), s())           -> _.
%% -callback pack(_)                           -> _.
%% -callback log_call(m(), a(), s())           -> ok.
%% -callback log_return(_, _, m(), a(), s())   -> ok.
%% -callback log_ok(_, m(), a(), s())          -> ok.
%% -callback log_error(_, m(), a(), s())       -> ok.
%% -callback log_response(_, m(), a(), s())    -> ok.
%% -callback log_spec(_, m(), a(), s())        -> ok.

behaviour_info(callbacks) ->
  [ {eval,         5}
  , {method,       1}
  , {unpack,       1}
  , {ok,           4}
  , {error,        4}
  , {pack,         1}
  , {log_call,     3}
  , {log_return,   5}
  , {log_ok,       4}
  , {log_error,    4}
  , {log_response, 4}
  , {log_spec,     4}
  ];
behaviour_info(_) -> undefined.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
