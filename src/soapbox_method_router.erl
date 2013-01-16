%%% @doc Parameterized module for combining two soapbox methods based on
%%%      a routing.
%%%
%%%      To differentiate the methods being combined, we use the
%%%      convention Left and Right. The Routing argument should be
%%%      created via the module soapbox_method_routing and routes to
%%%      either 'left' or 'right' for each soapbox_method callback.
%%% @copyright 2012 Klarna AB
-module(soapbox_method_router, [Left, Right, Routing]).

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
spec(Val)    -> do_routing(spec,   Val).
verify(Args) -> do_routing(verify, Args).
call(Args)   -> do_routing(call,   Args).

%%%_* Helpers ----------------------------------------------------------
do_routing(Callback, Arg) ->
  tulib_call:dispatch(module_map(Callback), Arg, Callback, [Arg]).

module_map(Callback) ->
  fun(Arg) ->
      method(soapbox_method_routing:routing(Routing, Callback, Arg))
  end.

method(left)  -> Left;
method(right) -> Right.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
