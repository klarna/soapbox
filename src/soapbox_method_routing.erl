%%% @doc Utilities for using soapbox_method_router.
%%% @copyright 2012 Klarna AB
-module(soapbox_method_routing).

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
%% Behaviour
-export([ behaviour_info/1
        ]).

-export([ make_routing_from_module/1
        , make_routing/3
        , forward/0
        , specable/0
        , verifyable/0
        , callable/0
        , routing/3
        ]).

%%%_* Includes =========================================================

%%%_* Code =============================================================
%%%_* Behaviour --------------------------------------------------------
behaviour_info(callbacks) ->
  [ {route_spec,   1}
  , {route_verify, 1}
  , {route_call,   1}
  ];
behaviour_info(_) -> undefined.


%%%_* API --------------------------------------------------------------
make_routing_from_module(Module) ->
  make_routing(fun(Val)  -> Module:route_spec(Val) end,
               fun(Args) -> Module:route_verify(Args) end,
               fun(Args) -> Module:route_call(Args) end).

make_routing(Spec, Verify, Call) ->
  RoutingSpec = lists:flatten([ mk(Spec,   route_spec)
                              , mk(Verify, route_verify)
                              , mk(Call,   route_call)
                              ]),
  {ok, Routing} = soapbox_type:check(RoutingSpec),
  Routing.

forward()    -> make_routing(right, right, right).
specable()   -> make_routing(left,  right, right).
verifyable() -> make_routing(left,  left,  right).
callable()   -> make_routing(left,  left,  left).

routing(Routing, Callback, Arg) when Callback =:= spec;
                                     Callback =:= verify;
                                     Callback =:= call ->
  RouteFn = soapbox_obj:get(Routing, cb_to_route_cb(Callback)),
  RouteFn(Arg).

%%%_* Helpers  =========================================================
cb_to_route_cb(spec)   -> route_spec;
cb_to_route_cb(verify) -> route_verify;
cb_to_route_cb(call)   -> route_call.

mk(Arg, Name) -> [Name, {Arg, soapbox_type_routefn, [name, Name]}].

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
