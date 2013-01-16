%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Tests for higher-order methods provided with SoapBox.
%%% @copyright 2012 Klarna AB
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
-module(higher_order_methods_test).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================
-ifdef(EUNIT).
-include_lib("soapbox/include/soapbox_test.hrl").

method_anon_test() ->
  Spec    = fun(Val) -> [ val, {Val, prim_type_test} ] end,
  Verify  = fun(Args) -> soapbox_obj:get(Args, val) > 1 andalso ok end,
  Call    = fun(Args) -> {ok, soapbox_obj:get(Args, val) * 2} end,
  Method  = soapbox_method_anon:new(Spec, Verify, Call),
  ?assertMethodOk(Method, 1, 4).

method_constant_test() ->
  K = soapbox_method_constant:new(1),
  ?assertMethodOk(K, [], 1),
  ok.

method_router_test() ->
  K1       = soapbox_method_constant:new(1),
  K2       = soapbox_method_constant:new(2),
  Routing1 = soapbox_method_routing:callable(),   %% L, L, L
  Routing2 = soapbox_method_routing:verifyable(), %% L, L, R
  Routed1  = soapbox_method_router:new(K1, K2, Routing1),
  Routed2  = soapbox_method_router:new(K1, K2, Routing2),
  ?assertMethodOk(Routed1, [], 1),
  ?assertMethodOk(Routed2, [], 2),

  RouteFn       = fun(Args) ->
                      case soapbox_obj:size(Args) of
                        0 -> left;
                        _ -> right
                      end
                  end,
  CustomRouting = soapbox_method_routing:make_routing(left, left, RouteFn),
  CustomRouted  = soapbox_method_router:new(K1, K2, CustomRouting),
  ?assertMethodOk(CustomRouted, [], 1),
  ok.


-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
