%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Dummy stub.
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
-module(stub_test).
-behavior(soapbox_stub).

%%%_* Exports ==========================================================
%% API
-export([ eval/3
        ]).

%% soapbox_stub callbacks
-export([ eval/5
        , ok/4
        , error/4
        , log_call/3
        , log_ok/4
        , log_error/4
        , log_response/4
        , log_return/5
        , log_spec/4
        , method/1
        , pack/1
        , unpack/1
        ]).

%%%_* Xref =============================================================
-ignore_xref([ error/4
             , eval/5
             , log_call/3
             , log_error/4
             , log_ok/4
             , log_response/4
             , log_return/5
             , method/1
             , ok/4
             , pack/1
             , unpack/1
             ]).

%%%_* Includes =========================================================
-include_lib("tulib/include/logging.hrl").

%%%_* Code =============================================================
eval(Method, Args, Ctx) -> (soapbox:handler(?MODULE))(Method, Args, Ctx).

eval(Call, Return, _Method, _Args, _Ctx) ->
  try Return(Call())
  catch Type:Exn -> {error, {Type, Exn, erlang:get_stacktrace()}}
  end.

method(add_country_code)    -> method_add_country_code_test.

pack(Resp)                  -> Resp.
unpack(Args)                -> Args.

log_call(_Mtd, _Args, _Ctx)   -> ?info("CALL: ~p(~p, ~p)", [_Mtd, _Args, _Ctx]).
log_spec(_Spec, _, _, _)      -> ?info("SPEC: ~p", [_Spec]).
log_ok(_Res, _, _, _)         -> ?info("OK: ~p", [_Res]).
log_error(_Rsn, _, _, _)      -> ?info("ERROR: ~p", [_Rsn]).
log_response(_Resp, _, _, _)  -> ?info("RESPONSE: ~p", [_Resp]).
log_return(_Ret, _A, _, _, _) -> ?info("RETURN: ~p (~p)", [_Ret, _A]).

ok(Res, _, _, _)            -> Res.
error(Rsn, _, _, _)         -> Rsn.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
