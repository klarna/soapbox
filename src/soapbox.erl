%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The SoapBox RPC engine.
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
-module(soapbox).

%%%_* Exports ==========================================================
-export([ annotate/2
        , handler/1
        ]).

%% Testing
-export([ do_call/2
        ]).

%%%_* Includes =========================================================
-include_lib("tulib/include/prelude.hrl").

%%%_* Code =============================================================
handler(StubCb) ->
  fun(Method, Args, State) ->
      StubCb:eval(?thunk(call(StubCb, Method, Args, State)),
                  fun(R) -> return(R, StubCb, Method, Args, State) end,
                  Method,
                  Args,
                  State)
  end.

call(StubCb, Method, Args0, State) ->
  StubCb:log_call(Method, Args0, State),
  MethodCb = StubCb:method(Method),
  Args1    = StubCb:unpack(Args0),
  Spec     = MethodCb:spec(Args1),
  StubCb:log_spec(soapbox_obj:from_list(Spec), Method, Args0, State),
  do_call(MethodCb, Args1).

do_call(MethodCb, Args1) ->
  case soapbox_type:check(MethodCb:spec(Args1)) of
    {ok, Args} ->
      case MethodCb:verify(Args) of
        ok               -> MethodCb:call(Args);
        {error, _} = Err -> Err
      end;
    {error, _} = Err -> Err
  end.

-record(annotated, {return::_, annotation::_}).
annotate(R, A) -> #annotated{return=R, annotation=A}.

return(#annotated{return=R, annotation=A}, StubCb, Method, Args, State) ->
  StubCb:log_return(R, A, Method, Args, State),
  return(R, StubCb, Method, Args, State);
return(R, StubCb, Method, Args, State) ->
  Response0 =
    case R of
      {ok, Res} ->
        StubCb:log_ok(Res, Method, Args, State),
        StubCb:ok(Res, Method, Args, State);
      {error, Rsn} ->
        StubCb:log_error(Rsn, Method, Args, State),
        StubCb:error(Rsn, Method, Args, State)
    end,
  Response = StubCb:pack(Response0),
  StubCb:log_response(Response, Method, Args, State),
  Response.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
