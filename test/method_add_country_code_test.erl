%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Example method.
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
-module(method_add_country_code_test).
-behaviour(soapbox_method).

%%%_* Exports ==========================================================
-export([ spec/1
        , verify/1
        , call/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Xref =============================================================
-ignore_xref([ call/1
             , spec/1
             , verify/1
             ]).

%%%_* Code =============================================================
spec([PhoneNumber]) ->
  [ phone_number, { PhoneNumber
                  , type_phone_number_test
                  , [has_trunk_code, false]
                  }
  ].

verify(_Args) -> ok.

call(Args) ->
  [_TrunkCode|PhoneNumber] = soapbox_obj:get(Args, phone_number),
  {ok, "001," ++ string:join(PhoneNumber, ",")}.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("soapbox/include/soapbox_test.hrl").

method_test() ->
  ?assertMethodOk(["123 456 7890"], "001,123,456,7890"),
  ?assertMethodOk(["123 456-7890"], "001,123,456,7890"),
  ?assertMethodOk(["123-456-7890"], "001,123,456,7890"),
  ?assertMethodError([1234567890], {type, _}),
  ok.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
