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

-module(call_test).
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

method_test() ->
    Method   = add_country_code,
    Args     = ["123 456 7890"],
    Context  = {test_context_tuple},
    Result   = stub_test:eval(Method, Args, Context),
    Expected = "001,123,456,7890",
    ?assertEqual(Result, Expected).

-endif.
