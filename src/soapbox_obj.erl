%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc SoapBox objects.
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
-module(soapbox_obj).

%%%_* Exports ==========================================================
-export([ all/2
        , del/2
        , difference/2
        , equal/2
        , filter/2
        , from_list/1
        , from_list/2
        , from_proplist/1
        , from_proplist/2
        , fold/3
        , get/2
        , get/3
        , intersection/2
        , is_empty/1
        , is_key/2
        , is_object/1
        , is_value/1
        , keys/1
        , multiget/2
        , multiset/2
        , map/2
        , mapfold/3
        , new/0
        , pick/1
        , set/3
        , size/1
        , to_list/1
        , union/2
        ]).

-export_type([ ect/0
             , key/0
             ]).

-ignore_xref([ del/2
             , fold/3
             , from_list/2
             , from_proplist/2
             , is_key/2
             , keys/1
             , mapfold/3
             ]).

%%%_* Includes =========================================================
-define(ASSERTIONS, true).

-include_lib("tulib/include/assert.hrl").
-include_lib("tulib/include/guards.hrl").
-include_lib("tulib/include/logging.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%_* Code =============================================================
%%%_ * Basics ----------------------------------------------------------
-type   key()  :: _.
-type   attr() :: {_, _}.
-opaque ect()  :: tuple(). %dict:dict().

-spec to_list(ect()) -> [attr()].
to_list(Obj)         -> dict:to_list(Obj).


-spec from_list(_)  -> ect() | no_return().
from_list(Lst)      -> from_list(Lst, fun(_, _) -> true end).

-spec from_list([_], fun((_, _) -> boolean())) -> ect() | no_return().
from_list(KVs, Pred)              -> from_list(KVs,  Pred, []).
from_list([K, V|Rest], Pred, Acc) -> from_list(Rest, Pred, [{K, V}|Acc]);
from_list([],          Pred, Acc) -> from_proplist(Acc, Pred).

-define(UNDEFINED, '__soapbox_undefined__').

-spec from_proplist([_ | {_, _}]) -> ect() | no_return().
from_proplist(Lst) -> from_proplist(Lst, fun(_, _) -> true end).

-spec from_proplist([_ | {_, _}], fun()) -> ect() | no_return().
from_proplist(Lst, Pred) ->
  dict:from_list(
    [case KV of
       {K, V} when V =/= ?UNDEFINED ->
         Pred(K, V) orelse throw({validator, K, V}),
         {K, V};
       K when not is_tuple(K) ->
         V = ?UNDEFINED,
         Pred(K, V) orelse throw(validator),
         {K, V}
     end || KV <- Lst]).


-spec new() -> ect().
new()       -> dict:new().


-spec pick(ect()) -> attr().
pick(Obj) ->
  case dict:to_list(Obj) of
    []        -> undefined;
    [First|_] -> First
  end.


-spec get(ect(), key()) -> _ | no_return().
get(Obj, K)             -> do_get(Obj, K).

-spec get(ect(), key(), _) -> _.
get(Obj, K, Default) ->
  try do_get(Obj, K)
  catch error:{not_found, K} -> Default
  end.

-spec multiget(ect(), [key()]) -> [_] | no_return().
multiget(Obj, Ks)              -> [do_get(Obj, K) || K <- Ks].

do_get(Obj, K) ->
  case dict:find(K, Obj) of
    {ok, V} -> V;
    error   -> erlang:error({not_found, K})
  end.


-spec multiset(ect(), [{key(), _}]) -> ect() | no_return().
multiset(Obj, KVs) ->
  lists:foldl(fun({K, V}, Acc) -> dict:store(K, V, Acc) end, Obj, KVs).

-spec set(ect(), key(), _) -> ect() | no_return().
set(Obj, K, V)             -> dict:store(K, V, Obj).


-spec is_key(ect(), key()) -> boolean().
is_key(Obj, K)             -> dict:is_key(K, Obj).

-spec del(ect(), key()) -> _ | no_return().
del(Obj, K)             -> dict:erase(K, Obj).


-spec keys(ect()) -> [key()].
keys(Obj)         -> [K || {K, _} <- to_list(Obj)].

%%%_ * Predicates ------------------------------------------------------
-spec is_object(_) -> boolean().
is_object(Obj)     -> is_tuple(Obj) andalso element(1, Obj) =:= dict.

-spec is_value(_)    -> boolean().
is_value(?UNDEFINED) -> false;
is_value(_)          -> true.

-spec is_empty(ect()) -> boolean().
is_empty(Obj)         -> ?MODULE:size(Obj) =:= 0.

-spec size(ect()) -> integer().
size(Obj)         -> dict:size(Obj).

-spec equal(ect(), ect()) -> boolean().
%% @doc Deep object equality.
equal(Obj1, Obj2) ->
  Exists =
    fun(Dict) ->
      fun( Key,  Val, true)  -> {ok, Val} =:= dict:find(Key, Dict);
         (_Key, _Val, false) -> false
      end
    end,
  dict:fold(Exists(Obj1), true, Obj2) andalso
    dict:fold(Exists(Obj2), true, Obj1).

%%%_ * Functionals -----------------------------------------------------
-spec map(fun(), ect())            -> ect().
map(F, Obj) when is_function(F, 1) -> map(fun(_K, V) -> F(V) end, Obj);
map(F, Obj) when is_function(F, 2) -> dict:map(F, Obj).

-spec filter(fun(), ect()) -> ect().
filter(F, Obj) when is_function(F, 1) ->
  filter(fun(_K, V) -> F(V) end, Obj);
filter(F, Obj) when is_function(F, 2) ->
  dict:filter(fun(K, V) -> F(K, V) end, Obj).

-spec fold(fun(), A, ect()) -> A.
fold(F, State, Obj)         -> dict:fold(F, State, Obj).

-spec mapfold(fun(), _, ect()) -> ect().
mapfold(F, State, Obj) when is_function(F, 2) ->
  mapfold(fun(_K, V, Acc) -> F(V, Acc) end, State, Obj);
mapfold(F, State0, Obj0) when is_function(F, 3) ->
  {Obj, _Acc} =
    dict:fold(fun(K, V0, {Obj, Acc0}) ->
                {V, Acc} = F(K, V0, Acc0),
                {dict:store(K, V, Obj), Acc}
              end, {dict:new(), State0}, Obj0),
  Obj.

-spec all(fun(), ect()) -> boolean().
all(F, Obj) -> dict:fold(fun(_K, V, Acc) -> Acc andalso F(V) end, true, Obj).

%%%_ * Object system ---------------------------------------------------
union(Obj1, Obj2) ->
  dict:merge(fun(_Key, Val1, _Val2) -> Val1 end, Obj1, Obj2).

intersection(Obj1, Obj2) ->
  filter(fun(K, _V) -> is_key(Obj2, K) end, Obj1).

difference(Obj1, Obj2) ->
  filter(fun(K, _V) -> not(is_key(Obj2, K)) end, Obj1).

%%%_* Tests ============================================================
-ifdef(TEST).

basic_test() ->
  Obj0  = new(),
  true  = is_empty(Obj0),
  true  = is_object(Obj0),
  false = is_object([{0, foo}]),
  Obj1  = union(Obj0, from_list([foo, 0])),
  false = is_empty(Obj1),
  true  = is_key(Obj1, foo),
  0     = ?MODULE:get(Obj1, foo),
  ?assertError({not_found, bar}, ?MODULE:get(Obj1, bar)),
  0     = ?MODULE:get(Obj1, foo, 0),
  0     = ?MODULE:get(Obj1, bar, 0),
  Obj2  = union(Obj1, from_list([bar, 0])),
  Obj3  = set(Obj2, foo, 1),
  1     = ?MODULE:get(Obj3, foo),
  Obj4  = ?MODULE:map(fun(K, V) -> tulib_atoms:catenate([K, V]) end, Obj3),
  foo1  = ?MODULE:get(Obj4, foo).

multiple_key_access_test() ->
  Obj0        = new(),
  Obj1        = multiset(Obj0,        [{key, val}, {key2, val2}]),
  [val, val2] = ?MODULE:multiget(Obj1, [key, key2]),
  ?assertError({not_found, bar}, ?MODULE:multiget(Obj1, [key, bar])).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
