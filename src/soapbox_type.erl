%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc SoapBox type behaviour and typechecker.
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
-module(soapbox_type).

%%%_* Exports ==========================================================
%% API
-export([ check/1
        , check_val/2
        , get_error/2
        , raise/1
        , spec_value/1
        , spec_type/1
        , spec_args/1
        ]).

%% Test support
-export([ assert_conversion_error/1
        , assert_internal_error/1
        , assert_normalization_error/1
        , assert_validation_error/1
        ]).

%% Behaviour
-export([behaviour_info/1]).

%% Internal exports
-export([ subst_def/2
        , subst_imp/2
        ]).

-ignore_xref([ subst_def/2
             , subst_imp/2
             , assert_conversion_error/1
             , assert_internal_error/1
             , assert_normalization_error/1
             , assert_validation_error/1
             ]).

%% Accessors for a spec
-ignore_xref([ spec_value/1
             , spec_type/1
             , spec_args/1
             ]).

%%%_* Includes =========================================================
-define(ASSERTIONS, true).
%%-define(DEBUG, true).

-include_lib("tulib/include/logging.hrl").
-include_lib("tulib/include/prelude.hrl").
-include_lib("tulib/include/types.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%_* Code =============================================================
%%%_ * Behaviour -------------------------------------------------------
%% -callback convert(_, _)   -> _.
%% -callback name(_)         -> atom().
%% -callback normalize(_, _) -> _.
%% -callback parameters()    -> [atom() | {atom(), _}].

behaviour_info(callbacks) ->
  [ {convert,    2}
  , {name,       1}
  , {normalize,  2}
  , {parameters, 0}
  ];
behaviour_info(_) -> undefined.

%%%_ * Preliminaries ---------------------------------------------------
%% Users provide a list which describes a SoapBox object:
%%   [ FieldName::atom(), FieldSpec::field_spec()
%%   , ...
%%   ]
%%
-type type()        :: atom() | {atom(), [_]}.
-type args()        :: [_].
-type sumtype()     :: [type()].
-type field_spec()  :: {_, type() | sumtype()} | {_, type(), args()}.
-type declaration() :: [atom() | field_spec()].

%% The internal representation is slightly more structured.
-record(spec,
        { val    :: _                 %value to be checked
        , type   :: atom()            %declared type
        , args   :: soapbox_obj:ect() %explicit arguments
        , params :: soapbox_obj:ect() %further parameters required by type
        }).

-record(val,{ue::_}). %private constructor


-spec check(declaration()) -> maybe([soapbox_obj:ect()], _).
check(Decl) ->
  case ?lift(parse(preprocess(Decl))) of
    {ok, Objs}   -> do_check(Objs);
    {error, Rsn} -> make_error(declaration, Rsn)
  end.


-spec preprocess(declaration()) -> [soapbox_obj:ect(tuple())].
preprocess(Decl) -> expand(canonicalize(soapbox_obj:from_list(Decl))).


%% Rewrite type aliases. Note that alias-types don't have parameters.
canonicalize(Obj) ->
  soapbox_obj:map(
    fun({V, Ts}) when is_list(Ts) -> {V, [do_canonicalize(T) || T <- Ts]};
       ({V, T})                   -> {V, do_canonicalize(T)};
       ({V, T, As})               -> {V, do_canonicalize({T, As})}
    end, Obj).

do_canonicalize(T) when is_atom(T) ->
  case is_alias(T) of
    true  -> do_canonicalize(rewrite(T));
    false -> {T, soapbox_obj:new()}
  end;
do_canonicalize({T, As}) ->
  case soapbox_obj:is_object(As) of
    true  -> {T, As};
    false -> {T, soapbox_obj:from_list(As)}
  end.

is_alias(T) -> constructor(T) =:= soapbox_type_alias.


%% Expand tree of alternative type propositions.
expand(Obj) ->
  case alts(Obj) of
    []   -> [Obj];
    Alts -> lists:concat([expand(A) || A <- Alts])
  end.

alts(Obj) ->
  case soapbox_obj:pick(soapbox_obj:filter(fun is_sumtype/1, Obj)) of
    {Field, {Val, Types}} ->
      [soapbox_obj:set(Obj, Field, {Val, T}) || T <- Types];
    undefined -> []
  end.

is_sumtype({_V, {T, _As}}) when is_atom(T)  -> false;
is_sumtype({_V, Ts})       when is_list(Ts) -> true.


-spec parse([soapbox_obj:ect(tuple())]) -> [soapbox_obj:ect(#spec{})].
parse(Decls) -> [soapbox_obj:map(fun make_spec/1, D) || D <- Decls].

make_spec({Val, {Type, Args}}) ->
  Params0 = soapbox_obj:from_proplist(parameters(Type)),
  Params  = soapbox_obj:difference(Params0, Args),
  #spec{ val    = Val
       , type   = Type
       , args   = Args
       , params = Params
       }.

-spec check_val(_, type()) -> maybe(_, _).
check_val(Val, Type) ->
  tulib_maybe:lift_with(check([val, {Val, Type}]),
                        fun(Obj) -> soapbox_obj:get(Obj, val) end).

%%%_ * Utilities -------------------------------------------------------
%% Behaviour accessors.
convert(Type, Val, Args)   -> Type:convert(Val, Args).
element_type(Type, Args)   -> Type:element_type(Args).
name(Type, Args)           -> Type:name(Args).
normalize(Type, Val, Args) -> Type:normalize(Val, Args).
parameters(Type)           -> Type:parameters().
rewrite(Type)              -> Type:rewrite().
spec(Type, Val, Args)      -> Type:spec(Val, Args).
validate(Type, Val, Args)  -> Type:validate(Val, Args).

constructor(Type) ->
  [Constructor] = [B || B <- behaviours(Type), is_constructor_behaviour(B)],
  Constructor.

behaviours(Type) ->
  lists:flatten([Vs || {K, Vs} <- Type:module_info(attributes),
                       is_behaviour_attribute(K)]).

is_behaviour_attribute(behaviour) -> true;
is_behaviour_attribute(behavior)  -> true;
is_behaviour_attribute(_)         -> false.

is_constructor_behaviour(soapbox_type_alias)     -> true;
is_constructor_behaviour(soapbox_type_list)      -> true;
is_constructor_behaviour(soapbox_type_object)    -> true;
is_constructor_behaviour(soapbox_type_primitive) -> true;
is_constructor_behaviour(_)                      -> false.

%%%_ * Algorithm -------------------------------------------------------
%% Type-checking succeeds iff there's a fixed point at which the object
%% is fully instantiated.
-spec do_check([soapbox_obj:ect(#spec{})]) -> maybe(soapbox_obj:ect(), _).
do_check(Alts) ->
  do_check(Alts, []).

do_check([Alt|Alts], Errs) ->
  case ?lift(fixpoint(Alt)) of %Exns caught here
    {ok, Obj} ->
      case is_instantiated(Obj) of
        true  -> {ok, unwrap(Obj)};
        false -> do_check(Alts, [make_error(missing_params, Obj)|Errs])
      end;
    {error, _} = Err -> do_check(Alts, [Err|Errs])
  end;
do_check([], Errs) -> make_error(untypable, lists:reverse(Errs)).


%% In each step, we attempt to reduce one or more specs to values, or
%% extend one or more specs with missing type-arguments.
fixpoint(Obj0) ->
  Obj = reduce(Obj0),
  case soapbox_obj:equal(Obj0, Obj) of
    true  -> Obj;
    false -> fixpoint(Obj)
  end.

reduce(Obj) ->
  do_reduce(fun maybe_type/3,
            do_reduce(fun subst_def/2,
                      do_reduce(fun subst_imp/2, Obj))).

do_reduce(F, Obj) ->
  soapbox_obj:map(
    fun( Field, #spec{} = Spec)  ->
        if is_function(F, 2) -> F(Spec, Obj);
           is_function(F, 3) -> F(Spec, Obj, Field)
        end;
       (_Field, #val{}  = Value) -> Value
    end, Obj).

%% Type-arguments may be passed explicitly (in the declaration) or
%% implicitly (from within the same object), or associated with default
%% values by the type callback.
%% Explicit arguments take precedence over implicit arguments which take
%% precedence over default arguments.
subst_imp(#spec{args=Args0, params=Params0} = Spec, Obj) ->
  Implicit0 = unwrap(soapbox_obj:filter(fun is_value/1, Obj)),
  Implicit  = soapbox_obj:intersection(Implicit0, Params0),
  Params    = soapbox_obj:difference(Params0, Implicit),
  Args      = soapbox_obj:union(Args0, Implicit),
  Spec#spec{args=Args, params=Params}.

subst_def(#spec{args=Args0, params=Params0} = Spec, Obj) ->
  Params1 = soapbox_obj:difference(Params0, Obj), %non-implicit params
  Defs    = soapbox_obj:filter(fun soapbox_obj:is_value/1, Params1),
  Params  = soapbox_obj:difference(Params0, Defs),
  Args    = soapbox_obj:union(Args0, Defs),
  Spec#spec{args=Args, params=Params}.

maybe_type(#spec{} = Spec, _Obj, Field) ->
  case is_parameterized(Spec) of
    true  -> #val{ue=type(Spec, Field)};
    false -> Spec
  end.

is_parameterized(#spec{params=Params}) -> soapbox_obj:is_empty(Params).

-define(EXN, '__soapbox_type_exn__').
type(#spec{val=_Val, type=Type, args=Args} = Spec, Field) ->
  try
    ?debug("typing ~p :: ~p(~p)", [_Val, Type, Args]),
    typecheck(Spec)
  catch
    throw:{?EXN, Err} -> throw(Err);
    _:Exn             -> throw(make_error(name(Type, Args), Field, Exn))
  end.

typecheck(#spec{val=Val0, type=Type, args=Args}) ->
  Cons = constructor(Type),
  Val1 = normalize(Type, Val0, Args),
  Val2 = do_typecheck(Cons, Val1, Type, Args),
  Val  = convert(Type, Val2, Args),
  Val.

do_typecheck(soapbox_type_primitive, Val, Type, Args) ->
  true = validate(Type, Val, Args),
  Val;
do_typecheck(soapbox_type_object, Val, Type, Args) ->
  propagate(check(spec(Type, Val, Args)));
do_typecheck(soapbox_type_list, Val, Type, Args) ->
  %% Note that Args is already a soapbox object here.
  [propagate(check_val(V, {element_type(Type, Args), Args})) || V <- Val].

propagate({ok, Val})        -> Val;
propagate({error, _} = Err) -> do_raise(Err).


is_instantiated(Obj) -> soapbox_obj:all(fun is_value/1, Obj).

is_value(#val{})  -> true;
is_value(#spec{}) -> false.

unwrap(#val{ue=Val}) -> Val;
unwrap(Obj)          -> soapbox_obj:map(fun unwrap/1, Obj).

%%%_ * Error tuples ----------------------------------------------------
make_error(Name, Rsn) ->
  make_error(Name, no_field, Rsn).
make_error(Name, Field, Rsn) ->
  {error, {type, {Name, Field, Rsn, erlang:get_stacktrace()}}}.

get_error(K, Err) -> e(K, Err).

e(K, {error, Rsn}                             ) -> e(K, Rsn);
e(K,         {type, Rsn}                      ) -> e(K, Rsn);
e(K,                {untypable, _, [Rsn|_], _}) -> e(K, Rsn);
e(name,             {N,         _, _,       _}) -> N;
e(field,            {_,         F, _,       _}) -> F;
e(rsn,              {_,         _, R,       _}) -> R;
e(trace,            {_,         _, _,       T}) -> T.

raise(Name)   -> do_raise(make_error(Name, raise)).
do_raise(Err) -> throw({?EXN, Err}).

%% Functions to access various portions of the specs in a soapbox_obj
spec_value({Val, _})    -> Val;
spec_value({Val, _, _}) -> Val.

spec_type({_, Type})    -> Type;
spec_type({_, Type, _}) -> Type.

spec_args({_, _})       -> undefined;
spec_args({_, _, Args}) -> Args.


%%%_ * Test support ----------------------------------------------------
assert_normalization_error(Err) -> normalize = get_callback(Err).
assert_validation_error(Err)    -> validate  = get_callback(Err).
assert_conversion_error(Err)    -> convert   = get_callback(Err).
assert_internal_error(Err)      -> internal  = get_callback(Err).

get_callback(Err) -> inspect(get_error(trace, Err)).

inspect(Trace) ->
  Functions = [ frame_get(Frame, f) || Frame <- Trace],
  case lists:filter(fun is_callback/1, Functions) of
    []           -> internal;
    [Callback|_] -> Callback
  end.

frame_get({M, _, _},    m)    -> M;
frame_get({M, _, _, _}, m)    -> M; %R15 and above
frame_get({_, F, _},    f)    -> F;
frame_get({_, F, _, _}, f)    -> F; %R15 and above
frame_get({_, _, A},    a)    -> A;
frame_get({_, _, A, _}, a)    -> A. %R15 and above

is_callback(convert)      -> true;
is_callback(element_type) -> true;
is_callback(name)         -> true;
is_callback(normalize)    -> true;
is_callback(parameters)   -> true;
is_callback(rewrite)      -> true;
is_callback(spec)         -> true;
is_callback(validate)     -> true;
is_callback(_)            -> false.

%%%_* Tests ============================================================
-ifdef(TEST).

-define(obj(Lst), soapbox_obj:from_list(Lst)).
-define(eq(Obj1, Obj2), (true = soapbox_obj:equal(Obj1, Obj2))).


prim_test() -> {ok, 2} = check_val(2, prim_type_test).


typable_test() ->
  {ok, Res} = check(typable_spec([1, ?obj([ delta,  0
                                          , modulo, 2
                                          ])])),
  Expected  = ?obj([ modulo, 2
                   , obj,    ?obj([ delta,  2
                                  , modulo, {2}
                                  ])
                   ]),
  ?eq(Res, Expected).

typable_spec([PrimVal, ObjPval]) ->
  [ modulo, {PrimVal, prim_type_test} %implicit param
  , obj,    {ObjPval, obj_ptype_test}
  ].


untypable_test() ->
  {error, Rsn}   = check_val(64, {prim_ptype_test, [modulo, 8]}),
  missing_params = get_error(name, Rsn).


typeerror_test() ->
  {error, Rsn} = check_val(?obj([ delta,  3
                                , modulo, 4
                                ]),
                          {obj_ptype_test, [modulo, 2]}),
  prim_type    = get_error(name, Rsn),
  assert_validation_error(Rsn).


recursion_test() ->
  {ok, Res} = check_val(?obj([result, 4]), {obj_rtype_test, [n, 3]}),
  Expected  = ?obj([result,
                    ?obj([result,
                          ?obj([result,
                                ?obj([result, 2])])])]),
  ?eq(Res, Expected).


stupid_test() ->
  _ = behaviour_info(callbacks),
  _ = behaviour_info(foo).


default_test() -> {ok, defval} = check_val(42, prim_dtype_test).


another_default_test() ->
  {ok, Res} = check([ foo, {0, prim_dtype1_test}
                    , bar, {0, prim_dtype2_test}
                    , baz, {0, prim_type_test}
                    ]),
  Expected  = ?obj([ foo, 2
                   , bar, 2
                   , baz, 2
                   ]),
  ?eq(Res, Expected).


sumtypable_test() -> {ok, 2} = check_val(2, [prim_ptype_test, prim_type_test]).


another_sumtypable_test() ->
  {ok, {0}} = check_val(3, [ {prim_ptype_test, [modulo, 1, delta, 0]}
                           , prim_type_test
                           ]).


sumtype_error_test() ->
  {error, Rsn}   = check_val(foo, [prim_ptype_test, prim_type_test]),
  missing_params = get_error(name, Rsn),
  assert_internal_error(Rsn).


listobj_test() ->
  Obj         = ?obj([val, 2]),
  {ok, CObj}  = check_val(Obj, obj_int_type_test),
  List        = [Obj, Obj, Obj],
  {ok, CList} = check_val(List, listobj_type_test),
  CList       = [CObj, CObj, CObj].


alias_test() -> {ok, 2} = check_val(0, alias_type_test).


raise_test() ->
  {error, Rsn} = check_val(foo, raising_type_test),
  myexn        = get_error(name, Rsn).


decl_error_test() ->
  {error, Rsn}                     = check(decl()),
  declaration                      = get_error(name, Rsn),
  {lifted_exn, function_clause, _} = get_error(rsn, Rsn),
  do_canonicalize                  = last_fun(get_error(trace, Rsn)).

decl() -> [frob, {0, [[prim_type_test], prim_dtype_test]}].

last_fun([Frame|_]) -> frame_get(Frame, f).


error_field_test() ->
  {error, Rsn} = check([ foo, {2, prim_type_test}
                       , bar, {3, prim_type_test}
                       ]),
  bar          = get_error(field, Rsn).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
