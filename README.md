      @@@@@@  @@@@@@   @@@@@@  @@@@@@@  @@@@@@@   @@@@@@  @@@  @@@
     !@@     @@!  @@@ @@!  @@@ @@!  @@@ @@!  @@@ @@!  @@@ @@!  !@@
      !@@!!  @!@  !@! @!@!@!@! @!@@!@!  @!@!@!@  @!@  !@!  !@@!@!
         !:! !!:  !!! !!:  !!! !!:      !!:  !!! !!:  !!!  !: :!!
     ::.: :   : :. :   :   : :  :       :: : ::   : :. :  :::  :::

## Overview
SoapBox is a framework for implementing RPC-style interfaces to
network services.

It also provides somewhat sophisticated support for input validation
and an opinion on how to exchange data between different components of
a large Erlang system.

A SoapBox interface has several components:

* The transport is responsible for shuffling bytes to and from the
  service via some network connection.

* A stub is responsible for marshalling and unmarshalling data it
  receives from the transport, dispatch, logging, and optionally guiding
  the evaluation of an RPC call in certain ways.

* Each call is implemented by a method. Methods follow standard Erlang
  conventions and may be shared across transports and stubs. Methods
  have an input-signature based on types. Types provide code
  which the SoapBox type-checker uses to determine type-membership of
  input values.

  Transport_0 ... Transport_N ... Transport_M
        \        /  \           /
         \      /    \         /
          \    /      \       /
          Stub_0 ...   Stub_N
            |  \      /  |
            |   \    /   |
            |    \  /    |
            |     \/     |
            |     /\     |
        Method_0 ... Method_N
            |   \    /   |
            |    \  /    |
            |     \/     |
            |     /\     |
          Type_0 ... Type_1

  Figure1: Code-reuse enabled by SoapBox

A stub may handle the data-conversion required by one or more
transports. Each Method may be dispatched to by one or more
stubs. Types may be used by one or more Methods.

Input is converted into SoapBox-objects as soon as possible and kept in
that format from there on out.

SoapBox was written by Jakob Sievers, with contributions by
* Daniel K. Lee (/dklee) (routing)
* Kevin Albrecht (/onlyafly) (documentation)

## Writing Components
### Methods
Methods provide code to type-check arguments, validate, and execute an
RPC call.

spec/1:
  Accepts one argument: Args. Args is a list of arguments passed to the
  method.

  Must return a SoapBox object-literal describing the desired
  input-object: keys are argument names and values are type
  declarations.

  Type declarations are tuples, and should consist of these terms:
  1. The value to typecheck.
  2. The SoapBox type of the value.
  3. Optionally, a SoapBox object-literal giving the type-parameters.

  Example:
  [ title, { json:json_get("title", Args), foo_type_string },
    price, { json:json_get("price", Args), foo_type_float,
             [min, 0.0, max, 100.0]
           }
  ]

verify/1:
  Accepts one argument: Args. Args is a SoapBox object containing the
  arguments, as parsed and typechecked by the spec/1 function.

  The function should verify that the method as called is valid. This
  could include checking that the caller has passed a valid security
  hash, etc.

  Should return the atom 'ok' if the call is valid, or {error,
  ErrorMessage} if it is not.

  TODO: a future release of SoapBox will include support for declaring
  and checking method signatures.

call/1:
  Accepts one argument: Args. Args is a SoapBox object containing the
  arguments, as parsed and typechecked by the spec/1 function.

  The function should perform the actual work of the method call.

  Returns {ok, Result}  or {error, ErrorMessage}.

### Stubs
eval/5:
  Accepts five arguments: CallFunction, ReturnFunction, Method, Args,
  and Context.
  - CallFunction is a function which should be called during the eval
    function to execute the method and get the return value. It takes no
    arguments.
  - ReturnFunction is a function which should be called at the end of
    the eval function to pass the return value back to SoapBox. The
    value passed to the ReturnFunction function can be the actual
    return value from the CallFunction or an error.
  - Method is an atom giving the name of the method that is being evaluated.
  - Args is an value containing the arguments. This might be a list of
    argument values, or an webserver-specific object used to retrieve
    the arguments from a POST request, for example.
  - Context is application-specific data related to the call.

method/1:
  Accepts one argument: MethodName. MethodName is an atom giving the
  name of the RPC method.

  Returns an atom giving the module name of the module implementing
  the given RPC method. The module should implement the soapbox_method
  behavior.

unpack/1:
  Accepts one argument: Args.
  - Args is an value containing the arguments, as received from the
    transport. This might be a list of argument values, or a
    webserver-specific object used to retrieve the arguments from a
    POST request, for example.

  Returns the arguments in a format to be passed to the SoapBox method.

ok/4:
  Accepts four arguments: Result, Method, Args, and Context.
  - Result is the result of the method call.
  - Method is the method that is returning.
  - Args is a value containing the arguments, as originally passed to
    the stub.
  - Context is application-specific data related to the call.

  Returns the result converted into a format that can be returned to
  the caller via the transport.

error/4:
  Accepts four arguments: Reason, Method, Args, and Context.
  - Reason is the error reason returned by a method call.
  - Method is the method that is returning.
  - Args is a value containing the arguments, as originally passed to
    the stub.
  - Context is application-specific data related to the call.

pack/1:
  Accepts one argument: Response.
  - Response is the response from the method call.

  Returns the response in a format to be returned to the transport.

log_call/3:
  Accepts three arguments: Method, Args, Context.
  - Method is an atom giving the name of the method that is being
    called.
  - Args is an value containing the arguments, as received from the
    transport.
  - Context is application-specific data related to the call.

log_return/5:
  Called when returning an annotated response.

  Accepts five arguments: Response, Annotation, Method, Args, and
  Context.
  - Response is the response as returned from the method call.
  - Annotation is the annotation added to the response.
  - Method is an atom giving the name of the method that is returning
    from a call.
  - Args is a list of arguments which was passed to the method.
  - Context is application-specific data related to the call.

log_ok/4:
  Used for logging successful 'ok' responses. See ok/4 for parameters.

log_error/4:
  Used for logging unsuccessful 'error' responses. See error/4 for
  parameters.

log_response/4:
  Used for logging all responses, regardless of failure or success.
  See ok/4 for parameters.

### Transports
TODO Document the functions for this behavior:
  start/0
  start_link/0
  stop/0
  start/1
  start_link/1
  stop/1

### Types
#### Introduction
Types should implement one of these behaviors:
  soapbox_type_alias
  soapbox_type_list
  soapbox_type_object
  soapbox_type_primitive

The following functions should be exported by your type's module
regardless of which behavior you implement. Unless otherwise stated,
whenever a function takes Params as an argument, Params is a SoapBox
object containing the type parameters.

convert/2:
  Accepts two arguments: Val and Params. Val is the value in the type
  as it is received from the normalize function.

  Returns the value converted into the format used internally by the
  application.

name/1:
  Accepts one argument: Params.

  Returns the name of the type as an atom.

normalize/2:
  Accepts two arguments: Val and Params. Val is the value in the type
  as it is received from the stub.

  Returns the value normalized into a format used for type-checking
  (for example, in the spec or validate functions).

parameters/0:
  Returns a list specifying the type parameters. Each type parameter
  specified in the list should be one of the following:
  1. A tuple specifying the parameter name as and atom and the default
     value for the parameter.
  2. An atom specifying the parameter name.

#### Primitive Types
Primitive types describe simple values like integers, strings,
floating-point values, etc.

validate/2:
  Accepts two arguments: Val and Params. Val is the actual primitive
  value being typed, after normalization.

  Returns 'true' if the value is valid, according to whatever the
  constraints the SoapBox type wishes to enforce.

#### Object Types
Object types describe complex types containing multiple fields, where
each field can be described as another SoapBox type.

spec/2:
  Accepts two arguments: Val and Params. Val is a value
  representing the value in the type as it is received from the stub,
  after normalization. Val may be a SoapBox object or another complex
  type, depending on the output from the stub.

  Returns an object-literal describing the desired output value.
  C.f. soapbox_method's spec/1.

#### List Types
List types are complex types which contain any number of elements of
the same type.

element_type/1:
  Accepts one argument: Params.

  Returns an atom giving the name of the SoapBox type which describes
  the elements of the list.

#### Alias Types
Alias types are aliases for other types, and therefore only implement
one function, rewrite/0.

rewrite/0:
  Returns a tuple describing the type that this serves as an alias to.
  The first element of the tuple is an atom giving the name of the
  type to rewrite to. The second element is a list giving arguments
  to the type.

  Example of a typical return value:
  { foo_type_float, [min, 32.0, max, 212.0] }

## Installation
jakob@hesitant.primat.es:~/git/klarna/soapbox$ gmake
jakob@hesitant.primat.es:~/git/klarna/soapbox$ gmake test

## Manifest
include/:
soapbox_test.hrl           -- assert macros

src/:
soapbox.erl                -- API and general control-flow
soapbox_method.erl         -- Method behaviour
soapbox_obj.erl            -- Object ADT
soapbox_stub.erl           -- Stub behaviour
soapbox_test.erl           -- Utility functions for writing test cases
soapbox_transport.erl      -- Transport behaviour
soapbox_type.erl           -- Type checker
soapbox_type_alias.erl     -- Type-alias behaviour
soapbox_type_list.erl      -- List-type behaviour
soapbox_type_object.erl    -- Object-type behaviour
soapbox_type_primitive.erl -- Primitive-type behaviour
