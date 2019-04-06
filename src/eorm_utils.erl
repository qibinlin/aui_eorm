-module(eorm_utils).

-export([
    binary_join/2
    ,to_binary/1
    ,to_str/1,to_lower_bin/1,to_atom/1
]).

to_lower_bin(Val) ->
    to_binary(string:to_lower(eorm_utils:to_str(Val))).

-spec to_str(any()) -> string().
to_str(Val) when is_binary(Val) ->
    binary_to_list(Val);

to_str(Val) when is_list(Val) ->
    Val;
to_str(Val) when is_atom(Val) ->
    atom_to_list(Val).


-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join([Head|Tail], Sep) ->
    lists:foldl(
        fun (Value, Acc) -> <<Acc/binary, Sep/binary, Value/binary>> end,
        Head, Tail).

to_binary(Value) when is_integer(Value) ->
    erlang:integer_to_binary(Value);
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value).



-spec to_atom(Data :: term()) -> atom().
to_atom(Data) when is_binary(Data) ->
    binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
    list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
    list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
    Data.
