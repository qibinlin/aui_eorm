-module(eorm_utils).

-export([
    binary_join/2
    ,to_binary/1
    ,to_str/1,to_lower_bin/1
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
