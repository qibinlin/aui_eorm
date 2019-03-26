-module(adapter_epgsql).

-export([
    exec_query/3
]).

-include_lib("epgsql/include/epgsql.hrl").

rows_to_proplist(Cols, Rows) ->
    lists:map(
        fun(Row) ->
            lists:zipwith(
                fun(#column{name=K}, V) -> {K, V} end,
                Cols, tuple_to_list(Row))
        end,
        Rows).

exec_query(Conn, Query, Bindings) ->
    QueryResult =
        case Conn of
            undefined -> pgapp:equery(Query, Bindings);
            _ ->epgsql:equery(Conn, Query, Bindings)
        end,
    case
        QueryResult
    of
        {ok, Count, Cols, Rows} ->
            {ok, {Count, rows_to_proplist(Cols, Rows)}};
        {ok, _Cols, []} ->
            {ok, []};
        {ok, Cols, Rows} ->
            {ok, rows_to_proplist(Cols, Rows)};
        {ok, Count} ->
            {ok, {Count, []}};
        {error, Error} ->
            {error, Error}
    end.