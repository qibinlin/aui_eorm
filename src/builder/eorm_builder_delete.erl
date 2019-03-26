%%%-------------------------------------------------------------------
%%% @author ericolin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 三月 2019 上午10:39
%%%-------------------------------------------------------------------
-module(eorm_builder_delete).
-autor("qibinlin@outlook.com").


% https://www.postgresql.org/docs/11/sql-delete.html
% [ WITH [ RECURSIVE ] with_query [, ...] ]
% DELETE FROM [ ONLY ] table_name [ * ] [ [ AS ] alias ]
% [ USING using_list ]
% [ WHERE condition | WHERE CURRENT OF cursor_name ]
% [ RETURNING * | output_expression [ [ AS ] output_name ] [, ...] ]


%% API
-compile([export_all]).


new_expr() ->
  #{
    fields => []
    ,joins => []
    ,where => []
    ,bindings => []
    ,extra_query => []
    ,sql => [<<"DELETE">>]
  }.

build(Entity, Query) ->
  build(Entity, Query, new_expr()).

build(Entity, InQuery, Expr) ->
  Query = eorm:cb_prepare_statement(Entity, {delete, InQuery}),
  erlz:do(
    build_expr(Entity, Query, Expr), [
       fun eorm_builder_where:build_sql/1
      ,fun eorm_builder:build_sql/1
    ]).

build_expr(Entity, Query, Expr) ->
  State = #{
    entity => Entity,
    'query' => Query,
    expr => Expr
  },
  build_expr(State).

build_expr(State) ->
  erlz:do(State,[
    % build expr
    fun get_table/1
    ,fun build_from_sql/1
    ,fun eorm_builder_where:build/1
  ]).

get_table(#{entity := Entity, 'query' := Query} = State) ->
  Table = eorm:get_table(Entity, {select, Query}),
  State#{'query' => Query#{table => Table}}.

build_from_sql(#{'query' := #{table := Table}, expr := #{sql := SQL} = Expr} = State) ->
  State#{expr => Expr#{sql => SQL ++ [<<"from ", Table/binary>>]}}.
