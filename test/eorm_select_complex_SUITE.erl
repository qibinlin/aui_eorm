-module(eorm_select_complex_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() -> [
     select_has_one_test
    ,select_belongs_to_test
    ,select_relates_has_many_test

].

init_per_suite(Config) ->
    test_utils:init_per_suite(Config).


end_per_suite(Config) ->
    test_utils:end_per_suite(Config).


select_belongs_to_test(_Config) ->
    Query = #{
        with => ["PurchTable","InventDimPurch"],
        where => #{
            recid => 5637296829
        },
        meta => #{dataSources =>[
            {"PurchTable",purchTable},
            {"InventDimPurch",inventDim},
            {"PurchLine",purchLine}
        ] }
    },
    {ok, SQL} = eorm_db:select("PurchLine", Query#{as_sql => true}),
    ct:log("SQL: ~p", [SQL]),

    {ok, Obj} = eorm_db:select("PurchLine", Query),
    ct:log("Obj: ~p", [Obj]),

    DataSet = eorm_object:to_dataSet(Obj),

    ct:log("ModelCollections: ~p", [DataSet]),
    ok.


select_has_one_test(_Config) ->
    Query = #{
        with => [inventDim],
        where => #{
            recid => 5637296829
        }
    },
    {ok, SQL} = eorm_db:select(purchLine, Query#{as_sql => true}),
    ct:log("SQL: ~p", [SQL]),

    {ok, Obj} = eorm_db:select(purchLine, Query),
    ct:log("Obj: ~p", [Obj]),

    DataSet = eorm_object:to_dataSet([Obj]),

    ct:log("ModelCollections: ~p", [DataSet]),

    ok.



select_relates_has_many_test(_Config) ->
    Query =#{
        with => [
            {purchLine,#{with =>[inventDim]}
            }
        ],
        where => #{
            recid => 22565440432
        }
    },
    {ok, SQL} = eorm_db:select(purchTable,Query#{as_sql => true} ),
    ct:log("SQL: ~p", [SQL]),

    {ok, Obj} = eorm_db:select(purchTable, Query),
    ct:log("Obj: ~p", [Obj]),


    DataSet = eorm_object:to_dataSet([Obj]),

    ct:log("ModelCollections: ~p", [DataSet]),

    ok.
