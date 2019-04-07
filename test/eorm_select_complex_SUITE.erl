-module(eorm_select_complex_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() -> [
     select_has_one_test
    ,select_belongs_to_test
    ,select_relates_has_many_test
    ,select_relates_has_many_with_dataSourceName_test

].

init_per_suite(Config) ->
    test_utils:init_per_suite(Config).


end_per_suite(Config) ->
    test_utils:end_per_suite(Config).


select_belongs_to_test(_Config) ->
    Query = #{
        fields =>[purchId,itemId],
        with => [
            {purchTable,#{fields =>[purchId,purchName],ds => "PurchTable_ds"}},
            {inventDim,#{fields =>[inventDimId,inventBatchId],ds =>"InventDimPurch"}}],
        where => #{
            recid => 5637296829
        },
        ds => "PurchLine_ds"
    },

    {ok, SQL} = eorm_db:select(purchLine, Query#{as_sql => true}),
    ct:log("SQL: ~p", [SQL]),

    {ok, Obj} = eorm_db:select(purchLine, Query),
    ct:log("Obj: ~p", [Obj]),

    DataSet = eorm_object:to_dataSet(Obj),

    ct:log("DataSet: ~p", [DataSet]),
    ok.


select_has_one_test(_Config) ->
    Query = #{
        with => [{inventDim,#{ds =>"InventDimPurch"}}],
        where => #{
            recid => 5637296829
        }
        ,ds =>"PurchLine_ds"
    },
    {ok, SQL} = eorm_db:select(purchLine, Query#{as_sql => true}),
    ct:log("SQL: ~p", [SQL]),

    {ok, Obj} = eorm_db:select(purchLine, Query),
    ct:log("Obj: ~p", [Obj]),

    DataSet = eorm_object:to_dataSet([Obj]),

    ct:log("DataSet: ~p", [DataSet]),

    ok.


select_relates_has_many_test(_Config) ->
    Query =#{
        fields => [purchId,purchName],
        with => [
            {purchLine,#{
                fields => [purchId,itemId],
                with =>[
                      {inventDim,#{fields =>[inventDimId,inventBatchId]}
                      }
               ]}
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



select_relates_has_many_with_dataSourceName_test(_Config) ->
    Query =#{
        fields => [purchId,purchName],
        with => [
            {purchLine,#{
                fields => [purchId,itemId],
                ds => "PurchLine_ds",
                with =>[
                    {inventDim,#{
                        fields =>[inventDimId,inventBatchId],
                        ds => "InventDimPurch"

                    }}
                ]}
            }

        ],
        where => #{
            recid => 22565440432
        },
        ds => "PurchTable_ds"

    },

    {ok, SQL} = eorm_db:select(purchTable,Query#{as_sql => true} ),
    ct:log("SQL: ~p", [SQL]),

    {ok, Obj} = eorm_db:select(purchTable, Query),
    ct:log("Obj: ~p", [Obj]),


    DataSet = eorm_object:to_dataSet([Obj]),

    ct:log("DataSet: ~p", [DataSet]),

    ok.
