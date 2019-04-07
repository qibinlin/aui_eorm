-module(ts).

-export([
    t1/0

]).

-define(MAX_CHUNKS, 2).



%% @doc @private
def_entity() ->

    {ok, Conn1} = epgsql:connect(
        "127.0.0.1", "dbuser", "dbpassword", [
            {database, "testdb"},
            {timeout, 4000}
        ]),

    eorm:def_entity(inventDim, #{
        db_connection => Conn1,
        table => inventdim,
        fields =>[inventBatchId,inventDimId],
        pk => recid
    }),

    eorm:def_entity(prodTable, #{
        db_connection => Conn1,
        table => prodTable,
        fields =>[prodId,itemRefType],
        pk => recid
    }),

    eorm:def_entity(purchTable, #{
        db_connection => Conn1,
        table => purchTable,
        fields =>[purchId,purchName],
        pk => recid

    }),



    eorm:def_entity(purchLine, #{
        db_connection => Conn1,
        table => purchline,
        fields =>[purchId,inventDimId,itemId],
        pk => recid,
        relations =>
        #{  inventDim => {'ZeroOne', [
            %%{Kind,Field,RelatedField }
            {normal ,inventDimId,inventDimId}
        ]},

            purchTable => {'ExactlyOne', [
                {normal,purchId,purchId}
            ]}
        }

    }).

t1() ->
    ts_helper:init_per_suite([]),

    def_entity(),
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

%%====================