-module(ts).

-export([
    t1/0,t2/0

]).

-define(MAX_CHUNKS, 2).

t2_1() ->

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
        pk => recid,
        relations =>
            #{ purchLine => {'has-many',purchId}}
    }),



    eorm:def_entity(purchLine, #{
        db_connection => Conn1,
        table => purchline,
        fields =>[purchId,inventDimId,itemId],
        pk => recid,
        relations =>
            #{  inventDim => {'has-one' , [
                %%{Kind,Field,RelatedField }
                {normal ,inventDimId,inventDimId}
                ]},

                purchTable => {'belongs-to', [
                    {normal,purchId,purchId}
                ]}
            }

    }).

t2() ->
    ts_helper:init_per_suite([]),

    t2_1(),

    Query = #{
        with => [inventDim],
        where => #{
            recid => 5637296829
        }
    },
    {ok, SQL} = eorm_db:select(purchLine, Query#{as_sql => true}),
    io:format("purchLine SQL: ~p ~n", [SQL]),

    {ok, Obj} = eorm_db:select(purchLine, Query),
    io:format("purchLine Obj: ~p ~n", [Obj]),



    %% purchTable
    Query2 =#{

        with => [purchLine],
        where => #{
            recid => 22565440432
        }
    },
    {ok, SQL2} = eorm_db:select(purchTable,Query2#{as_sql => true} ),

    io:format("purchTable SQL2: ~p ~n", [SQL2]),

    {ok, Obj2} = eorm_db:select(purchTable, Query2),
    io:format("purchTable Obj2: ~p ~n", [Obj2]).



t1() ->
    ts_helper:init_per_suite([]),

    {ok, [UserObj]} = eorm_db:select(
        user, #{
            with => [post],
            where => #{
                id => 1
            }
        }),
    io:format("UserObj: ~p", [UserObj]),
    ok.
%%====================