-module(eorm_delete_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

%% @doc
%% qibinlin@outlook.com
%% created: 2019-3-26
%% @end

all() -> [
    delete_test
].

init_per_suite(Config) ->
    test_utils:init_per_suite(Config).

end_per_suite(Config) ->
    test_utils:end_per_suite(Config).



delete_test(_Config) ->
    Obj = eorm_object:new(post, #{
        <<"user_id">> => 99,
        <<"data">> => <<"test">>
    }),
    SQL = eorm_db:insert(Obj, #{as_sql => true}),
    ct:log("insertSQL: ~p", [SQL]),

    {ok, UpdObj} = eorm_db:insert(Obj),
    ct:log("insertedObj: ~p", [UpdObj]),

    ID = eorm_object:id(UpdObj),

    %%----------------
    SQL2 = eorm_db:delete(post,#{
        as_sql => true,
        where => #{
            id => ID
            %%,user_id => 99
        }
    }),
    ct:log("DeleteSQL: ~p", [SQL2]),

    DeletedResult =  eorm_db:delete(post,#{
        where => #{
            id => ID
            %%,user_id => 99
        }
    }),
    ct:log("DeletedResult: ~p", [DeletedResult]),


    %%----------------

    ok.


