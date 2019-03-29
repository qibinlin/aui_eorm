-module(ts_helper).


-define(MAX_CHUNKS, 2).
-compile([export_all]).

init_per_suite(Config) ->
    application:ensure_all_started(eorm),
    application:ensure_all_started(epgsql),
    %%init_db(),
    init().

end_per_suite(Config) ->
   %% ?config(eorm_owner, Config) ! stop,
    clean_db(),
    ok.

new_connection() ->
    epgsql:connect(
        "127.0.0.1", "dbuser", "dbpassword", [
            {database, "testdb"},
            {timeout, 4000}
    ]).

init_db() ->
    {ok, Conn} = new_connection(),
    {ok, SQLSchema} = file:read_file(
        filename:join([code:lib_dir(eorm), "priv",  "schema.sql"])),

    Result = erlz:error_traverse(
        fun(Query) ->
            case
                epgsql:equery(Conn, Query, [])
            of
                {error, Error} -> {error, Error};
                _ -> {ok, ok}
            end
        end,
        binary:split(SQLSchema, <<";\n">>, [global])),
    case Result of
        {error, Error} ->
            clean_db(),
            io:format("Init db fail ~p", [Error]);
        _ -> ok
    end.

clean_db() ->
    {ok, Conn} = new_connection(),
    _Result = epgsql:squery(Conn, "drop owned by dbuser cascade"),
    ok.



init() ->
    eorm:init(),
    % % TODO
    % eorm:def_db(test_db #{
    %     adapter => adapter_epgsql,
    %     host => "127.0.0.1",
    %     database => "testdb",
    %     user => "dbuser",
    %     password => "dbpassword",
    % }),

    % % TODO
    % eorm:def_db(test_db #{
    %     adapter => adapter_epgsql_pool,
    %     pool_name => test_pool
    % }),
    {ok, Conn1} = epgsql:connect(
        "127.0.0.1", "dbuser", "dbpassword", [
            {database, "testdb"},
            {timeout, 4000}
        ]),
    {ok, Conn2} = epgsql:connect(
        "127.0.0.1", "dbuser", "dbpassword", [
            {database, "testdb"},
            {timeout, 4000}
        ]),

    eorm:def_entity(user, #{
        db_connection => Conn1,
        table => users,
        pk => id,
        'has-many' => [post],
        'has-one' => [email]
    }),

    eorm:def_entity(email, #{
        db_connection => Conn1,
        table => user_emails,
        pk => id,
        'belongs-to' => [user]
    }),

    eorm:def_entity(post, #{
        db_connection => fun({_Kind, _Query}) ->
            lists:nth(crypto:rand_uniform(1,3), [Conn1, Conn2])
                         end,
        table => fun({_Kind, Query}) ->
            % Kind :: reflect | select | update | insert | delete
            case Query of
                #{meta := #{chunk_id := ChunkId}} ->
                    list_to_binary(
                        lists:flatten(
                            io_lib:format(<<"posts_~2..0B">>, [ChunkId])));
                _ ->
                    <<"posts">>
            end
                 end,
        pk => id,
        'belongs-to' => [
            user,
            % and the same but with specifying of field
            {user, user_id}
        ],
        'has-many' => [
            {post_action_log, action_post_id}
        ],
        'transform-from' => #{
            'db' => #{
                <<"data">> => fun decompress/1
            }
        },
        'transform-to' => #{
            'db' => #{
                <<"data">> => fun compress/1
            },
            json => [
                fun(Obj) ->
                    Obj#{meta => #{
                        <<"info">> => <<"example of transformation whole object">>}}
                end,
                #{
                    <<"created_at">> => fun(DateTime) ->
                        {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
                        iolist_to_binary(
                            io_lib:format(
                                "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0fZ",
                                [Year, Month, Day, Hour, Min, Sec]))
                                        end
                }
            ]
        },
        callbacks => #{
            prepare_obj_statement => fun(Obj, {_Kind, Query}) ->
                % Kind :: update | insert | delete
                UserId = eorm_object:get_attr(<<"user_id">>, Obj),
                ChunkId = ((UserId + 1) rem ?MAX_CHUNKS) + 1,
                Query#{meta => #{chunk_id => ChunkId}}
                                     end,
            prepare_statement => fun({_Kind, Query}) ->
                % Kind :: select
                case Query of
                    #{where := #{user_id := UserId}} ->
                        ChunkId = ((UserId + 1) rem ?MAX_CHUNKS) + 1,
                        Query#{meta => #{chunk_id => ChunkId}};
                    _ ->
                        Query
                    %throw({partitioning_condition, "Could not to detect chunk id"})
                end
                                 end
        }
    }),


    eorm:def_entity(post_action_log, #{
        db_connection => Conn1,
        table => post_actions_log,
        pk => id,
        'has-one' => [post]
    }),
    ok.



compress(Data) ->
    compress(Data, zip).

decompress(Data) ->
    compress(Data, unzip).

compress(null, _Flag) ->
    null;
compress(Data, Flag) ->
    case Flag of
        zip ->
            zlib:zip(Data);
        unzip ->
            zlib:unzip(Data)
    end.