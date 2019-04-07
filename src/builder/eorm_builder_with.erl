%% @doc
%% 2019-3-28,add relations
%% @end


-module(eorm_builder_with).

-export([
    build/1
    ,build_sql/1
]).


build(#{'query' := #{with := []}} = State) ->
    State;

build(#{
        entity := FromEntity,
        'query' := #{with := InWith, table := FromTable},
        expr := InExpr} = State) ->

    #{'query' := Query } = State,

    UpdExpr = lists:foldl(
        fun(WithItem, Expr) ->
            case WithItem of
                {Name,WithQuery} ->
                    ToType = eorm_utils:to_binary(Name),
                    build_with({ToType,WithQuery}, FromEntity, FromTable, Expr);
                Name ->
                    ToType = eorm_utils:to_binary(Name),
                    build_with(ToType, FromEntity, FromTable, Expr)
            end

        end,
        InExpr,
        InWith),
    State#{expr => UpdExpr};

build(State) ->
    State.

build_sql(#{expr:=#{joins := []}} = State) ->
    State;
build_sql(#{expr:=#{sql := SQL, joins := Joins} = Expr} = State) ->
    State#{expr => Expr#{sql => SQL ++ Joins}}.


%% @doc modified on 2019-3-28 by linqibin
build_with({InToType, Query}, FromEntity, FromTable, Expr) ->

    case maps:get(relations,FromEntity,undefined) of
        undefined ->
            ToEntity = eorm:get_entity(InToType),
            case maps:get(relations,ToEntity,undefined) of
                undefined ->
                    %%
                    #{relationships := Relationships} = FromEntity,
                    ToType = eorm_utils:to_binary(InToType),
                    Relation = maps:get(ToType, Relationships, undefined),
                    build_relation(Relation, {ToType, Query}, FromEntity, FromTable, Expr);
                Relations ->
                    ToType = eorm_utils:to_binary(InToType),
                    #{type := FromType } = FromEntity,

                    %% {'ExactlyOne',Constraints} =
                    {_, [
                        {normal,Field,ReleateField}
                    ]} = maps:get(eorm_utils:to_atom(FromType), Relations, undefined),

                    build_constraints({'ZeroMore', {Field,ReleateField}}, {ToType, Query}, FromEntity, FromTable, Expr)

            end;

        Relations ->
            ToType = eorm_utils:to_binary(InToType),
            Constraints = maps:get(eorm_utils:to_atom(InToType), Relations, undefined),
            build_constraints(Constraints, {ToType, Query}, FromEntity, FromTable, Expr)

    end;

build_with(InToType, FromEntity, FromTable, Expr) ->
    build_with({InToType, #{}}, FromEntity, FromTable, Expr).

%%=========add on 2019-3-28 begin==========
%% @doc  @private
fnConstraint(FromTable,ToTable,Constraint) ->
    case Constraint of
        {normal,Field,RelatedField } ->
            FieldBin = eorm_utils:to_lower_bin(Field),
            RelatedFieldBin = eorm_utils:to_lower_bin(RelatedField),
            <<FromTable/binary, ".", FieldBin/binary, " = ",
                ToTable/binary, ".", RelatedFieldBin/binary>>;

        {fieldFixed,Field ,Value} ->
            FieldBin = eorm_utils:to_lower_bin(Field),
            ValueBin =
                case is_list(Value) of
                    true ->
                        eorm_utils:to_binary(
                            lists:concat(["'",Value,"'"])
                        );
                    false ->  eorm_utils:to_binary(Value)
                end,
            <<FromTable/binary, ".", FieldBin/binary, " = ",
                ValueBin/binary>>;

        {relatedFieldFixed,RelatedField,Value} ->
            RelatedFieldBin = eorm_utils:to_lower_bin(RelatedField),
            ValueBin =
                case is_list(Value) of
                    true ->
                        eorm_utils:to_binary(
                            lists:concat(["'",Value,"'"])
                        );
                    false ->  eorm_utils:to_binary(Value)
                end,

            <<ToTable/binary, ".", RelatedFieldBin/binary, " = ",
                ValueBin/binary>>

    end.

%% 'has-one' 0..1
build_constraints({'ZeroOne',Constraints}, {ToType, Query}, FromEntity, FromTable, Expr) ->
    #{
        type := FromType,
        pk := FromPk
    } = FromEntity,
    ToEntity = eorm:get_entity(ToType),
    #{
        'query' := #{table := ToTable},
        expr := UpdExpr
    } = eorm_builder_select:build_expr(ToEntity, Query, Expr),

    PreRelations =
        lists:foldl(fun(Contraint,AccIn) ->
            lists:append([AccIn,[fnConstraint(FromTable,ToTable,Contraint)] ])
                    end,[],Constraints),

    PreRelationsBin = eorm_utils:binary_join(PreRelations,<<" AND ">>),
    %% INNER JOIN
    Join = <<" left join  ",
        ToTable/binary, " on ", PreRelationsBin/binary>>,

    #{joins := Joins} = UpdExpr,
    UpdExpr#{joins => Joins ++ [Join]};

%% 'belongs-to' : 1
build_constraints({'ExactlyOne', Constraints}, {ToType, Query}, _FromEntity, FromTable, Expr) ->
    ToEntity = eorm:get_entity(ToType),
    #{pk := ToPk} = ToEntity,

    #{
        'query' := #{table := ToTable},
        expr := UpdExpr
    } = eorm_builder_select:build_expr(ToEntity, Query, Expr),


    PreRelations =
        lists:foldl(fun(Contraint,AccIn) ->
            lists:append([AccIn,[fnConstraint(FromTable,ToTable,Contraint)] ])
                    end,[],Constraints),

    PreRelationsBin = eorm_utils:binary_join(PreRelations,<<" AND ">>),
    Join = <<" INNER JOIN ",
        ToTable/binary, " on ", PreRelationsBin/binary>>,


    #{joins := Joins} = UpdExpr,
    UpdExpr#{joins => Joins ++ [Join]};

%%{Kind,Field,RelatedField } 'has-many': ZeroMore 0..n
build_constraints({'ZeroMore', {Field,RelationKey} }, {ToType, Query}, _FromEntity, _FromTable, #{extra_query := ExtraQuery} = Expr) ->
    Expr#{extra_query => ExtraQuery ++ [{ToType, RelationKey, Query}]
        ,extra_info => #{id_field => Field}};


build_constraints(undefined, {ToType, _Query}, #{type:=FromType} = _FromEntity, _FromTable, _Expr) ->
    throw({no_relationConstraint, {FromType, ToType}}).

%%=========add on 2019-3-28 end============

build_relation({'belongs-to', RelationKey}, {ToType, Query}, _FromEntity, FromTable, Expr) ->
    ToEntity = eorm:get_entity(ToType),
    #{pk := ToPk} = ToEntity,

    #{
        'query' := #{table := ToTable},
        expr := UpdExpr
    } = eorm_builder_select:build_expr(ToEntity, Query, Expr),

    Join = <<"left join ",
        ToTable/binary, " on ",
        FromTable/binary, ".", RelationKey/binary, " = ",
        ToTable/binary, ".", ToPk/binary>>,

    #{joins := Joins} = UpdExpr,
    UpdExpr#{joins => Joins ++ [Join]};


build_relation({'has-one', RelationKey}, {ToType, Query}, FromEntity, FromTable, Expr) ->
    #{
        type := FromType,
        pk := FromPk
    } = FromEntity,
    ToEntity = eorm:get_entity(ToType),
    #{
        'query' := #{table := ToTable},
        expr := UpdExpr
    } = eorm_builder_select:build_expr(ToEntity, Query, Expr),

    Join = <<"left join ",
        ToTable/binary, " on ",
        FromTable/binary, ".", FromPk/binary, " = ",
        ToTable/binary, ".", RelationKey/binary>>,

    #{joins := Joins} = UpdExpr,
    UpdExpr#{joins => Joins ++ [Join]};

build_relation({'has-many', RelationKey}, {ToType, Query}, _FromEntity, _FromTable, #{extra_query := ExtraQuery} = Expr) ->
    Expr#{extra_query => ExtraQuery ++ [{ToType, RelationKey, Query}]};

build_relation(undefined, {ToType, _Query}, #{type:=FromType} = _FromEntity, _FromTable, _Expr) ->
    throw({no_relationship, {FromType, ToType}}).