%%%-------------------------------------------------------------------
%%% @author ericolin
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% ref:Pure Key/Value database library for Erlang Applications
%%%  https://github.com/refuge/cowdb
%%% @end
%%% Created : 23. 九月 2018 上午11:16
%%%-------------------------------------------------------------------
-module(eorm_kv).
-author("qibinlin@outlook.com").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MasterTrackingId,key_masterTrackingId).

-record(state, {
  dbPid = undefined

}).

-type transact_id() :: integer() | tx_end.


-export([put/2,get/1,delete/1]).

-export([masterTrackingId/0]).

%%%===================================================================
%%% API
%%%===================================================================

masterTrackingId() ->
  do_call({masterTrackingId}).


%% @doc add one object to a store
-spec put( term(), any()) -> {ok, transact_id()} | {error, term()}.
put(Key,Val) ->
  do_call({put,Key,Val}).


%% @doc get an object by the specified key
-spec get(Key::any()) -> any() | not_found.
get( Key) ->
  do_call({get,Key}).

%% @doc delete one object from the store
-spec delete( term()) -> {ok, transact_id()} | {error, term()}.
delete( Key) ->
  do_call({delete,Key}).




%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Dir) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Dir], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Dir) ->

  Filename = "dbinner.kvdb",

  File = filename:join([Dir,"storages", "kvdb",Filename]),

  {ok, Pid} = cowdb:open(File),

  {ok, #state{dbPid = Pid }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

%% {masterTrackingId}
handle_call({masterTrackingId}, _From, #state{dbPid = Pid} = State) ->

  OldVal =
    case  cowdb:get(Pid,?MasterTrackingId) of
      {ok,{_Key,Val}} -> Val ;
      _ -> 1
    end,
  ValNew = OldVal + 1,
  cowdb:put(Pid,?MasterTrackingId,ValNew),

  {reply, eorm_utils:to_binary(ValNew), State};


%% [put/2,get/1,delete/1]).
handle_call({put,Key,Val}, _From, #state{dbPid = Pid} = State) ->
  Ret = cowdb:put(Pid,Key,Val),
  {reply, Ret, State};

handle_call({get,Key}, _From, #state{dbPid = Pid} = State) ->

  Ret =
  case  cowdb:get(Pid,Key) of
    {ok,{_Key,Val}} -> Val;
    _ -> not_found
  end,
  {reply, Ret, State};


handle_call({delete,Key}, _From, #state{dbPid = Pid} = State) ->
  Ret = cowdb:delete(Pid,Key),
  {reply, Ret, State};


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{dbPid = Pid} = _State) ->
  cowdb:close(Pid),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


do_call(Request) ->
  gen_server:call(?SERVER,Request).
