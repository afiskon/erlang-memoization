-module(erlymemo).

-behaviour(gen_server).

%% API
-export([
        start_link/0,
        call/3,
        clean/0,
        used_space/0,
        free_space/0,
        table_max_size/0
    ]).

%% gen_server callbacks
-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-record(state, {
        lru_ets :: ets:tab(),
        curr_unique_id :: non_neg_integer(),
        free_space :: non_neg_integer()
    }).

-define(INVALID_UNIQUE_ID, 0).
-define(START_UNIQUE_ID, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, _} | ignore.
% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec call(atom(), atom(), [_]) -> _ | no_return().
call(Module, Function, Args) ->
    MemoKey = erlang:md5(term_to_binary({Module, Function, Args})),
    case ets:lookup(?MODULE, MemoKey) of
        [{_MemoKey, _LruUniqId, Result}] ->
            ok = gen_server:call(?MODULE, {update_lru, MemoKey}),
            Result;
        [] ->
            Result = erlang:apply(Module, Function, Args),
            ok = gen_server:call(?MODULE, {memoize_and_update_lru, MemoKey, Result}),
            Result
    end.

-spec clean() -> ok.
clean() ->
    gen_server:call(?MODULE, clean).

-spec used_space() -> non_neg_integer() | no_return().
used_space() ->
    Size = ets:info(?MODULE, size),
    case Size of
        undefined -> throw({not_started, ?MODULE});
        _ -> Size
    end.

-spec free_space() -> non_neg_integer() | no_return().
free_space() ->
    {ok, FreeSpace} = gen_server:call(?MODULE, free_space),
    FreeSpace.

-spec table_max_size() -> non_neg_integer() | no_return().
table_max_size() ->
    {ok, MaxSize} = gen_server:call(?MODULE, table_max_size),
    MaxSize.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #state{}}.
% @private
init([]) ->
    ets:new(?MODULE, [set, named_table, protected]),
    {ok, #state{
            lru_ets = ets:new(lru_ets, [ordered_set, private]),
            free_space = config_max_table_size(),
            curr_unique_id = ?START_UNIQUE_ID
         }}.

-spec handle_call(_, _, #state{}) -> {reply, ok, #state{}}.
% @private
handle_call(
        {memoize_and_update_lru, MemoKey, Result}, _From,
        #state{ free_space = FreeSpace } = State) ->
    PrevUniqId =
        case ets:lookup(?MODULE, MemoKey) of
            [{ _MemoKey, Found, _Result }] -> Found;
            [] -> ?INVALID_UNIQUE_ID
        end,
    NewState = memoize_and_update_lru(MemoKey, Result, PrevUniqId, State),
    {reply, ok, delete_expired(NewState#state{ free_space = FreeSpace - 1 })};

handle_call({update_lru, MemoKey}, _From, State) ->
    NewState =
        case ets:lookup(?MODULE, MemoKey) of
            [{_MemoKey, PrevUniqId, Result}] ->
                memoize_and_update_lru(MemoKey, Result, PrevUniqId, State);
            [] -> % rare case: MemoKey was just deleted from ETS
                State
        end,
    {reply, ok, NewState };

handle_call(clean, _From, #state{ lru_ets = LruEts } = State) ->
    ets:delete_all_objects(?MODULE),
    ets:delete_all_objects(LruEts),
    {reply, ok, State#state{ free_space = config_max_table_size(), curr_unique_id = ?START_UNIQUE_ID } };

handle_call(free_space, _From, #state{ free_space = FreeSpace } = State) ->
    {reply, { ok, FreeSpace }, State};

handle_call(table_max_size, _From, #state{ free_space = FreeSpace } = State) ->
    Size = ets:info(?MODULE, size),
    {reply, {ok, Size + FreeSpace}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(_, #state{}) -> {noreply, #state{}}.
% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_, #state{}) -> {noreply, #state{}}.
% @private
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_, #state{}) -> ok.
% @private
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, #state{}, _) -> {ok, #state{}}.
% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec memoize_and_update_lru(binary(), _, non_neg_integer(), #state{}) -> #state{}.
memoize_and_update_lru(
        MemoKey, Result, PrevUniqId,
         #state{
            lru_ets = LruEts,
            curr_unique_id = CurrUniqId
         } = State) ->
    ets:delete(LruEts, PrevUniqId),
    ets:insert(LruEts, { CurrUniqId, MemoKey }),
    ets:insert(?MODULE, { MemoKey, CurrUniqId, Result}),
    State#state{ curr_unique_id = CurrUniqId + 1 }.

-spec delete_expired(#state{}) -> #state{}.
delete_expired(#state{ free_space = FreeSpace } = State)
        when FreeSpace >= 0 ->
    State;

delete_expired(#state{ lru_ets = LruEts } = State) ->
    LruKey = ets:first(LruEts),
    [{_LruKey, MemoKey}] = ets:lookup(LruEts, LruKey),
    ets:delete(LruEts, LruKey),
    ets:delete(?MODULE, MemoKey),
    State#state{ free_space = 0 }.

-spec config_max_table_size() -> non_neg_integer().
config_max_table_size() ->
    {ok, MaxSize} = application:get_env(?MODULE, table_max_size),
    MaxSize.
