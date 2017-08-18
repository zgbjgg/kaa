-module(kaa_main_worker).

-behaviour(gen_server).

-include("kaa_core.hrl").

%% API
-export([start_link/0,
    get_worker/1,
    kaa_proto_in/2,
    stop_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% the values can be override during initialization
-record(state, {jun_worker = undefined :: pid(),
    mon_ref = undefined :: reference(),
    storage = undefined :: atom(),
    key = <<"kaa">> :: binary()}).

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    % register process using syn
    Key = pid_to_list(Pid),
    ok = syn:register(Key, Pid),
    {ok, Key}.

get_worker(Key) ->
    Pid = syn:find_by_key(Key),
    gen_server:call(Pid, get_worker).

kaa_proto_in(Key, PBMsg) ->
    Pid = syn:find_by_key(Key),
    gen_server:call(Pid, {kaa_proto_in, PBMsg}, infinity).

stop_link(Key) ->
    Pid = syn:find_by_key(Key),
    gen_server:call(Pid, stop_link).

init([]) ->
    % start the py process and initializes its importing modules
    case jun_worker:start_link() of
        {ok, JunPid} ->
            MonRef = erlang:monitor(process, JunPid),
            lager:info("initialized jun worker pid ~p", [JunPid]),
            Key = pid_to_list(self()),
            Storage = ets:new(?KAA_ENVIRONMENT(Key), [named_table, public]),
            {ok, #state{jun_worker = JunPid, mon_ref = MonRef, key = Key,
                storage = Storage}};
        Error      ->
            lager:error("cannot initializes jun worker due to ~p", [Error]),
            {stop, Error}
    end.

handle_call(get_worker, _From, State)            ->
    JunPid = State#state.jun_worker,
    Worker = kaa_proto:worker(JunPid),
    {reply, {ok, Worker}, State};

handle_call({kaa_proto_in, PBMsg}, _From, State) ->
    Result = kaa_proto:exec(PBMsg),    
    {reply, {ok, Result}, State};

handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};
 
handle_call(_Request, _From, State) ->    
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, _Type, _Object, _Info}, State=#state{mon_ref = MonRef}) ->
    % process py pid is down, which one is the process to restart?
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % terminate the jun worker process
    JunPid = State#state.jun_worker,
    ok = jun_worker:stop_link(JunPid),
    % also removes syn register & ets
    Key = State#state.key,
    true = ets:delete(?KAA_ENVIRONMENT(Key)),
    ok = syn:unregister(Key),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================
