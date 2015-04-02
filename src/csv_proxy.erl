%%% ==================================================================
%%% @author:zhuoyikang
%%% @doc csv模块的进程代理
%%% @end
%%% ==================================================================
-module(csv_proxy).
-behaviour(gen_server).

-export([start_link/0, stop/1, reload/0, reload/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2,code_change/3]).

-record(state, {path}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop(Pid) -> gen_server:cast(Pid, stop).
reload() -> gen_server:call(?MODULE, reload).
reload(Tags) -> gen_server:call(?MODULE, {reload,Tags}).

init([]) ->
  csv_config:start(), {ok, #state{path = ""}}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {no_reply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> io:format("shouldn't terminate !!!!!!"), ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(reload, _From, State) ->
  csv_config:load(),
  {reply, ok, State};

handle_call({reload, Tags}, _From, State) ->
  csv_config:load(Tags),
  {reply, ok, State};


handle_call(_Event, _From, State) -> {noreply, State}.
