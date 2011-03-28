%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc LDAP Client FSM

-module(ldap_fsm).

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2, reply/2, set_bind/2]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4]).

-export([listen/2, read/2]).

-record(state, {socket, addr, ops, binddn}). 

-define(TIMEOUT, 120000).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

reply(Pid, Message) when is_pid(Pid)  ->
    gen_fsm:send_event(Pid, {out, Message}).

set_bind(Pid, BindDN) when is_pid(Pid)  ->
    gen_fsm:send_event(Pid, {set_bind, BindDN}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, listen, #state{binddn=undefined, ops=bush:init()}}.

listen({socket_ready, Socket}, State) when is_port(Socket) ->
    inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, read, State#state{socket=Socket, addr=IP}, ?TIMEOUT};

listen(Other, State) ->
    error_logger:error_msg("Unexpected message: ~p\n", [Other]),
    {next_state, listen, State}.

read({set_bind, BindDN}, State) ->
    {next_state, read, State#state{binddn=BindDN}, ?TIMEOUT};

read({in, {{abandonRequest, Options},_}}, #state{ops=Ops} = State) ->
    {'AbandonRequest', MessageID} = Options,
    case bush:take_k(MessageID, Ops) of
	{MessageID, Pid, NewOps} -> exit(Pid, 'EXIT');
	false -> NewOps = Ops
    end,
    {next_state, read, State#state{ops=NewOps}, ?TIMEOUT};

read({in, {{unbindRequest,_},_}}, State) ->
    {stop, normal, State};

read({in, {ProtocolOp, MessageID}}, #state{ops=Ops, binddn=BindDN} = State) ->
    {ok, Pid} = eds_app:start_ops(),
    NewOps = bush:insert(MessageID, Pid, Ops),
    erlang:link(Pid),
    ldap_ops:dispatch(Pid, ProtocolOp, MessageID, BindDN, self()),
    {next_state, read, State#state{ops=NewOps}, ?TIMEOUT};

read({out, Message}, #state{socket=S} = State) ->
    Bytes = list_to_binary(ldap_msg:encode(Message)),
    gen_tcp:send(S, Bytes),
    {next_state, read, State, ?TIMEOUT};

read(timeout, State) ->
    error_logger:error_msg("Client connection timeout: ~p\n", [State]),
    {stop, normal, State};

read(_Data,  State) ->
    {stop, normal, State}.

handle_event(Event, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(Event, _From, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

handle_info({tcp, S, Bin}, StateName, #state{socket=S} = State) ->
    inet:setopts(S, [{active, once}]),
    ?MODULE:StateName({in, ldap_msg:decode(Bin)}, State);

handle_info({tcp_closed,_S}, _StateName, State) ->
    {stop, normal, State};

handle_info({bind, BindDN}, StateName, State) ->
    {next_state, StateName, State#state{binddn=BindDN}};

handle_info({'EXIT', Pid,_}, StateName, #state{ops=Ops} = State) ->
    NewOps = bush:delete_v(Pid, Ops),
    {next_state, StateName, State#state{ops=NewOps}};

handle_info(_Info, StateName, State) ->
    {noreply, StateName, State}.

terminate(_Reason,_StateName, #state{socket=S}) ->
    (catch gen_tcp:close(S)),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
