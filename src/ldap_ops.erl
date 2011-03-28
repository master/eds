%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc LDAP Operations

-module(ldap_ops).

-behaviour(gen_server).

-export([start_link/0, dispatch/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("LDAP.hrl").

-define(COLL, "root").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_) ->
    {ok, {}}.

%% @doc Dispatch a message to an ldap_ops worker. From is a Pid of a message sender (used for reply).
%% @spec dispatch(Pid, ProtocolOp, MessageID, BindDN, From) -> ok
dispatch(Pid, ProtocolOp, MessageID, BindDN, From) ->
    gen_server:cast(Pid, {ProtocolOp, MessageID, BindDN, From}).

%% @doc Process BindRequest
bind(BindDN, {simple, Password}) ->
    Filter = {equalityMatch, 
	      {'AttributeValueAssertion', "userPassword", Password}},
    search(BindDN, BindDN, baseObject, 1, Filter, []);
bind(_BindDN,_Creds) ->
    authMethodNotSupported.

%% @doc Process a reply from bind/2 and notify FSM on new BindDN if required.
bind_reply(_From, BindResult,_MessageID) when is_atom(BindResult) ->
    BindResult;
bind_reply(_From, [],_MessageID) ->
    invalidCredentials;
bind_reply(From, [BindResult],_MessageID) when is_list(BindResult) ->
    BindDN = bitstring_to_list(ldap_obj:get("dn", BindResult)),
    ldap_fsm:set_bind(From, BindDN),
    success.

%% @doc Process SearchRequest
search(undefined,_BaseObject,_Scope,_SizeLimit,_Filter,_Attributes) ->
    insufficientAccessRights;
search(_BindDN, BaseObject, Scope, SizeLimit, Filter, Attributes) ->
    ScopeFilter = ldap_filter:scope(BaseObject, Scope),
    EntryFilter = ldap_filter:filter(Filter),
    FieldsOption = ldap_filter:fields(Attributes),
    LimitOption = ldap_filter:limit(SizeLimit),
    emongo:find_all(eds, ?COLL, 
		    ScopeFilter ++ EntryFilter,
		    FieldsOption ++ LimitOption).

%% @doc Process a reply from search/6, send result entries to FSM if required.
search_reply(_From, SearchResult,_MessageID) when is_atom(SearchResult) ->
    SearchResult;
search_reply(From, [Item|Result], MessageID) ->
    Attrs = lists:flatten(lists:map(fun(I) -> ldap_obj:to_attr(I) end, Item)),
    {value, {_, "dn", [DN]}, PartialAttrs} = lists:keytake("dn", 2, Attrs),
    Entry = {'SearchResultEntry', DN, PartialAttrs},
    ldap_fsm:reply(From, {{searchResEntry, Entry}, MessageID}),
    search_reply(From, Result, MessageID);
search_reply(_From, [],_MessageID) ->
    success.

%% @doc Process ModifyDNRequest
modifydn(_BindDN, DN, NewRDN,_DeleteOldRDN) ->
    case emongo:find_one(eds, ?COLL, [{"_rdn", rdn(DN)}]) of
	[] -> noSuchObject;
	[Entry] ->
	    OldDN = bitstring_to_list(ldap_obj:get(<<"dn">>, Entry)),
	    BaseDN = lists:dropwhile(fun(C) -> C =/= $, end, OldDN),
	    NewDN = NewRDN ++ BaseDN,	   
	    ModDN = ldap_obj:modify(<<"dn">>, NewDN, Entry),
	    NewEntry = ldap_obj:modify(<<"_rdn">>, rdn(NewDN), ModDN),
	    Response = emongo:update_sync(eds, ?COLL, [{<<"_rdn">>, rdn(DN)}], NewEntry, false),
	    parse_response(Response)
    end.	

%% @doc Process AddRequest
add(_BindDN, DN, Attrs) ->
    case emongo:find_one(eds, ?COLL, [{"_rdn", rdn(DN)}]) of
	[_Entry] -> entryAlreadyExists;
	[] ->
	    Entry = lists:map(fun(A) -> ldap_obj:to_record(A) end, Attrs),
	    AddDN = ldap_obj:insert(<<"dn">>, DN, Entry),
	    NewEntry = ldap_obj:insert(<<"_rdn">>, rdn(DN), AddDN),	    
	    Response = emongo:insert_sync(eds, ?COLL, NewEntry),
	    parse_response(Response)
    end.

%% @doc Process DelRequest
delete(_BindDN, DN) ->
    case emongo:find_one(eds, ?COLL, [{"_rdn", rdn(DN)}]) of
	[] -> noSuchObject;
	[_Entry] ->
	    Response = emongo:delete_sync(eds, ?COLL, [{<<"_rdn">>, rdn(DN)}]),
	    parse_response(Response)
    end.    

%% @doc Process ModifyRequest
modify(_BindDN, DN, Attrs) ->
    case emongo:find_one(eds, ?COLL, [{"_rdn", rdn(DN)}]) of
	[] -> noSuchObject;
	[Entry] ->
	    NewEntry = lists:foldl(fun(C, E) -> modify_apply(C, E) end, Entry, Attrs),
	    Response = emongo:update_sync(eds, ?COLL, [{<<"_rdn">>, rdn(DN)}], NewEntry, false),
	    parse_response(Response)
    end.    

%% @doc Apply ModifyRequest change atoms to an object
modify_apply({'ModifyRequest_changes_SEQOF', add, Change}, Entry) ->
    {Key, Value} = ldap_obj:to_record(Change),
    ldap_obj:insert(Key, Value, Entry);
modify_apply({'ModifyRequest_changes_SEQOF', replace, Change}, Entry) ->
    {Key, Value} = ldap_obj:to_record(Change),
    ldap_obj:modify(Key, Value, Entry);
modify_apply({'ModifyRequest_changes_SEQOF', delete, Change}, Entry) ->
    {Key,_Value} = ldap_obj:to_record(Change),
    ldap_obj:delete(Key, Entry).

%% @doc Parse eMongo response code
parse_response([Response]) ->
    case lists:keyfind(<<"err">>, 1, Response) of
	{<<"err">>, undefined} -> success;
	_Else -> protocolError
    end.

%% @doc Create a reveresed DN from a DN
%% @spec rdn(DN) -> DN
%%       DN -> list()
rdn(DN) ->
    lists:reverse(DN).

handle_cast({{bindRequest, Options}, MessageID,_BindDN, From}, State) ->
    {'BindRequest',_, BindDN, Creds} = Options,
    BindResult = bind(BindDN, Creds),
    Result = bind_reply(From, BindResult, MessageID),
    Response = #'BindResponse'{resultCode = Result, matchedDN = "", diagnosticMessage = ""},
    ldap_fsm:reply(From, {{bindResponse, Response}, MessageID}),
    {stop, normal, State};

handle_cast({{searchRequest, Options}, MessageID, BindDN, From}, State) ->
    {'SearchRequest', BaseObject, Scope,_, SizeLimit,_,_, Filter, Attributes} = Options,
    SearchResult = search(BindDN, BaseObject, Scope, SizeLimit, Filter, Attributes),
    Result = search_reply(From, SearchResult, MessageID),
    Response = #'LDAPResult'{resultCode = Result, matchedDN = "", diagnosticMessage = ""},
    ldap_fsm:reply(From, {{searchResDone, Response}, MessageID}),
    {stop, normal, State};

handle_cast({{modifyRequest, Options}, MessageID, BindDN, From}, State) ->
    {'ModifyRequest', DN, Attributes} = Options,
    Result = modify(BindDN, DN, Attributes),
    Response = #'LDAPResult'{resultCode = Result, matchedDN = "", diagnosticMessage = ""},
    ldap_fsm:reply(From, {{modifyResponse, Response}, MessageID}),
    {stop, normal, State};

handle_cast({{addRequest, Options}, MessageID, BindDN, From}, State) ->
    {'AddRequest', DN, Attributes} = Options,
    Result = add(BindDN, DN, Attributes),
    Response = #'LDAPResult'{resultCode = Result, matchedDN = "", diagnosticMessage = ""},
    ldap_fsm:reply(From, {{addResponse, Response}, MessageID}),
    {stop, normal, State};

handle_cast({{delRequest, Options}, MessageID, BindDN, From}, State) ->
    DN = Options,
    Result = delete(BindDN, DN),
    Response = #'LDAPResult'{resultCode = Result, matchedDN = "", diagnosticMessage = ""},
    ldap_fsm:reply(From, {{delResponse, Response}, MessageID}),
    {stop, normal, State};

handle_cast({{modDNRequest, Options}, MessageID, BindDN, From}, State) ->
    {'ModifyDNRequest', DN, NewRDN, DeleteOldRDN,_} = Options,
    Result = modifydn(BindDN, DN, NewRDN, DeleteOldRDN),
    Response = #'LDAPResult'{resultCode = Result, matchedDN = "", diagnosticMessage = ""},
    ldap_fsm:reply(From, {{modDNResponse, Response}, MessageID}),    
    {stop, normal, State};

handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

handle_call(Request,_From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_info({'EXIT',_, Reason}, State) ->
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok, State}.
