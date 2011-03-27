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

%% @doc @todo
dispatch(Pid, ProtocolOp, MessageID, BindDN, From) ->
    gen_server:cast(Pid, {ProtocolOp, MessageID, BindDN, From}).

%% @doc @todo
bind(BindDN, {simple, Password}) ->
    Filter = {equalityMatch, 
	      {'AttributeValueAssertion', "userPassword", Password}},
    search(BindDN, BindDN, baseObject, 1, Filter, []);
bind(_BindDN,_Creds) ->
    authMethodNotSupported.

%% @doc @todo
bind_reply(_From, BindResult,_MessageID) when is_atom(BindResult) ->
    BindResult;
bind_reply(_From, [],_MessageID) ->
    invalidCredentials;
bind_reply(From, [BindResult],_MessageID) when is_list(BindResult) ->
    BindDN = bitstring_to_list(object_get("dn", BindResult)),
    ldap_fsm:set_bind(From, BindDN),
    success.

%% @doc @todo
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

%% @doc @todo
search_reply(_From, SearchResult,_MessageID) when is_atom(SearchResult) ->
    SearchResult;
search_reply(From, [Item|Result], MessageID) ->
    Attrs = lists:flatten(lists:map(fun(I) -> item_to_attribute(I) end, Item)),
    {value, {_, "dn", [DN]}, PartialAttrs} = lists:keytake("dn", 2, Attrs),
    Entry = {'SearchResultEntry', DN, PartialAttrs},
    ldap_fsm:reply(From, {{searchResEntry, Entry}, MessageID}),
    search_reply(From, Result, MessageID);
search_reply(_From, [],_MessageID) ->
    success.

%% @doc @todo
modify_dn(_BindDN, DN, NewRDN,_DeleteOldRDN) ->
    case emongo:find_one(eds, ?COLL, [{"_rdn", rdn(DN)}]) of
	[] -> noSuchObject;
	[Entry] ->
	    OldDN = bitstring_to_list(object_get(<<"dn">>, Entry)),
	    BaseDN = lists:dropwhile(fun(C) -> C =/= $, end, OldDN),
	    NewDN = NewRDN ++ BaseDN,	   
	    ModDN = object_modify(<<"dn">>, NewDN, Entry),
	    NewEntry = object_modify(<<"_rdn">>, rdn(NewDN), ModDN),
	    Response = emongo:update_sync(eds, ?COLL, [{<<"_rdn">>, rdn(DN)}], NewEntry, false),
	    parse_response(Response)
    end.	

%% @doc @todo
add(_BindDN, DN, Attrs) ->
    case emongo:find_one(eds, ?COLL, [{"_rdn", rdn(DN)}]) of
	[_Entry] -> entryAlreadyExists;
	[] ->
	    Entry = lists:map(fun(A) -> attribute_to_item(A) end, Attrs),
	    AddDN = object_insert(<<"dn">>, DN, Entry),
	    NewEntry = object_insert(<<"_rdn">>, rdn(DN), AddDN),	    
	    Response = emongo:insert_sync(eds, ?COLL, NewEntry),
	    parse_response(Response)
    end.

%% @doc @todo
delete(_BindDN, DN) ->
    case emongo:find_one(eds, ?COLL, [{"_rdn", rdn(DN)}]) of
	[] -> noSuchObject;
	[_Entry] ->
	    Response = emongo:delete_sync(eds, ?COLL, [{<<"_rdn">>, rdn(DN)}]),
	    parse_response(Response)
    end.    

%% @doc @todo
modify(_BindDN, DN, Attrs) ->
    case emongo:find_one(eds, ?COLL, [{"_rdn", rdn(DN)}]) of
	[] -> noSuchObject;
	[Entry] ->
	    success
    end.    

%% @doc @todo
parse_response([Response]) ->
    case lists:keyfind(<<"err">>, 1, Response) of
	{<<"err">>, undefined} -> success;
	_Else -> protocolError
    end.

%% @doc @todo
object_modify(Key, NewValue, Entry) when is_list(Key) ->
    object_modify(list_to_bitstring(Key), NewValue, Entry);
object_modify(Key, NewValue, Entry) when is_bitstring(Key),
					 is_list(NewValue) ->
    object_modify(Key, list_to_bitstring(NewValue), Entry);
object_modify(Key, NewValue, Entry) when is_bitstring(Key),
					 is_bitstring(NewValue) ->
    lists:keyreplace(Key, 1, Entry, {Key, NewValue}).

%% @doc @todo
object_get(Key, Entry) when is_list(Key) ->
    object_get(list_to_bitstring(Key), Entry);
object_get(Key, Entry) when is_bitstring(Key) ->
    element(2, lists:keyfind(Key, 1, Entry)).

%% @doc @todo
object_insert(Key, Value, Entry) when is_list(Key) ->
    object_insert(list_to_bitstring(Key), Value, Entry);
object_insert(Key, Value, Entry) when is_bitstring(Key),
				      is_list(Value) ->
    object_insert(Key, list_to_bitstring(Value), Entry);
object_insert(Key, Value, Entry) when is_bitstring(Key),
				      is_bitstring(Value) ->
    [{Key, Value} | Entry].

%% @doc @todo
object_delete(Key, Entry) when is_list(Key) ->
    object_get(list_to_bitstring(Key), Entry);
object_delete(Key, Entry) when is_bitstring(Key) ->
    lists:keydelete(Key, 1, Entry).

%% @doc @todo
rdn(DN) ->
    lists:reverse(DN).

%% @doc @todo
item_to_attribute({_Name, {oid, _}}) ->
    [];
item_to_attribute({<<"_rdn">>,_}) ->
    [];
item_to_attribute({Name, Value}) when is_bitstring(Name),
				      is_bitstring(Value) ->
    {'PartialAttribute', bitstring_to_list(Name), [bitstring_to_list(Value)]};
item_to_attribute({Name, {array, Value}}) when is_bitstring(Name), 
					       is_list(Value) ->
    lists:map(fun(V) -> 
		      {'PartialAttribute', bitstring_to_list(Name), [bitstring_to_list(V)]}
	      end, Value).

%% @doc @todo
attribute_to_item({'PartialAttribute', Name, [Value]}) when is_list(Name),
							    is_list(Value) ->
    {list_to_bitstring(Name), list_to_bitstring(Value)};
attribute_to_item({'PartialAttribute', Name, Value}) when is_list(Name),
							  is_list(Value) ->
    {list_to_bitstring(Name), {array, [list_to_bitstring(V) || V <- Value]}}.

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
    {'AddRequest', DN, Attributes} = Options,
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
    Result = modify_dn(BindDN, DN, NewRDN, DeleteOldRDN),
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
