%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc LDAP Operations

-module(ldap_ops).

-behaviour(gen_server).

-export([start_link/0, dispatch/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("LDAP.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_) ->
    {ok, {}}.

dispatch(Pid, ProtocolOp, MessageID, BindDN, From) ->
    gen_server:cast(Pid, {ProtocolOp, MessageID, BindDN, From}).

bind(BindDN, {simple, Password}) ->
    Filter = {equalityMatch, 
	      {'AttributeValueAssertion', "userPassword", Password}},
    {ok, Res} = search(BindDN, BindDN, baseObject, 1, Filter, []),
    case length(Res) of
	0 -> {error, invalid_credentials};
	_ -> {ok, BindDN}
    end;
bind(_BindDN,_Creds) ->
    {error, unsupported_method}.

search(undefined,_BaseObject,_Scope,_SizeLimit,_Filter,_Attributes) ->
    {error, access_denied};
search(_BindDN, BaseObject, Scope, SizeLimit, Filter, Attributes) ->
    ScopeFilter = ldap_filter:scope(BaseObject, Scope),
    EntryFilter = ldap_filter:filter(Filter),
    FieldsOption = ldap_filter:fields(Attributes),
    LimitOption = ldap_filter:limit(SizeLimit),
    Res = emongo:find_all(eds, "root", 
			  ScopeFilter ++ EntryFilter,
			  FieldsOption ++ LimitOption),
    {ok, Res}.

search_reply(From, [Item|Result], MessageID) ->
    Attrs = lists:flatten(
	      lists:map(fun(I) -> 
				item_to_attribute(I) 
			end, Item)),
    {value, {_, "dn", [DN]}, PartialAttrs} = lists:keytake("dn", 2, Attrs),
    Entry = {'SearchResultEntry', DN, PartialAttrs},
    ldap_fsm:reply(From, {{searchResEntry, Entry}, MessageID}),
    search_reply(From, Result, MessageID);
search_reply(_From, [],_MessageID) ->
    ok.

item_to_attribute({_Name, {oid, _}}) ->
    [];
item_to_attribute({<<"_rdn">>,_}) ->
    [];
item_to_attribute({Name, Value}) when is_bitstring(Name), is_bitstring(Value) ->
    {'PartialAttribute', bitstring_to_list(Name), [bitstring_to_list(Value)]};
item_to_attribute({Name, {array, Value}}) when is_bitstring(Name), is_list(Value) ->
    lists:map(fun(V) -> 
		      {'PartialAttribute', bitstring_to_list(Name), [bitstring_to_list(V)]}
	      end, Value).

handle_cast({{bindRequest, Options}, MessageID,_BindDN, From}, State) ->
    {'BindRequest',_, BindDN, Creds} = Options,
    case bind(BindDN, Creds) of
	{ok, BindDN} -> 
	    Response = #'BindResponse'{resultCode = success,
				       matchedDN = "",
				       diagnosticMessage = ""},
	    ldap_fsm:set_bind(From, BindDN);
	{error, unsupported_method} ->
	    Response = #'BindResponse'{resultCode = authMethodNotSupported,
				       matchedDN = "",
				       diagnosticMessage = "Authentication method not supported"};
	{error, invalid_credentials} ->
	    Response = #'BindResponse'{resultCode = invalidCredentials,
				       matchedDN = "",
				       diagnosticMessage = "Invalid credentials"}
    end,
    ldap_fsm:reply(From, {{bindResponse, Response}, MessageID}),
    {stop, normal, State};

handle_cast({{searchRequest, Options}, MessageID, BindDN, From}, State) ->
    {'SearchRequest', BaseObject, Scope,_, SizeLimit,_,_, Filter, Attributes} = Options,
    case search(BindDN, BaseObject, Scope, SizeLimit, Filter, Attributes) of
	{ok, Result} ->
	    search_reply(From, Result, MessageID),
	    Response = #'LDAPResult'{resultCode = success,
				     matchedDN = "",
				     diagnosticMessage = ""};
	{error, access_denied} ->
	    Response = #'LDAPResult'{resultCode = insufficientAccessRights,
				     matchedDN = "",
				     diagnosticMessage = "Access denied"};
	{error, not_found} ->
	    Response = #'LDAPResult'{resultCode = noSuchObject,
				     matchedDN = "",
				     diagnosticMessage = "No such object"}
    end,
    ldap_fsm:reply(From, {{searchResDone, Response}, MessageID}),
    {stop, normal, State};

handle_cast({{modifyRequest, Options},_BindDN,_From}, State) ->
    io:format("-> ~p~n", [Options]),
    {'ModifyRequest',_,_} = Options,
    {stop, normal, State};

handle_cast({{addRequest, Options},_BindDN,_From}, State) ->
    {'AddRequest'} = Options,
    {stop, normal, State};

handle_cast({{modDNRequest, Options},_BindDN,_From}, State) ->
    {'ModifyDNRequest'} = Options,
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
