%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc Bush data structure.
%% Basically, ``bush'' is a set of two RB-trees: first is indexed in a normal way, 
%% second has keys swapped with values. 

-module(bush).

-export([init/0, insert/3, lookup_k/2, lookup_v/2, 
	 take_k/2, take_v/2, delete_k/2, delete_v/2]).

-export_type([bush/2]).

-type bush(K, V) :: {KVTree::rbtree:tree(K), VKTree::rbtree:tree(V)}.

%% @doc Create an empty bush
-spec init() -> bush(term(), term()).
init() ->
    {{}, {}}.

%% @doc Get KV tree of a bush
-spec kv_tree(bush(K, term())) -> rbtree:tree(K).
kv_tree(Bush) -> 
    element(1, Bush).

%% @doc Get VK tree of a bush
-spec vk_tree(bush(term(), V)) -> rbtree:tree(V).
vk_tree(Bush) ->
    element(2, Bush).

%% @doc Insert an item into a bush
-spec insert(K, V, bush(K, V)) -> bush(K, V).
insert(Key, Value, Bush) ->    
    {rbtree:insert(Key, Value, kv_tree(Bush)),
     rbtree:insert(Value, Key, vk_tree(Bush))}.

%% @doc Look up an item by its key
-spec lookup_k(K, bush(K, V)) -> {} | V.
lookup_k(Key, Bush) ->
    rbtree:lookup(Key, kv_tree(Bush)).

%% @doc Look up an item by its value
-spec lookup_v(V, bush(K, V)) -> {} | K.
lookup_v(Value, Bush) ->
    rbtree:lookup(Value, vk_tree(Bush)).

%% @doc Delete an item given by its key from a bush
-spec delete_k(K, bush(K, V)) -> bush(K, V).
delete_k(Key, Bush) ->
    case lookup_k(Key, Bush) of
	{} -> Bush;
	{Key, Value} ->
	    {rbtree:delete(Key, kv_tree(Bush)),
	     rbtree:delete(Value, vk_tree(Bush))}
    end.

%% @doc Delete an item given by its value from a bush
-spec delete_v(V, bush(K, V)) -> bush(K, V).
delete_v(Value, Bush) ->
    case lookup_v(Value, Bush) of
	{} -> Bush;
	{Value, Key} ->
	    {rbtree:delete(Key, kv_tree(Bush)),
	     rbtree:delete(Value, vk_tree(Bush))}
    end.

%% @doc Drop an item given by its key from a bush. Return new bush or false if no item found.
-spec take_k(K, bush(K, V)) -> false | bush(K, V).
take_k(Key, Bush) ->
    case lookup_k(Key, Bush) of
	{} -> false;
	{Key, Value} -> {Key, Value,
			 {rbtree:delete(Key, kv_tree(Bush)),
			  rbtree:delete(Value, vk_tree(Bush))}}
    end.

%% @doc Drop an item given by its value from a bush. Return new bush or false if no item found.
-spec take_v(V, bush(K, V)) -> false | bush(K, V).
take_v(Value, Bush) ->
    case lookup_v(Value, Bush) of
	{} -> false;
	{Value, Key} -> {Value, Key,
			 {rbtree:delete(Key, kv_tree(Bush)),
			  rbtree:delete(Value, vk_tree(Bush))}}
    end.
