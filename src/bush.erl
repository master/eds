%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc Bush data structure.
%% Basically, ``bush'' is a set of two RB-trees: first is indexed in a normal way, 
%% second has keys swapped with values. 

-module(bush).

-export([init/0, insert/3, 
	 lookup_k/2, lookup_v/2, 
	 take_k/2, take_v/2,
	 delete_k/2, delete_v/2]).

%% @doc Create an empty bush
%% @spec init() -> Bush
%%       Bush -> {Tree, Tree}
%%       Tree -> 
init() ->
    {{}, {}}.

%% @doc Get KV tree of a bush
%% @spec kv_tree(Bush) -> Tree
kv_tree(Bush) -> 
    element(1, Bush).

%% @doc Get VK tree of a bush
%% @spec vk_tree(Bush) -> Tree
vk_tree(Bush) ->
    element(2, Bush).

%% @doc Insert an item into a bush
%% @spec insert(Key, Value, Bush) -> Bush
insert(Key, Value, Bush) ->    
    {rbtree:insert(Key, Value, kv_tree(Bush)),
     rbtree:insert(Value, Key, vk_tree(Bush))}.

%% @doc Look up an item by its key
%% @spec lookup_k(Key, Bush) -> Item_KV | {}
%%       Item_KV -> {Key, Value}
lookup_k(Key, Bush) ->
    rbtree:lookup(Key, kv_tree(Bush)).

%% @doc Look up an item by its value
%% @spec lookup_k(Value, Bush) -> Item_VK | {}
%%       Item_VK -> {Value, Key}
lookup_v(Value, Bush) ->
    rbtree:lookup(Value, vk_tree(Bush)).

%% @doc Delete an item given by its key from a bush
%% @spec delete_k(Key, Bush) -> Bush
delete_k(Key, Bush) ->
    case lookup_k(Key, Bush) of
	{} -> Bush;
	{Key, Value} ->
	    {rbtree:delete(Key, kv_tree(Bush)),
	     rbtree:delete(Value, vk_tree(Bush))}
    end.

%% @doc Delete an item given by its value from a bush
%% @spec delete_v(Value, Bush) -> Bush
delete_v(Value, Bush) ->
    case lookup_v(Value, Bush) of
	{} -> Bush;
	{Value, Key} ->
	    {rbtree:delete(Key, kv_tree(Bush)),
	     rbtree:delete(Value, vk_tree(Bush))}
    end.

%% @doc Drop an item given by its key from a bush. Return new bush or false if no item found.
%% @spec take_k(Key, Bush) -> Bush | false
take_k(Key, Bush) ->
    case lookup_k(Key, Bush) of
	{} -> false;
	{Key, Value} -> {Key, Value,
			 {rbtree:delete(Key, kv_tree(Bush)),
			  rbtree:delete(Value, vk_tree(Bush))}}
    end.

%% @doc Drop an item given by its value from a bush. Return new bush or false if no item found.
%% @spec take_v(Value, Bush) -> Bush | false
take_v(Value, Bush) ->
    case lookup_v(Value, Bush) of
	{} -> false;
	{Value, Key} -> {Value, Key,
			 {rbtree:delete(Key, kv_tree(Bush)),
			  rbtree:delete(Value, vk_tree(Bush))}}
    end.
