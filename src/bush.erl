%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc Bush data structure
%% Basically, bush is a set of two RB-trees.
%% First is indexed in a normal way. Second has keys swapped with values. 

-module(bush).

-export([init/0, insert/3, 
	 lookup_k/2, lookup_v/2, 
	 take_k/2, take_v/2,
	 delete_k/2, delete_v/2]).

init() ->
    {{}, {}}.

kv_tree(Bush) -> 
    element(1, Bush).

vk_tree(Bush) ->
    element(2, Bush).

insert(Key, Value, Bush) ->    
    {rbtree:insert(Key, Value, kv_tree(Bush)),
     rbtree:insert(Value, Key, vk_tree(Bush))}.

lookup_k(Key, Bush) ->
    rbtree:lookup(Key, kv_tree(Bush)).

lookup_v(Value, Bush) ->
    rbtree:lookup(Value, vk_tree(Bush)).

delete_k(Key, Bush) ->
    case lookup_k(Key, Bush) of
	{} -> Bush;
	{Key, Value} ->
	    {rbtree:delete(Key, kv_tree(Bush)),
	     rbtree:delete(Value, vk_tree(Bush))}
    end.

delete_v(Value, Bush) ->
    case lookup_v(Value, Bush) of
	{} -> Bush;
	{Value, Key} ->
	    {rbtree:delete(Key, kv_tree(Bush)),
	     rbtree:delete(Value, vk_tree(Bush))}
    end.

take_k(Key, Bush) ->
    case lookup_k(Key, Bush) of
	{} -> false;
	{Key, Value} -> {Key, Value,
			 {rbtree:delete(Key, kv_tree(Bush)),
			  rbtree:delete(Value, vk_tree(Bush))}}
    end.

take_v(Value, Bush) ->
    case lookup_v(Value, Bush) of
	{} -> false;
	{Value, Key} -> {Value, Key,
			 {rbtree:delete(Key, kv_tree(Bush)),
			  rbtree:delete(Value, vk_tree(Bush))}}
    end.
