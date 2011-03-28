{application, eds,
 [
  {description, "Erlang Directory Server"},
  {vsn, "0.1.1"},
  {id, "eds"},
  {modules,      [tcp_listener, ldap_ops, ldap_fsm, ldap_filter, 
  		 ldap_obj, ldap_msg, 'LDAP', bush, rbtree]},
  {registered,   [tcp_server_sup, client_sup, ops_sup, tcp_listener]},
  {applications, [kernel, stdlib]},
  {mod, {eds_app, []}},
  {env, []}
 ]
}.
