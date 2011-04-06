#!/bin/bash

ldap_search() {
    FILTER=${1:-'"objectClass=*"'}
    ARGS=$2' '$3' '$4$' '$5
    local CMD='ldapsearch -b "dc=synrc,dc=com" -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret '$FILTER' '$ARGS
    eval $CMD
}

ldap_modify() {
    local CMD='ldapmodify -f '$1' -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret 2>&1'
    eval $CMD
}

run_test() {
    echo -n " - "$1"... "
    RES=$($2)
    MOD=$(< $3)
    ( [ "$RES" = "$MOD" ] && echo "ok." ) || echo "Failed!"
}

run_test "Cleanup" "mongo eds 000-cleanup.js" "000-cleanup.txt"

run_test "Initialization" "../priv/populate.py 000-init.ldif" "000-init.txt"

run_test "Search for 'objectClass=*'" "ldap_search" "000-search.txt"

run_test "Search for '(&(uid=*)(cn=Ma*))'" "ldap_search \"(&(uid=*)(cn=Ma*))\"" "001-search.txt"

run_test "Search with attribute selection" "ldap_search \"sn=*\" cn sn" "002-search.txt"

run_test "Search with size limit" "ldap_search \"sn=*\" cn sn -z1" "003-search.txt"

R=$(ldap_modify "010-add.ldif")
run_test "Adding entries" "ldap_search" "010-add.txt"

run_test "Adding duplicate entries" "ldap_modify \"011-add-exists.ldif\"" "011-add-exists.txt"

R=$(ldap_modify "020-modify-replace.ldif")
run_test "Replacing value of an attribute" "ldap_search" "020-modify-replace.txt"

R=$(ldap_modify "021-modify-add.ldif")
run_test "Adding new attribute" "ldap_search" "021-modify-add.txt"

R=$(ldap_modify "022-modify-del.ldif")
run_test "Deleting an attribute" "ldap_search" "022-modify-del.txt"

R=$(ldap_modify "023-modify-multi.ldif")
run_test "Changing many attributes at once" "ldap_search" "023-modify-multi.txt"

run_test "Changing nonexistent object" "ldap_modify \"024-modify-noobj.ldif\"" "024-modify-noobj.txt"

R=$(ldap_modify "025-modify-dn.ldif")
run_test "Modifying DN" "ldap_search" "025-modify-dn.txt"

R=$(ldap_modify "030-delete.ldif")
run_test "Deleting entries" "ldap_search" "030-delete.txt"

run_test "Deleting nonexistent entries" "ldap_modify \"031-delete-noobj.ldif\"" "031-delete-noobj.txt"
