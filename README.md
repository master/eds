README
======
Erland Directory Server is an [LDAP](http://en.wikipedia.org/wiki/LDAP) server with [MongoDB](http://www.mongodb.org/) backend.

INSTALL
=======
Compile eMongo driver first:

        $ git clone git://github.com/master/emongo.git emongo
        $ cd emongo
        $ make
        $ cd ..

Download and compile EDS:

        $ git clone git://github.com/master/eds.git eds
        $ cd eds
        $ make

Populate database with sample LDIF:

        $ priv/populate.py t/000-init.ldif

	dc=synrc,dc=com
	uid=admin,dc=synrc,dc=com
	ou=People,dc=synrc,dc=com
	cn=Oleg Smirnov,ou=People,dc=synrc,dc=com
	cn=Maxim Sokhatsky,ou=People,dc=synrc,dc=com


USAGE
=====
        $ ./eds.sh

LDAP port is 1389 by default. Anonymous bind is unsupported.

Search
------
	$ ldapsearch -b "dc=synrc,dc=com" 'objectClass=*'
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

Filters:

	$ ldapsearch -b "dc=synrc,dc=com" '(&(uid=*)(cn=Ma*))' \
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

Attributes selection:

	$ ldapsearch -b "dc=synrc,dc=com" 'sn=*' cn sn \
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

Size limit on query result:

	$ ldapsearch -b "dc=synrc,dc=com" 'sn=*' cn sn -z 1 \
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

Add
---
	$ ldapmodify -f t/001-add.ldif 
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret	

	adding new entry "cn=alice,ou=People,dc=synrc,dc=com"

	adding new entry "cn=bob,ou=People,dc=synrc,dc=com"

	$ ldapmodify -f t/002-add-exists.ldif 
	  	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

	adding new entry "cn=alice,ou=People,dc=synrc,dc=com"
	ldap_add: Already exists (68)

Modify DN
---------
    	$ ldapmodify -f t/007-modify-dn.ldif 
	  	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

	modifying rdn of entry "cn=alice,ou=People,dc=synrc,dc=com"
