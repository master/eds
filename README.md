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

        $ cd priv
        $ ./populate.py synrc.ldif

USAGE
=====
        $ ./eds.sh

LDAP port is 1389 by default. Anonymous bind is unsupported. Still server is pretty usable:

	$ ldapsearch -b "dc=synrc,dc=com" 'objectClass=*'
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

Filters:

	$ ldapsearch -b "dc=synrc,dc=com" '(&(uid=*)(cn=Ma*))' \
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

Attributes selection:

	$ ldapsearch -b "dc=synrc,dc=com" 'sn=*' cn sn \
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret

Size limit:

	$ ldapsearch -b "dc=synrc,dc=com" 'sn=*' cn sn -z 1 \
	$ 	     -h localhost -p 1389 -D "uid=admin,dc=synrc,dc=com" -w secret
