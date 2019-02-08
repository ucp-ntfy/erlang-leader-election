#!/bin/sh 

NODES=""
ERL_NODES_ARG="["
COMMA=""
for NODE in ns1 ns2 ns3 ns4 ns5; do
	NODES="$NODES $NODE@localhost"
	ERL_NODES_ARG="$ERL_NODES_ARG $COMMA '$NODE@localhost'"
	COMMA=","
done
ERL_NODES_ARG="$ERL_NODES_ARG ]"

PATH="$PWD/test:$PATH"

mkdir -p .test
cp ebin/* .test
cd .test

for T in test1 test2 test3 test4; do
	echo "test: $T"
	for NAME in $NODES; do
		echo "Stopping $NAME"
		stop_node.erl $NAME
	done

	for NAME in $NODES; do
		echo "Starting $NAME"
		erl -pz ebin -sname $NAME -leader_election participants "$ERL_NODES_ARG" -detached -kernel error_logger "{file, \"/tmp/erl.log.$NAME\"}" -setcookie cookie1

	done
	test_leader_election.erl $T $NODES
done

