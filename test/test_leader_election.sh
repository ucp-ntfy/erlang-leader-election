#!/bin/sh 

HOSTNAME=`hostname`
NODES=""
ERL_NODES_ARG="["
COMMA=""
for NODE in ns1 ns2 ns3; do
	NODES="$NODES $NODE@$HOSTNAME"
	ERL_NODES_ARG="$ERL_NODES_ARG $COMMA '$NODE@$HOSTNAME'"
	COMMA=","
done
ERL_NODES_ARG="$ERL_NODES_ARG ]"

mkdir -p .test
cp ebin/* .test
cd .test

for NAME in $NODES; do
	echo $NAME
	erl -pz ebin -name $NAME -leader_election participants "$ERL_NODES_ARG" -detached -kernel error_logger "{file, \"/tmp/erl.log.$NAME\"}"
done

