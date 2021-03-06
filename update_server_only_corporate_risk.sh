#!/bin/bash

echo "update corporate_risk server when code has changed in corporate_risk"
SRC=$HOME/src/corporate_risk/
APP=corporate_risk
APPVERS=${APP}-1.0-SNAPSHOT
SBT=sbt

cd ~/src/corporate_risk/
git pull --verbose
$SBT dist

echo "sofware recompiled!"

cd ~/deploy
kill `cat corporate_risk-1.0-SNAPSHOT/RUNNING_PID`

# pour garder les logs
rm -r corporate_risk-1.0-SNAPSHOT_OLD
mv corporate_risk-1.0-SNAPSHOT  corporate_risk-1.0-SNAPSHOT_OLD

unzip $HOME/src/corporate_risk/target/universal/corporate_risk-1.0-SNAPSHOT.zip

cd corporate_risk-1.0-SNAPSHOT
mkdir -p ../TDBrisk
mkdir -p ../TDBrisk2
mkdir -p ../TDBrisk3

ln -s ../TDBrisk  TDB
ln -s ../TDBrisk2 TDB2
ln -s ../TDBrisk3 TDB3
nohup bin/corporate_risk -mem 100 -J-server -Dhttp.port=9153 &
