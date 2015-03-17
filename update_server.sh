echo "update corporate_risk server when code has changed"

cd ~/src/semantic_forms/scala/forms/
git pull --verbose
./activator clean publishLocal

cd ~/src/corporate_risk/
git pull --verbose
./activator dist

echo "sofware recompiled!"

cd ~/deploy
kill `cat corporate_risk-1.0-SNAPSHOT/RUNNING_PID`

# pour garder les logs
rm -r corporate_risk-1.0-SNAPSHOT_OLD
mv corporate_risk-1.0-SNAPSHOT  corporate_risk-1.0-SNAPSHOT_OLD

unzip $HOME/src/corporate_risk/target/universal/corporate_risk-1.0-SNAPSHOT.zip

cd corporate_risk-1.0-SNAPSHOT
ln -s ../TDBrisk TDB
nohup bin/corporate_risk -mem 100 -J-server -Dhttp.port=9153 &
