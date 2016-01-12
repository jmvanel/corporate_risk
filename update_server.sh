echo "update corporate_risk server when code has changed in semantic_forms & corporate_risk"
SBT=./activator
export SBT=sbt

cd ~/src/semantic_forms/scala/forms/
git pull --verbose
$SBT clean publishLocal

./update_server_only_corporate_risk.sh
