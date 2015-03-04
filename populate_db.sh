echo "To regenerate the form from human experts data, run vocabulary/capital/generate_form.sh"
echo "See also vocabulary/risk/README.md and vocabulary/capital/README.md"

sbt <<EOF 
runMain tdb.tdbloader --loc=TDB --graph=vocabulary vocabulary/risk/risk_questions.owl.ttl
runMain tdb.tdbloader --loc=TDB --graph=vocabulary vocabulary/risk/labels.ttl
runMain tdb.tdbloader --loc=TDB --graph=vocabulary vocabulary/capital/Evaluation_capital_opérationnel.owl.ttl
runMain tdb.tdbloader --loc=TDB --graph=vocabulary vocabulary/capital/Evaluation_capital_Structurel.owl.ttl
runMain tdb.tdbloader --loc=TDB --graph=vocabulary vocabulary/capital/Evaluation_du_capital_humain.owl.ttl
runMain tdb.tdbloader --loc=TDB --graph=vocabulary vocabulary/risk/semantic_links.ttl
runMain tdb.tdbloader --loc=TDB --graph=vocabulary vocabulary/capital/semantic_links.ttl
EOF

echo "Local SPARQL database in TDB/ populated."
