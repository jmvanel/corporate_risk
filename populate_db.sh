#!/bin/bash

echo "To regenerate the form from human experts data, run vocabulary/capital/generate_form.sh"
echo "See also vocabulary/risk/README.md and vocabulary/capital/README.md"

echo "Avez vous fait wget http://rdf.insee.fr/codes/nafr2.ttl.zip ; unzip nafr2.ttl.zip ?"

# coherent with val vocabularyGraph in UserData.scala
GRAPH=model:vocabulary

echo "Enlever le graphe <$GRAPH> dans la base TDB"
echo "DROP GRAPH <$GRAPH>" > /tmp/delete_graph.rq
sbt <<EOF 
runMain tdb.tdbupdate --loc=TDB --verbose --update=/tmp/delete_graph.rq
runMain tdb.tdbloader --loc=TDB --graph=$GRAPH \
	vocabulary/risk/risk_questions.owl.ttl \
	vocabulary/risk/labels.ttl \
	vocabulary/capital/Evaluation_capital_opérationnel.owl.ttl \
	vocabulary/capital/Evaluation_capital_Structurel.owl.ttl \
	vocabulary/capital/Evaluation_du_capital_humain.owl.ttl \
	vocabulary/risk/semantic_links.ttl \
	vocabulary/capital/semantic_links.ttl
runMain tdb.tdbloader --loc=TDB --graph=data:insee.NAF nafr2.ttl
EOF

echo "Local SPARQL database in TDB/ populated."
