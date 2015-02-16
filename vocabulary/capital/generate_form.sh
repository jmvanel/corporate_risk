# Argument : a markdown file, e.g. "Evaluation_capital_opérationnel.md"

md=$1
sed -e '1,$s/ {\.P1}//;1,$s/ {\.P29}//;1,$s/!\[\](data:image.*)//' $md > $md.new
mv $md.new $md
echo $md file processed with sed!

( cd converter ; sbt "runMain jmvanel.markdown.RDFDiscounterApp ../$md" )
ttl=${md%%.md}.ttl
echo $ttl file made!

owl=${ttl%%.ttl}.owl.ttl
eye --nope $ttl questionnaire2owl.n3 --query questionnaire2owl.q.n3 > $owl
echo $owl file made!

rapper -i turtle $owl -o turtle > $owl.new
mv $owl.new $owl
echo $owl file reformatted!
