#/bin/sh
# /bin/bash

EYE=/opt/eye/bin/eye.sh

# Argument : a markdown file, e.g. "Evaluation_capital_opérationnel.md"
function md2owl {
md=$1
initialIndex=$2
echo md2owl md $md , initialIndex $initialIndex

sed -e '1,$s/ {\.P1}//;1,$s/ {\.P29}//;1,$s/!\[\](data:image.*)//;1,$s/   //;1,$s/ *$//;1,$s/  $//;/### \[\]$/d' $md > $md.new
echo removing duplicated lines
uniq $md.new > $md.new2
echo 'joining lines with "-" only'
ex -c "%g/^-$/j" -c "wq" $md.new2
mv $md.new2 $md
echo $md file processed with sed, uniq, ex!

( cd converter ;
  echo  "runMain jmvanel.markdown.RDFDiscounterApp ../$md $initialIndex";
  sbt <<EOF
  runMain jmvanel.markdown.RDFDiscounterApp ../$md $initialIndex
EOF
)

ttl=${md%%.md}.ttl
echo $ttl file made!

owl=${ttl%%.ttl}.owl.ttl
echo $EYE --nope $ttl questionnaire2owl.n3 --query questionnaire2owl.q.n3 ">" $owl
$EYE --nope $ttl questionnaire2owl.n3 --query questionnaire2owl.q.n3 > $owl
echo $owl file made!

rapper -i turtle $owl -o turtle > $owl.new
mv $owl.new $owl
echo $owl file reformatted!
}

md2owl "Evaluation_capital_opérationnel.md" 0 
md2owl "Evaluation_capital_Structurel.md" 1000
md2owl "Evaluation_du_capital_humain.md" 2000


