# Transformation des données

Les fichiers originels pour LimeSurvey sont traités par pandoc pour obtenir des fichiers Markdown UFT-8 :

    pandoc -f html -t markdown -o Evaluation_capital_opérationnel.md --no-wrap "Evaluation du capital opérationnel.html"
    pandoc -f html -t markdown -o Evaluation_du_capital_humain.md --no-wrap "Evaluation du capital humain.html"
    pandoc -f html -t markdown -o Evaluation Evaluation_capital_Structurel.md --no-wrap Evaluation_capital_Structurel.html

Ensuite il convient d'enlever les images qui accompagnent les listes à boulettes (via vi, vim ou sed):

    1,$s/!\[\](data:image.*)//
    1,$s/   //
    1,$s/ {.P1}//
    1,$s/ {.P29}//

On  obtient les 3 fichiers Markdown qui sont sous git.

Les fichiers Markdown sont ensuite traités par le programme Scala dans le répertoire converter/ ,
pour obtenir:

    Evaluation_capital_opérationnel.ttl
etc.

Ensuite on utilise une base de règles (questionnaire2owl.q.n3) avec Euler / EYE pour transformer afin de créer le vocabulaire OWL et le groupe de formulaires pour semantic\_forms .

Enfin, on utilise rapper (installer raptor2-utils sur Linux) pour reformater joliment le vocabulaire OWL obtenu:
    Evaluation_capital_opérationnel.owl.ttl


Tout cela est rassemblé dans un script: `generate_form.sh` , avec la fonction md2owl qui prend en argument un fichier Markdown.

