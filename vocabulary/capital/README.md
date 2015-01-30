
Les fichiers originels pour LimeSurvey sont traités par pandoc pour obtenir des fichiers Markdown UFT-8 :

    pandoc -f html -t markdown -o Evaluation_capital_opérationnel.md "Evaluation du capital opérationnel.html"
    pandoc -f html -t markdown -o Evaluation_du_capital_humain.md  "Evaluation du capital humain.html"
    pandoc -f html -t markdown -o Evaluation Evaluation_capital_Structurel.md "Evaluation capital Structurel.html"

Ensuite il convient d'enlever les images qui accompagnent les listes à boulettes (via vi, vim ou sed):

    1,$s/!\[\](data:image.*)//
    1,$s/   //

On  obtient les 3 fichiers Markdown qui sont sous git.

Les fichiers Markdown sont ensuite traités par le programme Scala dans le répertoire converter/ ,
pour obtenir:

    Evaluation_capital_opérationnel.ttl
etc.

