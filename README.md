# corporate\_risk
Forms for corporate risk evaluation

## Introduction
This projet manages input forms for corporate risk  and immaterial capital evaluation.

## Status

Currently in development, first release on february 6.

## Technique

This projet leverages on [semantic forms, a form generators leveraging semantic web standards RDF(S), OWL, SPARQL](https://github.com/jmvanel/semantic_forms#semantic-forms),
which itself leverages on Play! Framework, Banana-RDf and Jena TDB.

### Running

Just like any Play! Framework application, install SBT, and type:

    run

For more details, see [in semantic\_forms the paragraph "How to run"](https://github.com/jmvanel/semantic_forms/tree/master/scala/forms_play#how-to-run)

### Preloading RDF content

- Preloading vocabularies, and pre-defined form specifications : in activator shell type:
```
    runMain tdb.tdbloader --loc=TDB --graph=vocabulary \
      vocabulary/risk/risk_questions.owl.ttl \
      vocabulary/capital/Evaluation_capital_opérationnel.owl.ttl \
      vocabulary/capital/Evaluation_capital_Structurel.owl.ttl \
      vocabulary/capital/Evaluation_du_capital_humain.owl.ttl
```
