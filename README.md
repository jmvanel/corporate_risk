# corporate\_risk
Forms for corporate risk evaluation

## Introduction
This projet generates input forms (from markdown and tabular data) and implements results management 
for corporate risk  and immaterial capital evaluation.

It is a web application built with Play! Framework and W3C's Semantic Web recommandations (RDF, SPARQL).

## Status

Currently in development, first release in march.

## Technique

This projet leverages on [semantic forms, a form generators leveraging semantic web standards RDF(S), OWL, SPARQL](https://github.com/jmvanel/semantic_forms#semantic-forms),
which itself leverages on Play! Framework, Banana-RDf and Jena TDB.

### Running

First, install from sources the Scala project [semantic\_forms/forms](https://github.com/jmvanel/semantic_forms/tree/master/scala/forms).
Then, just like any Play! Framework application, install SBT, and type:

    run

For more details, see [in semantic\_forms the paragraph "How to run"](https://github.com/jmvanel/semantic_forms/tree/master/scala/forms_play#how-to-run)

There is also a script to restart the server when the code has changed:
    update_server.sh

## Debug
See 
[playframework documentation/2.3.x/IDE](https://www.playframework.com/documentation/2.3.x/IDE)

Apparently Activator, not SBT, is needed:
    activator -jvm-debug 9999 run

### Preloading RDF content

- Preloading vocabularies, and pre-defined form specifications : in shell type:
```
    populate_db.sh
```

## Note on the data model
There are 3 levels:
- form groups
- forms
- questions

A form is modeled by an OWL class C, plus an OWL object property P, such that :

    P rdfs:domain :User ; rdfs:range C .

which means that P connects the class User to a class C that models the form.
The form is built by the semantic\_forms tool from C by gathering all properties PC such that:

    PC rdfs:domain C .

So each PC gives an atomic question in the form defined by C and P.

Then forms are grouped in form groups appearing in different pages in the application, with a summary of the answers.
In the RDF database, the form groups are connected to the OWL object properties P of the forms by a ques:properties predicate, like this :

```
:risk-fg a :FormGroup ;
     rdfs:label "Questions sur la gestion des risques."@fr ;
    :properties :prop-5 , :prop-6 , :prop-7 , :prop-8 ,:prop-9 , :prop-10 , :prop-11 , :prop-12 , :prop-13 , :prop-14 , :prop-15
.
```

Currently some things are hard coded in UserDataTrait :
```
    formsGroups = List("risk", "human", "structural", "operational")
    formsGroupsURIs
```
But in principle any database made of OWL Turtle files can be used in the application.
