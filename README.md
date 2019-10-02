# SemanticLMM
Semantic model of linear mixed model (LMM) analysis

The repository hosts the development of resources for semantic modelling of the results of linear mixed model (LMM) analysis. 
A related paper "Semantic concept schema of the linear mixed model of experimental observations" has been submitted.
The repository directories contains in particular:
  - ontology: STATO-LMM, an LMM-related extension of STATO ontology, based on a STATO module and additional concepts for statistical data analysis
  - semLMM: code in R for generation of RDF graphs for *lme4/nlme* linear mixed models; organised as an R package *semLMM*
  - examples/graphGeneration: an R notebook demonstrating how to generate RDF graphs for *lme4/nlme* linear mixed models
  - examples/graphs: a set of RDF graphs of LMM analyses, serialized as .trig files
  - examples/queries: SPARQL queries that allow for exploration of RDF graphs of LMM analyses in triples store via a SPARQL endpoint (e.g. http://lmm.cropnet.pl/sparql)
  - examples/examplesInPaper: datasets and R code used to generate examples for the paper

