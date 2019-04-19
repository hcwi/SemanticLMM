# SemanticLMM
Semantic model of linear mixed model (LMM) analysis

The repository hosts the development of resources for semantic modelling of the results of linear mixed model (LMM) analysis. 
A related paper "A semantic concept schema of the linear mixed model of experimental observations" has been submitted.
The repository directories contains in particular:
  - Graph generation: an R notebook demonstrating how to generate RDF graphs for linear mixed models obtained from lmer and lmer R packages
  - SPARQL: queries that allow for exploration of RDF graphs of LMM analyses, uploaded in a triples store with SPARQL endpoint (e.g. http://lmm.cropnet.pl)
  - graphs: RDF graphs of LMM analyses, serialized as .trig files
  - paper: datasets and R code used to generate examples for the paper
  - semLMM: code for graph generation, organised as a R package (work in progress)
