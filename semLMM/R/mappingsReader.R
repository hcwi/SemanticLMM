# get terms for R objects (as simple ONTOLOGY_ID or full ONTOLOGY_URI)
prepareAnnotations <- function(annotationType="ONTOLOGY_ID") {

  # list of mappings R names > LMM or STATO terms
  termListCsv <- system.file("extdata", "modelTerms.csv", package = "semLMM", mustWork = TRUE)
  #termListCsv <- "modelTerms.csv"
  terms <- read.table(termListCsv, sep=";", header = TRUE, comment.char = "", stringsAsFactors = FALSE)

  # creating variables for the terms
  # (selecting one of LMM or STATO terms from csv file above and putting it in R file with assignement operation "<-" )
  for (i in 1:dim(terms)[1]) {
    ontTerm <- terms[i, annotationType]
    if (ontTerm == "") {
      warning(paste("No ontology term for the object '", terms[i,1], "' found.", sep=""))
      ontTerm <- toupper(terms[i,1])
    }
    assign(toupper(terms[i,1]), ontTerm, envir = termsEnv)
  }
}

prefixes <- "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix stato: <http://purl.obolibrary.org/obo/stato.owl#> .
@prefix obi: <http://purl.obolibrary.org/obo/obi.owl#> .
@prefix iao: <http://purl.obolibrary.org/obo/iao.owl#> .
@prefix bfo: <http://purl.obolibrary.org/obo/bfo.owl#> .
@prefix ro: <http://purl.obolibrary.org/obo/ro.owl#> .
@prefix obo: <http://purl.obolibrary.org/obo/> .
@prefix lmm: <http://purl.org/stato-lmm#> .

"
termsEnv <- new.env()
prepareAnnotations()
