# get terms for R objects from specific ontology 'ont' (currently LMM, STATO_SIMPLE, STATO)
prepareTermsFromOntology <- function(ont = "LMM") {
  
  # list of mappings R names > LMM or STATO terms
  termListCsv <- "/Users/hania/Code/R_oom/modelTerms/modelTerms.csv"
  terms <- read.table(termListCsv, sep=";", header = TRUE, comment.char = "", stringsAsFactors = FALSE)
  
  # creating variables for the terms
  # (selecting one of LMM or STATO terms from csv file above and putting it in R file with assignement operation "<-" )
  for (i in 1:dim(terms)[1]) {
    ontTerm <- terms[i,ont]
    if (ontTerm == "") {
      warning(paste("No ", ont, " term for the object '", terms[i,1], "' found.", sep=""))
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
@prefix lmm: <http://igr.poznan.pl/lmm#> .

"
termsEnv <- new.env()
prepareTermsFromOntology("STATO") #STATO_SIMPLE")
