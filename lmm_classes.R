init <- function() {
  
  source("term_mappings_reader.R")
  
  id <<- 0
  
  getID <<- function(o) {
    l <- o$label
    if (is.null(l) || l == "") {
      id <<- id + 1
      l <- id
    }
    c <- as.character(class(o))
    paste(c, l, sep="_")
  }
  
  reg <<- list()
  
  register <<- function(o) {
    o$id <- getID(o)
    reg[o$id] <<- list(o)
  }
  
}
init()

OntologyEntity <- {setRefClass("OntologyEntity",
                              fields = list(
                                id = "character",
                                label ="character"
                              ),
                              methods = list(
                                initialize = function(label) {
                                  .self$label <- label
                                  register(.self)
                                },
                                show = function() {
                                  queue <<- list()
                                  queue[.self$id] <<- FALSE
                                  cat(asTTL())
                                  i <- 1
                                  while (TRUE) {
                                    if (i <= length(queue)) {
                                      key <- names(queue)[i]
                                      #print(paste("Checking", key, "==", queue[key], "(", i, "/", length(queue), ")"))
                                      if (queue[key] == TRUE) {
                                        queue[key] <<- FALSE # sets a key to be printed (if still ahead); 
                                        # if the object has been put in the queue before the current one (and thus already printed), 
                                        # the change to 'FALSE' doesn't make any difference, as the loop doesn't go back
                                        cat(reg[[key]]$asTTL())
                                      }
                                      i <- i + 1
                                    } else {
                                      break
                                    }
                                  }
                                  queue <<- NULL
                                },
                                ident = function(identifier) {
                                  paste0("<", identifier, ">")
                                },
                                lit = function(literal) {
                                  paste0("\"", literal, "\"")
                                },
                                asTTL = function() {
                                  paste(ident(id), TYPE, "OntologyEntity",
                                        ";\n", LABEL, lit(label),
                                        ".\n")
                                },
                                listAsTTL = function(oo) {
                                  ids = "" # concatenated IDs from the 'oo' list
                                  for (o in oo) {
                                    ids <- paste(ids, ident(o$id), sep=", ") # add ID to the string
                                    if (!exists("queue") || is.null(queue)) {
                                      queue <<- list()
                                    }
                                    queue[o$id] <<- TRUE # add to the queue and / or set printing (if already exists)
                                  }
                                  substring(ids,3) # return concatenated without initial separators ', '
                                }
                              )
)}
oe <- OntologyEntity(label = "eoLabel")
#oe
#str(oe)

reg #list registered objects
getEntity <- function(className, label) {
  for (r in reg) {
    if (class(r) == className & r$label == label)
      return(r)
  }
  return(NULL)
}
getEntity("OntologyEntity", "eoLabel")

ModelParameter <- {setRefClass("ModelParameter",
                              contains = "OntologyEntity",
                              fields = list(
                                type = "character"
                                ),
                              methods = list(
                                initialize = function(... , type) {
                                  callSuper(...)
                                  .self$type <- type
                                },
                                asTTL = function() {
                                  paste(ident(id), TYPE, MODELPARAMETER,
                                        ";\n", "xxx:TMP_TYPE", lit(type),
                                        ".\n"
                                  )
                                }
                              )
)}
param1 <- ModelParameter(label = "param1", type = "variance")

Hypothesis <- {setRefClass("Hypothesis", 
                           contains = "OntologyEntity",
                           fields = list(
                             pvalue = "numeric",
                             qvalue = "numeric",
                             modelParams = "list" #ModelParameter
                           ),
                           methods = list(
                             initialize = function(... , pvalue, modelParams) {
                               callSuper(...)
                               .self$pvalue <- pvalue
                               .self$modelParams <- modelParams
                             },
                             asTTL = function() {
                               paste(ident(id), TYPE, HYPOTHESIS,
                                     ";\n", PVALUE, lit(pvalue),
                                     ";\n", QVALUE, lit(qvalue),
                                     ";\n", ISABOUT, listAsTTL(modelParams),
                                     ".\n"
                               )
                             }
                           )
)}

StatisticalTest <- {setRefClass("StatisticalTest", 
                           contains = "OntologyEntity",
                           fields = list(
                             pvalue = "numeric",
                             df = "numeric",
                             testStatistic = "list", #Statistic
                             hypothesis = "list"
                           ),
                           methods = list(
                             initialize = function(... , pvalue, df, testStatistic) {
                               callSuper(...)
                               .self$pvalue <- pvalue
                               .self$df <- df
                               .self$testStatistic <- testStatistic
                             },
                             asTTL = function() {
                               paste(ident(id), TYPE, STATITICALHYPOTHESISTEST,
                                     ";\n", PVALUE, lit(pvalue),
                                     ";\n", DF, lit(df),
                                     ";\n", "xxx:_has_input_", listAsTTL(testStatistic),
                                     ";\n", ISABOUT, listAsTTL(hypothesis),
                                     ".\n"
                               )
                             }
                           )
)}

Statistic <- {setRefClass("Statistic", 
                          contains = "OntologyEntity",
                          fields = list(
                            type = "character",
                            value = "numeric"
                          ),
                          methods = list(
                            initialize = function(... , type, value) {
                              callSuper(...)
                              .self$value <- value
                              .self$type <- type
                            },
                            asTTL = function() {
                              paste(ident(id), TYPE, TESTSTATISTIC,
                                    ";\n", VALUE, lit(value),
                                    ";\n", TYPE, lit(type),
                                    ".\n"
                              )
                            }
                          )
)}

hypo1 <- Hypothesis(label="hypo0", pvalue=0.05, modelParams = list(param1))
tstat <- Statistic(label="t-stat", type="T-student statistic", value=18.46)
ttest <- StatisticalTest(label="t-test", df=20, pvalue=0.0023, testStatistic = list(tstat))
ttest$hypothesis <- list(hypo1)
ttest

Estimate <- {setRefClass("Estimate", 
                        contains = "OntologyEntity",
                        fields = list(
                          value = "numeric",
                          se = "numeric",
                          conflev = "numeric",
                          ucl = "numeric",
                          lcl = "numeric",
                          isEstimateOf = "ANY" #ModelParameter
                        ),
                        methods = list(
                          initialize = function(... , value, parameter) {
                            callSuper(...)
                            .self$value <- value
                            .self$isEstimateOf <- parameter
                          },
                          asTTL = function() {
                            paste(ident(id), TYPE, ESTIMATE,
                                  ";\n", VALUE, lit(value),
                                  ";\n", SE, lit(se),
                                  ";\n", ISESTIMATEOF, listAsTTL(list(isEstimateOf)),
                                  ".\n"
                            )
                          }
                        )
)}
est <- Estimate(label = "est1", value = 222, parameter = param1)
est
#cat(est$asTTL())

Variable <- {setRefClass("Variable",
                         contains = "OntologyEntity",
                         fields = list(
                         ),
                         methods = list(
                           initialize = function(...) {
                             callSuper(...)
                           },
                           asTTL = function() {
                             paste(ident(id), TYPE, VARIABLE,
                                   ";\n", LABEL, lit(label),
                                   ".\n"
                             )
                           }
                         )
)}

VariableLevel <- {setRefClass("VariableLevel",
                              contains = "OntologyEntity",
                              fields = list(
                                variable = "ANY"
                              ),
                              methods = list(
                                initialize = function(..., variable) {
                                  callSuper(...)
                                  .self$variable <- variable
                                },
                                asTTL = function() {
                                  paste(ident(id), TYPE, VARIABLELEVEL,
                                        ";\n", LABEL, lit(label),
                                        ";\n", ISLEVELOF, listAsTTL(list(variable)),
                                        ".\n"
                                  )
                                }
                              )
)
}

CategoricalVariable <- {setRefClass("CategoricalVariable",
                         contains = "Variable",
                         fields = list(
                           levels = "list" # of VariableLevel
                         ),
                         methods = list(
                           initialize = function(... , levels=character()) {
                             callSuper(...)
                             .self$levels <- list()
                             for (l in levels){
                              .self$levels <- append(.self$levels, 
                                                     VariableLevel(label=l, variable=.self))
                             }
                           },
                           asTTL = function() {
                             paste(ident(id), TYPE, CATEGORICALVARIABLE,
                                   ";\n", LABEL, lit(label),
                                   ";\n", HASLEVEL, listAsTTL(levels),
                                   ".\n"
                             )
                           }
                         )
)}
v1 <- CategoricalVariable(label="infection", levels = c("infected", "non-infected"))
v2 <- CategoricalVariable(label="infraname", levels = c("CamB1", "Maresi", "Soldo"))
t1 <- Variable(label="plantHeight")

camB1 <- VariableLevel(label="CamB1", variable=v2)
camB1

ModelTerm <- {
  setRefClass("ModelTerm",
              contains = "OntologyEntity",
              fields = list(
                order = "integer",
                variable = "list", # of Variable
                effect = "list", # of Effect
                estimate = "list" # of Estimates of Effects
              ),
              methods = list(
                initialize = function(..., order = NA_integer_, variable = list(), effect = list()) {
                  callSuper(...)
                  .self$order <- order
                  .self$variable <- variable
                },
                asTTL = function() {
                  paste(ident(id), TYPE, MODELTERM,
                        ";\n", LABEL, lit(label),
                        ";\n", HASORDER, lit(order),
                        ";\n", paste(ISABOUT, listAsTTL(variable), collapse = " ;\n "),
                        ";\n", paste(HASEFFECT, listAsTTL(effect), collapse = " ;\n "),
                        ";\n", paste(TMP, listAsTTL(estimate), collapse = " ;\n "),
                        ".\n")
                }
              )
  )}

CovarianceStructure <- {
  setRefClass("CovarianceStructure",
              contains = "OntologyEntity",
              fields = list(
                covarianceModel = "character",
                params = "list", # of ModelParams
                estimate = "list" # of Estimates of Params
              ),
              methods = list(
                initialize = function(..., covarianceModel = "Identity", params) {
                  callSuper(...)
                  #TODO correct choosing cov model
                  .self$covarianceModel <- covarianceModel
                  tmp <- list()
                  for (i in params) {
                    tmp <- append(tmp, ModelParameter(label=i, type="variance param"))
                  }
                  .self$params <- tmp
                },
                asTTL = function() {
                  paste(ident(id), TYPE, COVARIANCESTRUCTURE,
                        #TODO correct choosing cov model
                        ";\n", TYPE, COVIDENTITY,
                        ";\n", LABEL, lit(label),
                        ";\n", paste(HASPART, listAsTTL(params), collapse = " ;\n "),
                        ";\n", paste(TMP_EST, listAsTTL(estimate), collapse = " ;\n "),
                        ".\n")
                }
              )
              )
}
#cs <- CovarianceStructure(label = "blockTerm", params="sigma")
#cs

RandomModelTerm <- {
  setRefClass("RandomModelTerm",
              contains = "ModelTerm",
              fields = list(
                covarianceStructure = "list" #of CovarianceStructure
                ),
              methods = list(
                asTTL = function() {
                  paste(ident(id),TYPE, RANDOMMODELTERM,
                        ";\n", LABEL, lit(label),
                        ";\n", HASORDER, lit(order),
                        ";\n", paste(ISABOUT, listAsTTL(variable), collapse = " ;\n "),
                        ";\n", paste(HASEFFECT, listAsTTL(effect), collapse = " ;\n "),
                        ";\n", paste(HASPART, listAsTTL(covarianceStructure), collapse = " ;\n "),
                        ";\n", paste(TMP_EST, listAsTTL(estimate), collapse = " ;\n "), #TODO move from term to effects
                        ".\n")
                }
              )
  )}

ErrorModelTerm <- {
  setRefClass("ErrorModelTerm",
              contains = "RandomModelTerm",
              methods = list(
                asTTL = function() {
                  paste(ident(id),TYPE, ERRORMODELTERM,
                        ";\n", LABEL, lit(label),
                        #";\n", HASORDER, lit(order),
                        #";\n", paste(ISABOUT, listAsTTL(variable), collapse = " ;\n "),
                        #";\n", paste(HASEFFECT, listAsTTL(effect), collapse = " ;\n "),
                        ";\n", paste(HASPART, listAsTTL(covarianceStructure), collapse = " ;\n "),
                        ";\n", paste(TMP_EST, listAsTTL(estimate), collapse = " ;\n "), #TODO move from term to effects
                        ".\n")
                }
              )
  )}

FixedModelTerm <- {
  setRefClass("FixedModelTerm",
              contains = "ModelTerm",
              methods = list(
                asTTL = function() {
                  paste(ident(id),TYPE, FIXEDMODELTERM, 
                        ";\n", LABEL, lit(label),
                        ";\n", HASORDER, lit(order),
                        ";\n", paste(ISABOUT, listAsTTL(variable), collapse = " ;\n "),
                        ";\n", paste(HASEFFECT, listAsTTL(effect), collapse = " ;\n "),
                        #";\n", paste("ESTIMATOR_TO_MOVE", listAsTTL(estimate), collapse = " ;\n "),
                        ".\n")
                }
              )
  )}


#fmt1 <- FixedModelTerm(label="infection")
#fmt1$variable <- c(v1)

#fmt2 <- FixedModelTerm(label="infraname")
#fmt2$variable <- c(v2)

#fmt3 <- FixedModelTerm(label="infection:infraname")
#fmt3$variable <- c(v1, v2)

#rmt1 <- RandomModelTerm(label="blockTerm")
#rmt1$variable <- list(CategoricalVariable(label="Block", levels=c("b1", "b2", "b3")), CategoricalVariable(label="Rep", levels=c("r1", "r2", "r3")))

Effect <- {
  setRefClass("Effect",
              contains = "OntologyEntity",
              fields= list(
                correspondingVarLevels = "list", #of Levels (for now)
                describesValueOf = "ANY", #dependent variable
                estimate = "list"
              ),
              methods = list(
                initialize = function(..., levels, describesValueOf) {
                  callSuper(...)
                  tmp <- list()
                  for (i in levels) {
                    tmp <- append(tmp, getEntity("VariableLevel", i))
                  }
                  .self$correspondingVarLevels <- tmp
                  .self$describesValueOf <- describesValueOf
                },
                asTTL = function() {
                  paste(ident(id), TYPE, EFFECT,
                        ";\n", TYPE, MODELPARAMETER,
                        ";\n", LABEL, lit(label),
                        ";\n", paste(ISABOUT, listAsTTL(correspondingVarLevels), collapse = " ;\n "),
                        if (length(estimate) > 0) {
                          paste(";\n", paste(TMP_EST, listAsTTL(estimate), collapse = " ;\n "))
                        },
                        #";\n", DESCRIBESVALUEOF, describesValueOf, # remove or generate automatically
                        ".\n")
                }
              )
  )}
#ef1 <- Effect(levels=list("CamB1","Drought")) # Interaction Effect
#cat(ef1$asTTL())

Lmm <- {
  setRefClass("Lmm",
              contains = "OntologyEntity",
              fields = list(
                formula = "formula",
                dependentVariable = "list", 
                independentFixedTerm = "list", # of ModelTerm
                independentRandomTerm = "list", # of ModelTerm
                errorTerm = "list", # of ModelTerm
                criterionREML = "numeric",
                criterionAIC = "numeric"
              ),
              methods = list(
                initialize = function(..., formula) {
                  callSuper(...)
                  .self$formula <- formula
                  #.self$residual <- resid
                },
                asTTL = function() {
                  paste(ident(id), TYPE, LMM,
                        ";\n", LABEL, lit(label),
                        ";\n", FORMULA, lit(deparse(formula)),
                        #";\n", CRITREML, criterionREML,
                        #";\n", CRITAIC, criterionAIC,
                        ";\n", ISMODELFOR, listAsTTL(dependentVariable),
                        ";\n", paste(HASTERM, listAsTTL(independentFixedTerm), collapse = " ;\n "),
                        ";\n", paste(HASTERM, listAsTTL(independentRandomTerm), collapse = " ;\n "),
                        ";\n", paste(HASTERM, listAsTTL(errorTerm), collapse = " ;\n "),
                        #";\n", HASTERM, lit("??? residual ???"),
                        ".\n")
                }
              )
  )}

#lmm <- Lmm(label="lmm1", formula=t1~v1*v2+(1|b1+b2))
#lmm$independentFixedTerm <- c(fmt1, fmt2, fmt3)
#lmm$independentRandomTerm <- c(rmt1)
#lmm$dependentVariable <- c(t1)

