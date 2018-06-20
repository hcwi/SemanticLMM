########################## aux functions ########################## 

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

reg #list registered objects
getEntity <- function(className, label) {
  for (r in reg) {
    classOk <- grepl(class(r), patt = className, ignore.case = T)
    if (classOk & r$label == label)
      return(r)
  }
  return(NULL)
}

listEntities <- function() {
  for (r in reg) {
    cat(sprintf("%20s %20s\n", class(r), r$label))
  }
}
#getEntity("OntologyEntity", "eoLabel")

listOfStringsToObjects <- function(objClass = "Level", objNames) {
  
  if (is(objNames,"character")) {
    
    objs <- list()
    for (objName in objNames) {
      obj <- getEntity(objClass, objName)
      if (is.null(obj)) {
        print(paste0("No level of '", objClass, "' for the name: ", objName))
      } else {
        objs <- append(objs, obj)
      }
    }
    return(objs)
    
  } else {
    
    if (is.null(objNames)) {
      return(list())
    }
    if (is(objNames, "list")) {
      if (length(objNames) == 0 | is(objNames[[1]], "OntologyEntity")) {
        return(objNames)
      } else {
        if (is(objNames[[1]], "character")) {
          return(listOfStringsToObjects(objClass, unlist(objNames)))
        }
      }
      stop(paste("objNames is a list of unknown type: ", class(objNames[[1]]),
                 "Should be a list of OntologyEntities or a Vector of characters"))
    } 
    
    stop("Unknown type of objNames")
  }
  
}

########################## LMM model classes ########################## 

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
                                        obj <- reg[[key]]
                                        assert_that(is(obj, "OntologyEntity"))
                                        cat(obj$asTTL())
                                      }
                                      i <- i + 1
                                    } else {
                                      break
                                    }
                                  }
                                  queue <<- NULL
                                },
                                ident = function(identifier) {
                                  paste0("<", gsub(identifier, patt=":", rep="."), ">")
                                },
                                lit = function(literal) {
                                  paste0("\"", literal, "\"")
                                },
                                mapping = function(type) {
                                  paste0(get(toupper(type)))
                                },
                                asTTL = function() {
                                  paste(getTTL(), ".\n")
                                },
                                getTTL = function() {
                                  paste(ident(id), TYPE, "OntologyEntity",
                                        ";\n", LABEL, lit(label))
                                },
                                listAsTTL = function(oo) {
                                  ids = "" # concatenated IDs from the 'oo' list
                                  for (o in oo) {
                                    assert_that(is(o, "OntologyEntity"))
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
#oe <- OntologyEntity(label = "eoLabel")
#oe
#str(oe)

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
                                getTTL = function() {
                                  paste(callSuper(),
                                        ";\n", TYPE, MODELPARAMETER,
                                        ";\n", "xxx:TMP_TYPE", lit(type)
                                  )
                                }
                              )
)}
#param1 <- ModelParameter(label = "param1", type = "variance")

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
                             getTTL = function() {
                               paste(callSuper(),
                                     ";\n", TYPE, HYPOTHESIS,
                                     ";\n", PVALUE, lit(pvalue),
                                     ";\n", QVALUE, lit(qvalue),
                                     ";\n", ISABOUT, listAsTTL(modelParams)
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
                            getTTL = function() {
                              paste(callSuper(),
                                    ";\n", TYPE, TESTSTATISTIC,
                                    ";\n", VALUE, lit(value),
                                    ";\n", TYPE, lit(type))
                            }
                          )
)}

#hypo1 <- Hypothesis(label="hypo0", pvalue=0.05, modelParams = list(param1))
#tstat <- Statistic(label="t-stat", type="T-student statistic", value=18.46)
#ttest <- StatisticalTest(label="t-test", df=20, pvalue=0.0023, testStatistic = list(tstat), hypothesis = list(hypo1))
#ttest

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
                          getTTL = function() {
                            paste(callSuper(),
                                  ";\n", TYPE, ESTIMATE,
                                  ";\n", VALUE, lit(value),
                                  ";\n", SE, lit(se),
                                  ";\n", ISESTIMATEOF, listAsTTL(list(isEstimateOf)))
                          }
                        )
)}
#est <- Estimate(label = "est1", value = 222, parameter = param1)
#est
#cat(est$asTTL())

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
                                getTTL = function() {
                                  paste(callSuper(),
                                        ";\n", TYPE, VARIABLELEVEL,
                                        ";\n", ISLEVELOF, listAsTTL(list(variable)))
                                }
                              )
)}

Variable <- {setRefClass("Variable",
                         contains = "OntologyEntity",
                         fields = list(
                           type = "character"
                         ),
                         methods = list(
                           initialize = function(..., type) {
                             callSuper(...)
                             .self$type <- type
                           },
                           getTTL = function() {
                             paste(callSuper(),
                                   ";\n", TYPE, VARIABLE,
                                   ";\n", TYPE, mapping(type)
                                   )
                           }
                         )
)}

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
                           getTTL = function() {
                             paste(callSuper(),
                                   ";\n", TYPE, CATEGORICALVARIABLE,
                                   ";\n", HASLEVEL, listAsTTL(levels))
                           }
                         )
)}

#####
# CategoricalIndependentVariable <- {setRefClass("CategoricalIndependentVariable",
#                                               contains = "CategoricalVariable",
#                                               methods = list(
#                                                 initialize = function(... , levels=character()) {
#                                                   callSuper(..., levels = levels)
#                                                 },
#                                                 getTTL = function() {
#                                                   paste(callSuper(),
#                                                         ";\n", TYPE, CATEGORICALINDEPENDENTVARIABLE
#                                                   )
#                                                 }
#                                               )
#                                               )}
                                              
#v1 <- CategoricalVariable(label="infection", levels = c("infected", "non-infected"))
#v2 <- CategoricalVariable(label="infraname", levels = c("CamB1", "Maresi", "Soldo"))
#t1 <- Variable(label="plantHeight")
#camB1 <- VariableLevel(label="CamB1", variable=v2)
#camB1
#v3 <- CategoricalIndependentVariable(label="infraname", levels = c("CamB1", "Maresi", "Soldo")) 
#####

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
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, MODELTERM,
                        ";\n", HASORDER, lit(order),
                        ";\n", paste(ISABOUT, listAsTTL(variable), collapse = " ;\n "),
                        ";\n", paste(HASEFFECT, listAsTTL(effect), collapse = " ;\n "),
                        ";\n", paste(TMP_EST, listAsTTL(estimate), collapse = " ;\n ")) #TODO move from term to effects
                }
              )
  )}

RandomModelTerm <- {
  setRefClass("RandomModelTerm",
              contains = "ModelTerm",
              fields = list(
                covarianceStructure = "list" #of CovarianceStructure #TODO change to "ANY" and remove lists
                ),
              methods = list(
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, RANDOMMODELTERM,
                        ";\n", paste(HASPART, listAsTTL(covarianceStructure), collapse = " ;\n ")
                        )
                }
              )
  )}

ErrorModelTerm <- {
  setRefClass("ErrorModelTerm",
              contains = "RandomModelTerm",
              methods = list(
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, ERRORMODELTERM
                        )
                }
              )
  )}

FixedModelTerm <- {
  setRefClass("FixedModelTerm",
              contains = "ModelTerm",
              methods = list(
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, FIXEDMODELTERM
                        )
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

CovarianceStructure <- {
  setRefClass("CovarianceStructure",
              contains = "OntologyEntity",
              fields = list(
                covarianceModel = "character",
                params = "list", # of ModelParams
                estimate = "list" # of Estimates of Params
              ),
              methods = list(
                initialize = function(..., covarianceModel = "Identity", params = list("sigma2e")) {
                  callSuper(...)
                  #TODO correct choosing cov model
                  .self$covarianceModel <- covarianceModel
                  tmp <- list()
                  for (i in params) {
                    tmp <- append(tmp, ModelParameter(label=i, type="variance param"))
                  }
                  .self$params <- tmp
                },
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, COVARIANCESTRUCTURE, #TODO correct choosing cov model
                        ";\n", TYPE, COVIDENTITY,
                        ";\n", paste(HASPART, listAsTTL(params), collapse = " ;\n "),
                        if (length(estimate) > 0) {
                          paste(";\n", paste(TMP_EST, listAsTTL(estimate), collapse = " ;\n "))
                        })
                }
              )
  )
}
#cs <- CovarianceStructure(label = "blockTerm", params="sigma")
#cs

Effect <- {
  setRefClass("Effect",
              contains = "ModelParameter",
              fields= list(
                correspondingVarLevels = "list", #of Levels (for now)
                describesValueOf = "ANY", #dependent variable
                estimate = "list"
              ),
              methods = list(
                initialize = function(..., levels, describesValueOf) {
                  callSuper(type="Effect", ...)
                  tmp <- listOfStringsToObjects("VariableLevel", levels)
                  .self$correspondingVarLevels <- tmp
                  .self$describesValueOf <- describesValueOf
                },
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, EFFECT,
                        ";\n", TYPE, MODELPARAMETER,
                        if (length(correspondingVarLevels) > 0) {
                          paste(";\n", paste(ISABOUT, listAsTTL(correspondingVarLevels), collapse = " ;\n "))
                        },
                        if (length(estimate) > 0) {
                          paste(";\n", paste(TMP_EST, listAsTTL(estimate), collapse = " ;\n "))
                        }
                        #";\n", DESCRIBESVALUEOF, describesValueOf, # remove or generate automatically
                      )
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
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, LMM,
                        ";\n", FORMULA, lit(deparse(formula)),
                        #";\n", CRITREML, criterionREML,
                        #";\n", CRITAIC, criterionAIC,
                        ";\n", ISMODELFOR, listAsTTL(dependentVariable),
                        ";\n", paste(HASTERM, listAsTTL(independentFixedTerm), collapse = " ;\n "),
                        ";\n", paste(HASTERM, listAsTTL(independentRandomTerm), collapse = " ;\n "),
                        ";\n", paste(HASTERM, listAsTTL(errorTerm), collapse = " ;\n ")
                        #";\n", HASTERM, lit("??? residual ???")
                        )
                }
              )
  )}

#lmm <- Lmm(label="lmm1", formula=t1~v1*v2+(1|b1+b2))
#lmm$independentFixedTerm <- c(fmt1, fmt2, fmt3)
#lmm$independentRandomTerm <- c(rmt1)
#lmm$dependentVariable <- c(t1)

Process <- {
  setRefClass("Process",
              contains = "OntologyEntity",
              fields= list(
                hasInput = "list",
                hasOutput = "list",
                hasPart = "list",
                processType = "character"
              ),
              methods = list(
                initialize = function(..., processType) {
                  callSuper(...)
                  .self$processType <- processType
                },
                getTTL = function() {
                  gsub(pattern=" +", rep=" ",
                    paste(callSuper(),
                          ";\n", TYPE, mapping(processType),
                        if (length(hasInput) > 0) {
                          paste(";\n", paste(HASINPUT, listAsTTL(hasInput), collapse = " ;\n "))
                        },
                        if (length(hasOutput) > 0) {
                          paste(";\n", paste(HASOUTPUT, listAsTTL(hasOutput), collapse = " ;\n "))
                        },
                        if (length(hasPart) > 0) {
                          paste(";\n", paste(HASPART, listAsTTL(hasPart), collapse = " ;\n "))
                        }
                        )
                  )}
              )
  )}
#proc <- Process("modelFitting", processType="MODELFITTING")
#proc2 <- Process("modelParamEstimation", processType="MODELPARAMETERESTIMATION")
#proc$hasPart <- append(proc$hasInput, proc2)
#proc
