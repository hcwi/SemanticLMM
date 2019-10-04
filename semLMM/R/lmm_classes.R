########################## aux functions ##########################

# local working environment
lenv <- new.env()

# logging setup
assign("logger", log4r::create.logger(logfile = "semLMM.log", level = "WARN"), envir = lenv)
setLogger <- function(logfile = "semLMM.log", level = "DEBUG") {
  assign("logger", log4r::create.logger(logfile = logfile, level = level), envir = lenv)
}

# whole graph - list of registered objects
graph <- new.env(parent = emptyenv(), hash = TRUE)
newGraph <- function() {
  log4r::debug(lenv$logger, match.call())
   rm(list = ls(graph), envir = graph)
  graph
}

register <- function(o) {
  key = o$label
  assertthat::assert_that(is(o, "AnnotatedEntity"), msg = paste("Trying to register non Annotated Entity: ", o$label))
  log4r::info(lenv$logger, paste("Registering object", key, o$id))
  assign(x = key, value = c(graph[[key]], o), envir = graph)
}

getEntities <- function(className, label) {
  labelMatch <- graph[[label]]
  classMatch <- grepl(lapply(labelMatch, class), pattern=className, ignore.case = T)
  matchingEntities <- labelMatch[classMatch]
  return(matchingEntities)
}

getEntity <- function(className, label, relatedClassLabel = NULL) {

  matchingEntities <- getEntities(className, label)

  if (length(matchingEntities) == 1) {
    #"No need to select from many, returning the first and only one"
    #print(paste("Ok, one matching entity found for ", className, label))
    return(matchingEntities[[1]])
  }
  else if (length(matchingEntities) > 1) {
    log4r::debug(lenv$logger, paste("More than one matching entity found for ", className, label, ". Checking if further selection possible.."))
    if (grepl(className, pattern = "level|valueSpecification", ignore.case = T) && is.character(relatedClassLabel)) {
      correctVariable = grepl(lapply(matchingEntities, function(x) x$variable$label), pattern = relatedClassLabel)
      if (sum(correctVariable) == 1) {
        log4r::debug(lenv$logger, paste("Yes, one entity selected for ", relatedClassLabel))
        return(matchingEntities[correctVariable])
      }
      else {
        log4r::debug(lenv$logger, paste("More than one matching entity found for ", className, label, " despite trying ", relatedClassLabel, " to specify the selection"))
        return(NULL)
      }
    } else {
      log4r::debug(lenv$logger, paste("More than one matching entity found for ", className, label, ". No relatedClassLabel to specify the selection is declared or no label given."))
      stop(paste("More than one matching entity found for ", className, label, ". No relatedClassLabel to specify the selection is declared or no label given."))
    }
  } else {
    log4r::debug(lenv$logger, paste("Nothing to return for", className, label, relatedClassLabel))
  }

}


listEntities <- function() {
  entities <- mget(ls(graph), envir = graph)
  printEntity <- function(x) (paste(x$label, class(x)))
  printedEntities <- sapply(entities, function(x) sapply(x, printEntity))
  unlist(printedEntities, use.names = F)

}
#getEntity("AnnotatedEntity", "eoLabel")

listOfStringsToObjects <- function(objClass = "Level", objNames) {

  if (is(objNames,"character")) {

    objs <- list()
    for (objName in objNames) {
      obj <- getEntity(objClass, objName)
      if (is.null(obj)) {
        log4r::error(lenv$logger, paste0("Haven't found '", objClass, "' for the name: ", objName))
        print(paste0("Haven't found '", objClass, "' for the name: ", objName))
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
      if (length(objNames) == 0 || is(objNames[[1]], "AnnotatedEntity")) {
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

init <- function() {
  newGraph()
}


########################## LMM model classes ##########################

AnnotatedEntity <- {setRefClass("AnnotatedEntity",
                                fields = list(
                                  id = "character",
                                  label = "character",
                                  type = "list",
                                  comments = "list"
                                ),
                                methods = list(
                                  initialize = function(label, type = list(), comments = list()) {
                                    .self$label <- label
                                    tmp <- type
                                    if (is.character(tmp)) {
                                      tmp <- as.list(tmp)
                                    }
                                    .self$type <- tmp
                                    .self$comments <- comments
                                    .self$id <- setID()
                                    register(.self)
                                  },
                                  setID = function() {
                                    id <- format(as.numeric(Sys.time())*100000, digits=15) #format(Sys.time(), "%y%m%d%H%M%S")
                                    fullID <- paste(as.character(class(.self)), .self$label, id, sep="_")
                                    return(fullID)
                                  },
                                  show = function() {
                                    assign("queue", list(), envir = lenv)
                                    lenv$queue[.self$id] <- FALSE
                                    cat(asTTL())
                                    i <- 1
                                    while (TRUE) {
                                      if (i <= length(lenv$queue)) {
                                        key <- names(lenv$queue)[i]
                                        obj <- lenv$queue[[i]]
                                        log4r::info(lenv$logger, paste("Showing", key, "==", lenv$queue[key], "(", i, "/", length(lenv$queue), ")"))
                                        if (!isFALSE(obj)) {
                                          assertthat::assert_that(is(obj, "AnnotatedEntity"), msg = paste("No object in graph for key: ", key))
                                          lenv$queue[[i]] <- FALSE # being processed, not queuing for printing any more (if still ahead);
                                          cat(obj$asTTL())
                                        }
                                        i <- i + 1
                                      } else {
                                        break
                                      }
                                    }
                                    lenv$queue <- NULL
                                  },
                                  ident = function(identifier) {
                                    paste0("<", gsub(identifier, pattern=":", replacement ="."), ">")
                                  },
                                  lit = function(literal) {
                                    paste0("\"", literal, "\"")
                                  },
                                  num = function(number) {
                                    if (length(number) == 0 || number == "") {
                                      paste0("\"", number, "\"")
                                    } else {
                                      paste0("\"", number, "\"^^xsd:float")
                                    }
                                  },
                                  ont = function(type) {
                                    paste0(get(toupper(type), envir = termsEnv))
                                  },
                                  asTTL = function() {
                                    paste(getTTL(), ".\n")
                                  },
                                  getTTL = function() { #TODO simplify, after refactoring the structure is too complex!
                                    res <- innerGetTTL()
                                    res
                                  },
                                  innerGetTTL = function() {
                                    types <- ""
                                    for (t in .self$type) {
                                      types <- paste(types, ";\n", ont("TYPE"), ont(t))
                                    }
                                    comments <- ""
                                    if (length(.self$comments)>0) {
                                      comments <- paste(";\n", .self$comments, collapse = "")
                                    }
                                    paste(ident(id), ont("LABEL"), lit(label),
                                          types,
                                          comments
                                    )
                                  },
                                  listAsTTL = function(oo) {
                                    ids = "" # concatenated IDs from the 'oo' list
                                    for (o in oo) {
                                      assertthat::assert_that(is(o, "AnnotatedEntity"))
                                      ids <- paste(ids, ", ", ident(o$id)) # add ID to the string
                                      if (!exists("queue", envir = lenv) || is.null(lenv$queue)) {
                                        lenv$queue <- list()
                                      }
                                      lenv$queue[o$id] <- list(o) # add to the queue and / or set printing (if already exists)
                                    }
                                    substring(ids,3) # return concatenated without initial separators ', '
                                  },
                                  saveTriples = function(graphName = NULL) {
                                    if(is.null(graphName)) {
                                      graphName <- .self$id
                                    }
                                    capture.output(cat(prefixes),
                                                   cat("<graphs/graph_", graphName, ">", sep=""),
                                                   cat(" {\n"),
                                                   .self,
                                                   cat("}"),
                                                   file = paste0(#"out", .Platform$file.sep,
                                                                 graphName, ".trig"))
                                    result <- paste0("Exported to ", graphName, ".trig")
                                    log4r::info(lenv$logger, result)
                                    result
                                  }
                                )
)}
# oe <- AnnotatedEntity(label = "eoLabel", type="ModelParameter")
# oe
#str(oe)

ObjProperty <- {setRefClass("ObjProperty",
                            contains = "AnnotatedEntity",
                            fields = list(
                              pred = "character",
                              obj = "list", #list of objects this entity is about, e.g. AIC isAbout model, df is about AIC
                              value = "ANY"
                            ),
                            methods = list(
                              initialize = function(... , pred, obj, value) {
                                if (any(sapply(as.list(match.call()), is.null))) {
                                  warning("NULL argument of a function", match.call())
                                }
                                callSuper(...)
                                .self$pred <- pred
                                .self$obj <- ifelse(is.list(obj), obj, list(obj))
                                .self$value <- value
                              },
                              innerGetTTL = function() {
                                paste(callSuper(),
                                      if (is.numeric(value) && length(value)==1) {
                                        paste(";\n", ont("VALUE"), num(value))
                                      } else {
                                        paste(";\n", ont("VALUE"), lit(value))
                                      },
                                      ";\n", ont(pred), listAsTTL(obj)
                                )
                              }
                            )
)}
#(op <- ObjProperty(label="REML", type="critREML", pred="isAbout", value=98765, obj=AnnotatedEntity("testEntity")))

Hypothesis <- {setRefClass("Hypothesis",
                           contains = "AnnotatedEntity",
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
                             innerGetTTL = function() {
                               paste(callSuper(),
                                     ";\n", ont("TYPE"), ont("HYPOTHESIS"),
                                     #";\n", ont("PVALUE"), num(pvalue),
                                     #";\n", ont("QVALUE"), num(qvalue),
                                     ";\n", ont("ISABOUT"), listAsTTL(modelParams)
                               )
                             }
                           )
)}

Dataset <- {setRefClass("Dataset",
                        contains = "AnnotatedEntity",
                        fields = list(
                          #TODO change URL to description (as in termsEnv$DESCRIPTION == dc:description)
                          url = "character",
                          variables = "list"
                        ),
                        methods = list(
                          initialize = function(... , url=character(0), variables = list()) {
                            callSuper(...)
                            .self$url <- url
                            .self$variables <- variables
                          },
                          innerGetTTL = function() {
                            paste(callSuper(),
                                  ";\n", ont("TYPE"), ont("DATASET"),
                                  if (length(url)>0) {paste(";\n", ont("DESCRIPTION"), lit(url))},
                                  ";\n", ont("CREATOR"), lit("HCK"),
                                  if (length(variables) > 0) {
                                    paste(";\n", paste(ont("HASPART"), listAsTTL(variables), collapse = " ;\n ")) }

                            )
                          }
                        )
)}
Statistic <- {setRefClass("Statistic",
                          contains = "AnnotatedEntity",
                          fields = list(
                            value = "numeric",
                            isAbout = "list",
                            hasPart = "list"
                          ),
                          methods = list(
                            initialize = function(... , value=numeric(0), isAbout = list(), hasPart = list()) {
                              callSuper(...)
                              if (!(length(value) == 0 || is.na(value))) {
                                .self$value <- value
                              }
                              .self$isAbout <- isAbout
                              .self$hasPart <- hasPart
                            },
                            innerGetTTL = function() {
                              paste(callSuper(),
                                    #";\n", TYPE, STATISTIC,
                                    if (is.numeric(value) && length(value)==1) {
                                      paste(";\n", ont("VALUE"), num(value)) },
                                    if (length(isAbout) > 0) {
                                      paste(";\n", paste(ont("ISABOUT"), listAsTTL(isAbout), collapse = " ;\n ")) },
                                    if (length(hasPart) > 0) {
                                      paste(";\n", paste(ont("HASPART"), listAsTTL(hasPart), collapse = " ;\n ")) }
                              )}
                          )
)}

#hypo1 <- Hypothesis(label="hypo0", pvalue=0.05, modelParams = list(param1))
#tstat <- Statistic(label="t-stat", type=list("Tstatistic"), value=18.46)
#ttest <- StatisticalTest(label="t-test", df=20, pvalue=0.0023, testStatistic = list(tstat), hypothesis = list(hypo1))
#ttest

Estimate <- {setRefClass("Estimate",
                         contains = "AnnotatedEntity",
                         fields = list(
                           value = "numeric",
                           se = "numeric",
                           seObj = "list",
                           conflev = "numeric",
                           ucl = "numeric",
                           lcl = "numeric",
                           isEstimateOf = "ANY" #ModelParameter
                         ),
                         methods = list(
                           initialize = function(... , value, parameter, se = NULL) {
                             callSuper(...)
                             .self$value <- value
                             .self$isEstimateOf <- parameter
                             if (!is.null(se)) {.self$se <- se}
                           },
                           innerGetTTL = function() {
                             if (is.null(.self$seObj) || length(.self$seObj) == 0 ) {
                               if(!is.na(se) && length(se) == 1 && se != "") {
                                 .self$seObj <- list(Statistic(paste0("se_", label), type=list("se"), value=se, isAbout = list(.self)))
                               }}
                             if (!is.null(.self$seObj) && length(.self$seObj) > 0) {
                               listAsTTL(seObj)
                             }
                             paste(callSuper(),
                                   ";\n", ont("TYPE"), ont("ESTIMATE"),
                                   ";\n", ont("VALUE"), num(value),
                                   ";\n", ont("ISESTIMATEOF"), listAsTTL(list(isEstimateOf)))
                           }
                         )
)}
#est <- Estimate(label = "est1", value = 222, parameter = param1)
#est
#cat(est$asTTL())

ValueSpecification <- {setRefClass("ValueSpecification",
                                   contains = "AnnotatedEntity",
                                   fields = list(
                                     variable = "ANY",
                                     value = "ANY"
                                   ),
                                   methods = list(
                                     initialize = function(..., variable, value = NULL) {
                                       callSuper(...)
                                       .self$variable <- variable
                                       .self$value <- value
                                     },
                                     innerGetTTL = function() {
                                       paste(callSuper(),
                                             ";\n", ont("TYPE"), ont("VALUESPECIFICATION"),
                                             if (!is.null(value)) {
                                               if (is.numeric(value) && length(value)==1) {
                                                 paste(";\n", ont("VALUE"), num(value))
                                               } else {
                                                 paste(";\n", ont("VALUE"), lit(value))
                                               }
                                             }
                                             #, ";\n", ont("SPECIFIESVALUEOF"), listAsTTL(list(variable))
                                       )
                                     }
                                   )
)}

VariableLevel <- {setRefClass("VariableLevel",
                              contains = "ValueSpecification",
                              methods = list(
                                innerGetTTL = function() {
                                  paste(callSuper(),
                                        ";\n", ont("TYPE"), ont("VARIABLELEVEL"),
                                        ";\n", ont("TYPE"), ont("CATEGORICALVALUESPECIFICATION"))
                                }
                              )
)}

Variable <- {setRefClass("Variable",
                         contains = "AnnotatedEntity",
                         methods = list(
                           initialize = function(...) {
                             callSuper(...)
                           },
                           innerGetTTL = function() {
                             paste(callSuper()
                                   #,
                                   #";\n", ont("TYPE"), ont("VARIABLE")
                             )
                           }
                         )
)}

ContinuousVariable <- {setRefClass("ContinuousVariable",
                                   contains = "Variable",
                                   fields = list(
                                     levels = "list" # of VariableLevel
                                   ),
                                   methods = list(
                                     initialize = function(... , levels=numeric()) {
                                       callSuper(...)
                                       .self$levels <- list()
                                       for (l in levels){
                                         lab = paste0(.self$label, "=", as.character(l))
                                         .self$levels <- append(.self$levels,
                                                                ValueSpecification(label=lab, value=as.numeric(l), variable=.self))
                                       }
                                     },
                                     innerGetTTL = function() {
                                       paste(callSuper(),
                                             ";\n", ont("TYPE"), ont("CONTINUOUSVARIABLE"),
                                             ";\n", ont("HASVALUESPECIFICATION"), listAsTTL(levels))
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
                                      innerGetTTL = function() {
                                        paste(callSuper(),
                                              ";\n", ont("TYPE"), ont("CATEGORICALVARIABLE"),
                                              ";\n", ont("HASVALUESPECIFICATION"), listAsTTL(levels))
                                      }
                                    )
)}

CompoundVariable <- {setRefClass("CompoundVariable",
                                 contains = "Variable",
                                 fields = list(
                                   levels = "list" # of Variables
                                 ),
                                 methods = list(
                                   initialize = function(... , levels=list()) {
                                     callSuper(...)
                                     .self$levels <- list()
                                     for (l in levels){
                                       if (!is(l, "Variable")) {
                                         l <- ContinuousVariable(l, levels=list(1), type=list("DependentVariable"))
                                       }
                                       .self$levels <- append(.self$levels, l)
                                     }
                                   },
                                   innerGetTTL = function() {
                                     paste(callSuper(),
                                           ";\n", ont("TYPE"), ont("CONTINUOUSVARIABLE"),
                                           ";\n", ont("TYPE"), ont("COMPOUNDVARIABLE"),
                                           ";\n", ont("HASPART"), listAsTTL(levels))
                                   }
                                 )
)}
#tmp <- CompoundVariable("CompoundVar", levels=list("y1", "y2", "y3"))

#####
# CategoricalIndependentVariable <- {setRefClass("CategoricalIndependentVariable",
#                                               contains = "CategoricalVariable",
#                                               methods = list(
#                                                 initialize = function(... , levels=character()) {
#                                                   callSuper(..., levels = levels)
#                                                 },
#                                                 innerGetTTL = function() {
#                                                   paste(callSuper(),
#                                                         ";\n", ont("TYPE"), ont("CATEGORICALINDEPENDENTVARIABLE")
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
              contains = "AnnotatedEntity",
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
                innerGetTTL = function() {
                  paste(callSuper(),
                        #";\n", ont("TYPE"), ont("MODELTERM"),
                        ";\n", ont("HASORDER"), lit(order),
                        if (length(variable) > 0) {
                          paste(";\n", paste(ont("ISABOUT"), listAsTTL(variable), collapse = " ;\n "))
                        },
                        if (length(effect) > 0) {
                          paste(";\n", paste(ont("HASEFFECT"), listAsTTL(effect), collapse = " ;\n "))
                        }
                  )}
              )
  )}

RandomModelTerm <- {
  setRefClass("RandomModelTerm",
              contains = "ModelTerm",
              fields = list(
                covarianceStructure = "list" #of CovarianceStructure #TODO change to "ANY" and remove lists
              ),
              methods = list(
                innerGetTTL = function() {
                  paste(callSuper(),
                        ";\n", ont("TYPE"), ont("RANDOMMODELTERM"),
                        ";\n", paste(ont("HASPART"), listAsTTL(covarianceStructure), collapse = " ;\n ")
                  )
                }
              )
  )}

ErrorModelTerm <- {
  setRefClass("ErrorModelTerm",
              contains = "RandomModelTerm",
              methods = list(
                innerGetTTL = function() {
                  paste(callSuper(),
                        ";\n", ont("TYPE"), ont("ERRORMODELTERM")
                  )
                }
              )
  )}

FixedModelTerm <- {
  setRefClass("FixedModelTerm",
              contains = "ModelTerm",
              methods = list(
                innerGetTTL = function() {
                  paste(callSuper(),
                        ";\n", ont("TYPE"), ont("FIXEDMODELTERM")
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

# CovarianceStructure <- {
#   setRefClass("CovarianceStructure",
#               contains = "AnnotatedEntity",
#               fields = list(
#                 params = "list", # of ModelParams
#                 estimate = "list" # of Estimates of Params
#               ),
#               methods = list(
#                 initialize = function(..., covType = list("covIdentity"), params = list("sigma2e")) {
#                   callSuper(...)
#                   #TODO correct choosing cov model
#                   tmp <- list()
#                   for (i in params) {
#                     tmp <- append(tmp, Parameter(label=i, type=list("ModelParameter", "varianceComponent")))
#                   }
#                   .self$params <- tmp
#                 },
#                 innerGetTTL = function() {
#                   paste(callSuper(),
#                         ";\n", ont("TYPE"), ont("COVARIANCESTRUCTURE"),
#                         ";\n", paste(ont("HASPART"), listAsTTL(params), collapse = " ;\n ")
#                         )
#                 }
#               )
#   )
# }
# cs <- CovarianceStructure(label = "blockTerm", params="sigma")
#cs

CovarianceStructure <- {
  setRefClass("CovarianceStructure",
              contains = "AnnotatedEntity",
              fields = list(
                params = "list", # of ModelParams
                estimate = "list", # of Estimates of Params
                covModel = "character", # covariance model (Identity, CompoundSymmetry, ...)
                vars = "list"
              ),
              methods = list(
                initialize = function(..., covModel = "pdIdent", params = list("sigma2e"), vars = list()) {
                  callSuper(...)
                  .self$covModel <- covModel
                  ps <- list()
                  for (i in params) {
                    #TODO add if it is a variance, covariance or correlation ?!
                    if (is(i, "Parameter")) {
                      i$type <- append(i$type, "ModelParameter")
                      i$type <- append(i$type, "varianceParameter")
                    } else {
                      i <- Parameter(label=i, type=list("ModelParameter", "varianceParameter"))
                    }
                    ps <- append(ps, i)
                  }
                  .self$params <- ps
                  .self$vars <- vars
                },
                innerGetTTL = function() {
                  paste(callSuper(),
                        ";\n", ont("TYPE"), ont("COVARIANCESTRUCTURE"),
                        ";\n", ont("TYPE"), ont(covModel),
                        ";\n", paste(ont("HASPART"), listAsTTL(params), collapse = " ;\n "),
                        if (length(vars) > 0) {
                          paste(";\n", paste(ont("ISABOUT"), listAsTTL(vars), collapse = " ;\n "))
                        }
                  )
                }
              )
  )
}


#param1 <- ModelParameter(label = "param1", type = list("variance"))
# param1

Parameter <- {
  setRefClass("Parameter",
              contains = "AnnotatedEntity",
              fields= list(
                correspondingVarLevels = "list", #of Levels (for now)
                relativeTo = "list", #list of reference effects (only ONE? reference effect)
                specifiesValueOf = "ANY", #dependent variable
                estimate = "list",
                effectType = "character"
              ),
              methods = list(
                initialize = function(..., levels=list(), reference = list(), valueOf = list(), effectType=character(0)) {
                  callSuper(...)
                  .self$correspondingVarLevels <- listOfStringsToObjects("VariableLevel", levels)
                  .self$relativeTo <- reference
                  if (length(valueOf) == 0) {
                    #TODO add variable
                    #warning(paste("No isAbout variable declared for parameter:", label))
                  } else if (length(valueOf) == 1 && typeof(valueOf) != "list") {
                    valueOf <- list(valueOf)
                  }
                  .self$specifiesValueOf <- valueOf
                  if (length(effectType) == 0) {
                    #warning(paste("No fixed/random effect type declared for parameter:", label))
                  }
                  .self$effectType <- effectType
                },
                innerGetTTL = function() {
                  paste(callSuper(),
                        ";\n", ont("TYPE"), ont("MODELPARAMETER"),
                        if (length(correspondingVarLevels) > 0) {
                          paste(";\n", paste(ont("ISABOUT"), listAsTTL(correspondingVarLevels), collapse = " ;\n "))
                        },
                        if (length(relativeTo) > 0) {
                          paste(";\n", ont("TYPE"), ont("RELATIVEEFFECT"),
                                ";\n", paste(ont("ISRELATIVETO"), listAsTTL(relativeTo), collapse = " ;\n "))
                        },
                        #if (length(estimate) > 0) {
                        #  paste(";\n", paste("xxx:TMP_EST", listAsTTL(estimate), collapse = " ;\n "))
                        #},
                        if (length(effectType) > 0) {
                          if (effectType == "fixed") {
                            paste(";\n", ont("TYPE"), ont("FIXEDEFFECT"), #TODO: why doesn't print?!
                                  if (length(specifiesValueOf) > 0) {
                                    paste(";\n", paste(ont("HASFIXEDEFFECTON"), listAsTTL(specifiesValueOf), collapse = " ;\n "))
                                  })
                          } else if (effectType == "random") {
                            paste(";\n", ont("TYPE"), ont("RANDOMEFFECT"),
                                  if (length(specifiesValueOf) > 0) {
                                    paste(";\n", paste(ont("HASRANDOMEFFECTON"), listAsTTL(specifiesValueOf), collapse = " ;\n "))
                                  })
                          } else {
                            paste(";\n", ont("TYPE"), ont("EFFECT"),
                                  ";\n", paste(ont("HASEFFECTON"), listAsTTL(specifiesValueOf), collapse = " ;\n "))
                          }
                        }
                  )
                }
              )
  )}
# test params
# cv <- CategoricalVariable("testVar", levels = list("CamB1", "Drought"))
# ef1 <- Parameter("testEffect", levels=list("CamB1","Drought"), type="EMM") # Interaction Effect
# cv2 <- CategoricalVariable("testVar2", levels = list("CamB2", "Drought2"))
# ef2 <- Parameter("testRelativeEffect", levels=list("CamB2","Drought2"), reference = list(ef1), effectType="random", valueOf=ContinuousVariable("y")) # Interaction Effect
# cat(ef1$asTTL())
# cat(ef2$asTTL())
# ef3 <- Parameter("testRelativeEffect", levels=list("CamB2","Drought2"), reference = list(ef1), effectType="xxx", valueOf=ContinuousVariable("y")) # Interaction Effect
# cat(ef3$asTTL())

# ParametricFunction <- {
#   setRefClass("ParametricFunction",
#               contains = "AnnotatedEntity",
#               fields = list(
#                 formula = "ANY",
#                 parameters = "list" #of model parameters
#               ),
#               methods = list(
#                 initialize = function(..., params = list(), formula = NULL) {
#                   callSuper(...)
#                   .self$parameters <- params
#                   .self$formula <- formula
#                 },
#                 innerGetTTL = function() {
#                   paste(callSuper(),
#                         ";\n", ont("TYPE"), ont("PARAMETRICFUNCTION"),
#                         #";\n", ont("TYPE"), ont("EFFECT"),
#                         #";\n", ont("TYPE"), ont("MODELPARAMETER"),
#                         ";\n", ont("FORMULA"), lit(formula),
#                         if (length(parameters) > 0) {
#                           paste(";\n", paste(ont("ISFUNCTIONOF"), listAsTTL(parameters), collapse = " ;\n "))
#                         }
#                   )
#                 }
#               )
#   )}

# pf <- ParametricFunction("paramFunction0" )
# pf2 <- ParametricFunction("paramFunction2", params = list(param1, param1), formula = "blablabla")
# pf2


# Contrast <- {
#   setRefClass("Contrast",
#               contains = "ParametricFunction",
#               methods = list(
#                 innerGetTTL = function() {
#                   paste(callSuper(),
#                         ";\n", TYPE, CONTRAST
#                   )}
#               )
#   )}
# param1 <- ModelParameter(label = "param1", type = list("variance"))
# param2 <- ModelParameter(label = "param2", type = list("variance"))
# con1 <- Contrast("Contrast0", params = list(param1, param2), formula = "???", type=list("TreatmentContrast"))
# con1

StudyDesign <- {
  setRefClass("StudyDesign",
              contains = "AnnotatedEntity",
              fields = list(
                vars = "list"
              ),
              methods = list(
                initialize = function(..., declares) {
                  callSuper(...)
                  .self$vars <- declares
                },
                innerGetTTL = function() {
                  paste(callSuper(),
                        ";\n", ont("TYPE"), ont("STUDYDESIGN"),
                        ";\n", paste(ont("DECLARES"), listAsTTL(vars), collapse = " ;\n ")
                  )
                }
              )
  )}

DesignMatrix <- {
  setRefClass("DesignMatrix",
              contains = "AnnotatedEntity",
              fields = list(
                studyDesign = "list"
              ),
              methods = list(
                initialize = function(..., declares) {
                  callSuper(...)
                  .self$studyDesign <- list(StudyDesign("sd", declares = declares))
                },
                innerGetTTL = function() {
                  paste(callSuper(),
                        ";\n", ont("TYPE"), ont("DESIGNMATRIX"),
                        ";\n", ont("DESCRIPTION"), lit("binary"),
                        ";\n", paste(ont("DENOTES"), listAsTTL(studyDesign), collapse = " ;\n ")
                  )
                }
              )
  )}


Lmm <- {
  setRefClass("Lmm",
              contains = "AnnotatedEntity",
              fields = list(
                formula = "list",
                dependentVariable = "list",
                independentFixedTerm = "list", # of ModelTerm
                independentRandomTerm = "list", # of ModelTerm
                errorTerm = "list", # of ModelTerm
                criterionREML = "numeric",
                criterionAIC = "numeric",
                df = "numeric",
                criterionBIC = "numeric",
                variables = "list",
                designMatrix = "list",
                quality= "list"
              ),
              methods = list(
                initialize = function(..., formula, vars = list()) {
                  callSuper(...)
                  if (is.language(formula)) {
                    formula <- deparse(formula)
                  }
                  if (is.character(formula)) {
                    .self$formula <- list(ObjProperty(label="formula", type="formula", pred="denotes", obj=.self, value=formula))
                  } else {
                    stop("Cannot generate 'formula' ObjProperty")
                  }
                  #.self$residual <- resid
                  .self$variables <- vars
                },
                getQuality = function() {
                  if (length(.self$quality) == 0) {
                    props <- list() # get model props
                    if (length(criterionREML)) {
                      props <- append(props, ObjProperty(label="REML", type="critREML", pred="isAbout", value=criterionREML, obj=.self))
                    }
                    if (length(criterionAIC)) {
                      props <- append(props, ObjProperty("AIC", "critAIC", pred="isAbout", value=criterionAIC, obj=.self))
                    }
                    if (length(criterionBIC)) {
                      props <- append(props, ObjProperty("BIC", "critBIC", pred="isAbout", value=criterionBIC, obj=.self))
                    }
                    .self$quality <- props
                  }
                  .self$quality
                },
                innerGetTTL = function() {
                  if (is.null(.self$designMatrix) || length(.self$designMatrix) == 0) {
                    .self$designMatrix <- list(DesignMatrix("dm", declares = append(variables, dependentVariable)))
                  }
                  props <- getQuality()
                  if (length(df)) {
                    props <- append(props, ObjProperty("DF", "df", pred="isAbout", value=df, obj=.self))
                  }
                  if (length(props) > 0) {
                    devNull <- listAsTTL(props) # puts properties in the queue for printing
                  }
                  #formula <- ObjProperty(label="formula", type="formula", pred="denotes", obj=.self, value=formula)
                  paste(callSuper(),
                        ";\n", ont("TYPE"), ont("LMM"),
                        ";\n", ont("ISDENOTEDBY"), listAsTTL(formula),
                        ";\n", ont("ISMODELFOR"), listAsTTL(dependentVariable),
                        ";\n", paste(ont("HASTERM"), listAsTTL(independentFixedTerm), collapse = " ;\n "),
                        ";\n", paste(ont("HASTERM"), listAsTTL(independentRandomTerm), collapse = " ;\n "),
                        ";\n", paste(ont("HASTERM"), listAsTTL(errorTerm), collapse = " ;\n "),
                        ";\n", paste(ont("ISDENOTEDBY"), listAsTTL(designMatrix), collapse = " ;\n ")
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
              contains = "AnnotatedEntity",
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
                innerGetTTL = function() {
                  gsub(pattern=" +", rep=" ",
                       paste(callSuper(),
                             ";\n", ont("TYPE"), ont(processType),
                             if (length(hasInput) > 0) {
                               paste(";\n", paste(ont("HASINPUT"), listAsTTL(hasInput), collapse = " ;\n "))
                             },
                             if (length(hasOutput) > 0) {
                               paste(";\n", paste(ont("HASOUTPUT"), listAsTTL(hasOutput), collapse = " ;\n "))
                             },
                             if (length(hasPart) > 0) {
                               paste(";\n", paste(ont("HASPART"), listAsTTL(hasPart), collapse = " ;\n "))
                             }
                       )
                  )}
              )
  )}
#proc <- Process("modelFitting", processType="MODELFITTING")#, type = list("ContrastEstimation"))
#proc
#proc2 <- Process("modelParamEstimation", processType="MODELPARAMETERESTIMATION")
#proc$hasPart <- append(proc$hasInput, proc2)
#proc
