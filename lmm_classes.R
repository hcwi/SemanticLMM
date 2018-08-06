########################## aux functions ########################## 

init <- function() {
  
  source("term_mappings_reader.R")
  
  id <<- 0
  
  getID <<- function(o) {
    l <- o$label
    #if (is.null(l) || l == "") {
    #  id <<- id + 1
    #  l <- id
    #}
    id <- format(Sys.time(), "%y%m%d%H%M%S")
    c <- as.character(class(o))
    paste(c, l, id, sep="_")
  }
  
  reg <<- list()
  
  register <<- function(o) {
    o$id <- getID(o)
    reg[o$id] <<- list(o)
  }
  
  getEntity <<- function(className, label) {
    for (r in reg) {
      classOk <- grepl(class(r), patt = className, ignore.case = T)
      if (classOk & r$label == label)
        return(r)
    }
    return(NULL)
  }
  
  listEntities <<- function() {
    for (r in reg) {
      cat(sprintf("%20s %20s\n", class(r), r$label))
    }
  }
  #getEntity("OntologyEntity", "eoLabel")
  
  listOfStringsToObjects <<- function(objClass = "Level", objNames) {
    
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
        if (length(objNames) == 0 || is(objNames[[1]], "OntologyEntity")) {
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
  
}
init()

#reg #list registered objects


########################## LMM model classes ########################## 

OntologyEntity <- {setRefClass("OntologyEntity",
                              fields = list(
                                id = "character",
                                label = "character",
                                type = "list"
                              ),
                              methods = list(
                                initialize = function(label, types = list()) {
                                  .self$label <- label
                                  if (is.character(types)) {
                                    types <- as.list(types)
                                  }
                                  .self$type <- types
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
                                        assert_that(is(obj, "OntologyEntity"), msg = paste("No reg object for key: ", key))
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
                                mapping = function(type) {
                                  paste0(get(toupper(type)))
                                },
                                asTTL = function() {
                                  paste(getTTL(), ".\n")
                                },
                                getTTL = function() {
                                  types <- ""
                                  for (t in type) {
                                    types <- paste(types, ";\n", TYPE, mapping(t))
                                  }  
                                  paste(ident(id), LABEL, lit(label),
                                        types
                                        )
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
# oe <- OntologyEntity(label = "eoLabel", type=list("ModelParameter", "Effect"))
# oe <- OntologyEntity(label = "eoLabel", type="ModelParameter")
# oe
#str(oe)


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
                                     #";\n", PVALUE, num(pvalue),
                                     #";\n", QVALUE, num(qvalue),
                                     ";\n", ISABOUT, listAsTTL(modelParams)
                               )
                             }
                           )
)}

Statistic <- {setRefClass("Statistic", 
                          contains = "OntologyEntity",
                          fields = list(
                            value = "numeric",
                            isAbout = "list",
                            hasPart = "list"
                          ),
                          methods = list(
                            initialize = function(... , value=numeric(0), isAbout = list(), hasPart = list()) {
                              callSuper(...)
                              .self$value <- value
                              .self$isAbout <- isAbout
                              .self$hasPart <- hasPart
                            },
                            getTTL = function() {
                              paste(callSuper(),
                                    ";\n", TYPE, STATISTIC,
                                    if (is.number(value)) {
                                      paste(";\n", VALUE, num(value)) },
                                    if (length(isAbout) > 0) {
                                      paste(";\n", paste(ISABOUT, listAsTTL(isAbout), collapse = " ;\n ")) },
                                    if (length(hasPart) > 0) {
                                      paste(";\n", paste(HASPART, listAsTTL(hasPart), collapse = " ;\n ")) }
                              )}
                          )
)}

#hypo1 <- Hypothesis(label="hypo0", pvalue=0.05, modelParams = list(param1))
#tstat <- Statistic(label="t-stat", type=list("Tstatistic"), value=18.46)
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
                            if(length(se) == 1 && se != "") {
                              listAsTTL(list(
                              Statistic(paste0("se_", label), type=list("se"), value=se, isAbout = list(.self))
                              ))
                            }
                            paste(callSuper(),
                                  ";\n", TYPE, "xxx:Estimate", #ESTIMATE,
                                  ";\n", VALUE, num(value),
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
                         methods = list(
                           initialize = function(...) {
                             callSuper(...)
                           },
                           getTTL = function() {
                             paste(callSuper(),
                                   ";\n", TYPE, VARIABLE#,
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
                        if (length(variable) > 0) {
                          paste(";\n", paste(ISABOUT, listAsTTL(variable), collapse = " ;\n "))
                        },
                        if (length(effect) > 0) {
                          paste(";\n", paste(HASEFFECT, listAsTTL(effect), collapse = " ;\n "))
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
                params = "list", # of ModelParams
                estimate = "list" # of Estimates of Params
              ),
              methods = list(
                initialize = function(..., type = list("covIdentity"), params = list("sigma2e")) {
                  callSuper(...)
                  #TODO correct choosing cov model
                  tmp <- list()
                  for (i in params) {
                    tmp <- append(tmp, Parameter(label=i, type=list("ModelParameter", "varianceComponent")))
                  }
                  .self$params <- tmp
                },
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, COVARIANCESTRUCTURE,
                        ";\n", paste(HASPART, listAsTTL(params), collapse = " ;\n ")
                        )
                }
              )
  )
}
# cs <- CovarianceStructure(label = "blockTerm", params="sigma")
#cs

#param1 <- ModelParameter(label = "param1", type = list("variance"))
# param1


Parameter <- {
  setRefClass("Parameter",
              contains = "OntologyEntity",
              fields= list(
                correspondingVarLevels = "list", #of Levels (for now)
                relativeTo = "list", #list of reference effects (only ONE? reference effect)
                specifiesValueOf = "ANY", #dependent variable
                estimate = "list"
              ),
              methods = list(
                initialize = function(..., levels=list(), reference = list(), valueOf = list()) {
                  callSuper(...)
                  tmp <- listOfStringsToObjects("VariableLevel", levels)
                  .self$correspondingVarLevels <- tmp
                  .self$relativeTo <- reference
                  .self$specifiesValueOf <- valueOf
                },
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, "xxx:Parameter",
                        if (length(correspondingVarLevels) > 0) {
                          paste(";\n", paste(ISABOUT, listAsTTL(correspondingVarLevels), collapse = " ;\n "))
                        },
                        if (length(relativeTo) > 0) {
                          paste(";\n", paste("xxx:TMP_REFERENCE", listAsTTL(relativeTo), collapse = " ;\n "))
                        },
                        if (length(estimate) > 0) {
                          paste(";\n", paste("xxx:TMP_EST", listAsTTL(estimate), collapse = " ;\n "))
                        },
                        if (length(specifiesValueOf) > 0) {
                          paste(";\n", paste(SPECIFIESVALUEOF, listAsTTL(specifiesValueOf), collapse = " ;\n "))
                        }
                      )
                }
              )
  )}

cv <- CategoricalVariable("testVar", levels = list("CamB1", "Drought"))
ef1 <- Parameter("testEffect", levels=list("CamB1","Drought"), type="EMM") # Interaction Effect
cv2 <- CategoricalVariable("testVar2", levels = list("CamB2", "Drought2"))
ef2 <- Parameter("testRelativeEffect", levels=list("CamB2","Drought2"), reference = list(ef1), type=c("RelativeEffect", "ModelParameter")) # Interaction Effect
cat(ef1$asTTL())
cat(ef2$asTTL())

# ParametricFunction <- {
#   setRefClass("ParametricFunction",
#               contains = "OntologyEntity",
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
#                 getTTL = function() {
#                   paste(callSuper(),
#                         ";\n", TYPE, PARAMETRICFUNCTION,
#                         #";\n", TYPE, EFFECT,
#                         #";\n", TYPE, MODELPARAMETER,
#                         ";\n", FORMULA, lit(formula),
#                         if (length(parameters) > 0) {
#                           paste(";\n", paste(ISFUNCTIONOF, listAsTTL(parameters), collapse = " ;\n "))
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
#                 getTTL = function() {
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
              contains = "OntologyEntity",
              fields = list(
                vars = "list"
              ),
              methods = list(
                initialize = function(..., declares) {
                  callSuper(...)
                  .self$vars <- declares
                },
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, STUDYDESIGN,
                        ";\n", paste(DECLARES, listAsTTL(vars), collapse = " ;\n ")
                  )
                }
              )
  )}

DesignMatrix <- {
  setRefClass("DesignMatrix",
              contains = "OntologyEntity",
              fields = list(
                studyDesign = "list"
              ),
              methods = list(
                initialize = function(..., declares) {
                  callSuper(...)
                  .self$studyDesign <- list(StudyDesign("sd", declares = declares))
                },
                getTTL = function() {
                  paste(callSuper(),
                        ";\n", TYPE, DESIGNMATRIX,
                        ";\n", paste(DENOTES, listAsTTL(studyDesign), collapse = " ;\n ")
                  )
                }
              )
  )}


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
                criterionAIC = "numeric",
                criterionAICdf = "numeric",
                variables = "list",
                designMatrix = "list"
              ),
              methods = list(
                initialize = function(..., formula, vars = list()) {
                  callSuper(...)
                  .self$formula <- formula
                  #.self$residual <- resid
                  .self$variables <- vars
                },
                getTTL = function() {
                  .self$designMatrix <- list(DesignMatrix("dm", declares = variables))
                  paste(callSuper(),
                        ";\n", TYPE, LMM,
                        ";\n", FORMULA, lit(deparse(formula)),
                        ";\n", ifelse(!is.na(criterionREML), 
                                      paste(CRITREML, criterionREML),
                                      paste(CRITAIC, criterionAIC, ";\n", CRITAICDF, criterionAICdf)),
                        ";\n", ISMODELFOR, listAsTTL(dependentVariable),
                        ";\n", paste(HASTERM, listAsTTL(independentFixedTerm), collapse = " ;\n "),
                        ";\n", paste(HASTERM, listAsTTL(independentRandomTerm), collapse = " ;\n "),
                        ";\n", paste(HASTERM, listAsTTL(errorTerm), collapse = " ;\n "),
                        ";\n", paste(HASTERM, listAsTTL(designMatrix), collapse = " ;\n ")
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
#proc <- Process("modelFitting", processType="MODELFITTING")#, type = list("ContrastEstimation"))
#proc
#proc2 <- Process("modelParamEstimation", processType="MODELPARAMETERESTIMATION")
#proc$hasPart <- append(proc$hasInput, proc2)
#proc
