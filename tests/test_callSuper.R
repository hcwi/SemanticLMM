VariableX <- {setRefClass("VariableX",
                                    fields = list(
                                      id = "character",
                                      label = "character" # of VariableLevel
                                    ),
                                    methods = list(
                                      initialize = function(label) {
                                        .self$id <- paste("id", runif(1), sep="_")
                                        .self$label <- label
                                      },
                                      getTTL = function() {
                                        paste(id, TYPE, VARIABLE,
                                              ";\n", LABEL, label
                                        )
                                      },
                                      asTTL = function() {
                                        paste(getTTL(), ".\n")
                                      }
                                    )
)}

TestX <- {setRefClass("TestX",
                      fields = list(
                        testField = "character"
                      ),
                      methods = list(
                        getTTL = function() {
                          paste(";\n", TYPE, "TestX")
                        }
                      ))}

CategoricalVariableX <- {setRefClass("CategoricalVariableX",
                                    contains = c("VariableX", "TestX"),
                                    fields = list(
                                      levels = "list" # of VariableLevel
                                    ),
                                    methods = list(
                                      initialize = function(... , levels=character()) {
                                        callSuper(...)
                                        .self$levels <- list()
                                        for (l in levels){
                                          .self$levels <- append(.self$levels, l)
                                        }
                                      },
                                      getTTL = function() {
                                        paste(callSuper(),
                                              ";\n", TYPE, CATEGORICALVARIABLE,
                                              ";\n", LABEL, label,
                                              ";\n", paste(HASLEVEL, levels, collapse=" ;\n "),
                                              .self$export("TestX")$getTTL()
                                        )
                                      }
                                    )
)}

cv <- CategoricalVariableX(label = "AnyVariable", levels=c("1", "2", "b"))
cat(cv$asTTL())

x <- VariableX(label = "generalClass")
cx <- CategoricalVariableX(x)
