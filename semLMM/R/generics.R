testClass <- function (x, ...) {
  UseMethod("testClass", x)
}

testClass.merMod <- function(x, ...) {
  print("merMod, lmer (lme4) ")
}

testClass.lme <- function(x, ...) {
  print("lme (nlme) ")
  log4r::debug(lenv$logger, ">>>>>>>>. test")
}



exportModelToRDF <- function (x, ...) {
  UseMethod("exportModelToRDF", x)
}

getModel <- function (x, ...) {
  UseMethod("getModel", x)
}

getVariables <- function (x, ...) {
  UseMethod("getVariables", x)
}

getErrorTerm <- function (x, ...) {
  UseMethod("getErrorTerm", x)
}

getRandomTerms <- function (x, ...) {
  UseMethod("getRandomTerms", x)
}

getFixedEstimation <- function (x, ...) {
  UseMethod("getFixedEstimation", x)
}

getEmmeans <- function (x, ...) {
  UseMethod("getEmmeans", x)
}


getVariablesFromGrid <- function(mod) {

  print(paste(match.call()))
  log4r::debug(lenv$logger, ">>>>>>>>. test")


  # terms <- terms(m5_3Fixed)
  # all.vars(delete.response(terms))

  vars <- list()
  varGrid <- levels(emmeans::ref_grid(mod))
  for (i in 1:length(varGrid)) {
    levs <- varGrid[[i]]
    if (length(levs) > 1) {
      var <- CategoricalVariable(label = names(varGrid)[i],
                                 levels = levs,
                                 type="IndependentVariable")
    } else {
      var <- ContinuousVariable(label = names(varGrid)[i],
                                levels = levs, # as.character(c(levs, 1)) # add unit value?
                                type=list("IndependentVariable"))
    }
    vars <- append(vars, var)
  }
  vars
}

getDependentVariables <- function(mod) {
  depVar <- list()
  varLab <- as.character(formula(mod))[2]
  # alternatively - perhaps better:
  # varLab <- rownames(attr(terms(mod), "factors"))[apply(attr(terms(mod), "factors"), 1, sum) == 0]
  var <- getEntity("Variable", varLab)
  if (is.null(var)) {
    var <- Variable(label = varLab, type="DependentVariable")
  }
  depVar <- append(depVar$dependentVariable, var)
  depVar
}

# get fixed terms from model attributes
# for each term, add variables and their levels
getFixedTerms <- function(mod) {

  print(paste(match.call(), class(mod)))

  #vars <- getVariables(mod) # commented!

  fixedTerms <- list()

  if (attr(terms(mod), "intercept") == 1) {
    fixTer <- FixedModelTerm(label = "(Intercept)", order = as.integer(0))
    #rint("--- Creating term (Intercept) with parenthesis - new implementation!")
    fixedTerms <- append(fixedTerms, fixTer)
  }

  terms <- terms(mod)

  for (i in 1:length(attr(terms, "term.labels"))) {

    lab <- attr(terms(mod), "term.labels")[i]
    order <- attr(terms(mod), "order")[i]
    fixTer <- FixedModelTerm(label = lab, order = order)

    #change the code from the next part to the approach based on factors table, below - NOT TESTED:
    #fct <- attr(terms(mod), "factors")
    #varLabs <- rownames(fct)[fct[,i] == 1]


    # add variables and their levels (no interation of levels, just values for separate variable)
    varLabs <- unlist(strsplit(lab,":"))
    #interact <- 0 # a matrix with all combinations of levels for the term, to be used for Effects in the next paragraph
    for (v in varLabs) {
      var <- getEntity("Variable", v)
      fixTer$variable <- append(fixTer$variable, var)

      # add new column with levels (constructing a cartesian product of levels of variables)
      #levels_df <- as.data.frame(levels(ref_grid(mod))[v])
      #interact <- merge(interact, levels_df, all=TRUE)
    }
    #interact # inspect the matrix

    # # create Effect for all combinations of factor levels (irrespective whether they are present in the estimation or not)
    # if (is.null(dim(interact))) {
    #   print("No levels defined. This should be handled by processing for Continuous Variable")
    # } else {
    #   for (e in 1:dim(interact)[1]) { #iterate through rows (effects)
    #     effLevels <- list() # levels per effect
    #     effName <- ""
    #     for (l in 2:dim(interact)[2]) { #iterate through columns (levels of factors)
    #       levName <- as.character(interact[e,l])
    #       effName <- paste(effName, levName, sep=":")
    #       lev <- getEntity("VariableLevel", levName)
    #       if (is.null(lev)) {
    #         print(paste("PARSE ERROR. No VariableLevel found for: ", levName))
    #       } else {
    #         effLevels <- append(effLevels, levName)
    #       }
    #     }
    #     effName <- substr(effName, 2, nchar(effName)) # remove ugly beginning of the name
    #     effect <- Parameter(label=effName, levels=effLevels, type="UNKNOWN")
    #     #cat(effect$asTTL()) # inspect
    #     fixTer$effect <- append(fixTer$effect, effect)
    #   }
    #
    # }
    #
    fixedTerms = append(fixedTerms, fixTer)
  }

  #getFixedEstimation(mod)

  fixedTerms
}

getDataset <- function(lmm, ds) { #TODO extract variables from the dataset, not the model

  print(paste(match.call()))

  dtset <- Dataset(label = ifelse(!is.null(ds[["label"]]), ds[["label"]], "Dataset"),
          url = ifelse(!is.null(ds[["url"]]), ds[["url"]], "url unavailable"),
          comments = {if (!is.null(ds[["comments"]])) ds[["comments"]] else list()},
          variables = append(lmm$variables, lmm$dependentVariable))
  dtset
}
