source("lmm_classes.R")
source("example.R")
require(lme4)
require(lmerTest)
require(emmeans)
require(assertthat)

# get fixed terms from model attributes
# for each term, add variables and their levels
getFixedTerms <- function(mod) {
  
  fixedTerms <- list()
  
  termLabels <- attr(terms(mod), "term.labels") #attr(attr(mod@frame, "terms"), "predvars.fixed"
  termOrders <- attr(terms(mod), "order")
  
  for (i in 1:length(termLabels)) {
    
    lab <- termLabels[i]
    order <- termOrders[i]
    fixTer <- FixedModelTerm(label = lab, order = order)
    
    # add variables and their levels (no interation of levels, just values for separate variable)
    varLabs <- unlist(strsplit(lab,":"))
    interact <- 0 # a matrix with all combinations of levels for the term, to be used for Effects in the next paragraph
    for (v in varLabs) {
      levels <- levels(ref_grid(mod))[[v]]
      var <- CategoricalVariable(label = v, levels = levels)
      fixTer$variable <- append(fixTer$variable, var)
      
      # add new column with levels (constructing a cartesian product of levels of variables)
      levels_df <- as.data.frame(levels)
      names(levels_df) <- v
      interact <- merge(interact, levels_df, all=TRUE)
    }
    interact # inspect the matrix
    
    # create Effect for all combinations of factor levels (irrespective whether they are present in the estimation or not)
    if (is.null(dim(interact))) {
      print("No levels defined. This should be handled by processing for Continuous Variable")
    } else {
      for (e in 1:dim(interact)[1]) { #iterate through rows (effects)
        effLevels <- list() # levels per effect
        effName <- ""
        for (l in 2:dim(interact)[2]) { #iterate through columns (levels of factors)
          levName <- as.character(interact[e,l])
          effName <- paste(effName, levName, sep=":")
          lev <- getEntity("VariableLevel",levName)
          if (is.null(lev)) {
            print(paste("PARSE ERROR. No VariableLevel found for: ", levName))
          } else {
            effLevels <- append(effLevels, levName)
          }
        }
        effName <- substr(effName, 2, nchar(effName)) # remove ugly beginning of the name 
        effect <- Effect(label=effName, levels=effLevels, describesValueOf="")
        #cat(effect$asTTL()) # inspect
        fixTer$effect <- append(fixTer$effect, effect)
      }
      
    }
    
    fixedTerms = append(fixedTerms, fixTer)
  }
  
  #getFixedEstimation(mod)
  
  fixedTerms
}

# calculating multiple contrasts (other then relevel, allowing only one reference level) :
# http://mindingthebrain.blogspot.com/2013/04/multiple-pairwise-comparisons-for.html

# get fixed effects 
# this time from summary(m) rather then m itself
getFixedEstimation <- function(mod) {
  
  procEst <- Process("paramEstimation", processType="MODELPARAMETERESTIMATION")
  procDfAppr <- Process("dfApproximation", processType="DFAPPROXIMATION")
  procTest <- Process("testing", processType="TESTING")
  
  fixefs <- summary(mod)$coefficients # get estimated effects
  feLabs <- row.names(fixefs)
  varLabs <- unique(unlist(strsplit(attr(terms(mod), "term.labels"),":"))) # get variable names
  
  for (varLab in varLabs) {
    feLabs <- sub(feLabs, pattern = varLab, rep="") # remove variable names from effect labels
  }
  feLabs
  
  for (i in 1:length(feLabs)) {
    eff <- getEntity("Effect", feLabs[i]) # find effects by labels
    if (is.null(eff)) {
      if (feLabs[i] == "(Intercept)") { #TODO handle intercepts and relative effects as contrasts
        print("=========================== TODO Primitive handling of (Intercept)")
        tmp_ref_grid <- as.data.frame(ref_grid(mod))
        refName <- tmp_ref_grid[1,1]
        ref <- getEntity("Level", refName)
        eff <- Effect(feLabs[i], levels = list(ref), describesValueOf = "")
        termName <- names(tmp_ref_grid)[1]
        term <- getEntity("Term", termName)
        term$effect <- append(term$effect, eff)
      } else {
        print(paste("PARSE ERROR. No Effect found for: ", feLabs[i]))
      }
    } else {
    
      est <- Estimate(feLabs[i], value = fixefs[i, "Estimate"], parameter = eff)
      est$se <- fixefs[i, "Std. Error"]
      eff$estimate <- append(eff$estimate, est) # add estimate to the effect
      
      procEst$hasOutput <- append(procEst$hasOutput, est)
      
      #TODO attach these values to sth that is printed
      hypo <- Hypothesis(label=feLabs[i], pvalue=fixefs[i, "Pr(>|t|)"], modelParams = list(eff))
      tstat <- Statistic(label="t-stat", type="T-student statistic", value=fixefs[i, "t value"])
      df <- Statistic(label="df", type="df statistic?", value=fixefs[i, "df"])
      pval <- Statistic(label="pvalue", type="pvalue", value=fixefs[i, "Pr(>|t|)"])
      
      procDfAppr$hasOutput <- append(procDfAppr$hasOutput, df)
      #procDfAppr$processType <- append(procDfAppr$processType, "Sattherwaite approximation") #TODO change 'processType' to list and uncomment
      
      procTest$hasInput <- append(procTest$hasInput, list(df, hypo, tstat))
      procTest$hasOutput <- append(procTest$hasOutput, list(pval))
    }
  }
  
  list(procEst, procDfAppr, procTest)
}

getVariables <- function(mod) {
  
  variables <- list()

  for (i in 1:length(mod@frame)) {
    varName <- names(mod@frame[i])
    varLevels <- levels(mod@frame[[i]])
    variable <- NULL
    if (is.null(varLevels)) {
      variable <- Variable(varName)
    } else {
      variable <- CategoricalVariable(varName, levels = varLevels)
    }
    #print(variable$asTTL())
    variables <- append(variables, variable)
  }
  
  variables
}

getRandomTerms2 <- function(mod) {
  
  variables <- getVariables(mod)
  
  randomTerms <- list()
  for (i in 1:length(mod@flist)) { # iterate over terms
    
    termName <- names(mod@flist[i]) # get variable names
    termVarNames <- unlist(strsplit(termName,":"))
    termVariables <- listOfStringsToObjects("Variable", termVarNames)
    assert_that(length(termVarNames) == length(termVariables))
    randomTerm <- RandomModelTerm(termName, variable = termVariables)
    #cat(randomTerm$asTTL())
    
    randomTerm$covarianceStructure <- list(CovarianceStructure(termName))
    
    effects <- list()
    termEffectNames <- levels(mod@flist[[i]]) # get effects and assign levels to them
    for (termEffectName in termEffectNames) {
      termEffectLevelNames <- unlist(strsplit(termEffectName,":"))
      effLevels <- listOfStringsToObjects("Level", termEffectLevelNames)
      assert_that(length(effLevels) == length(termEffectLevelNames))
      eff <- Effect(termEffectName, levels = effLevels, describesValueOf = "")
      effects <- append(effects, eff)
    }
    effects
    randomTerm$effect <- effects
      
    randomTerms <- append(randomTerms, randomTerm)
  }
  
  randomTerms
}

getRandomPrediction <- function(mod) {
  
  procPred <- Process("paramPrediction", processType="MODELPARAMETERESTIMATION")
  
  estimates <- list()
  ranefs <- ranef(mod)
  for (ranef in ranefs) {
    effNames <- row.names(ranef)
    for (effName in effNames) {
      eff <- getEntity("Effect", effName)
      if (is.null(eff)) {
        print("No effect for name:", effName)
      } else {
        val <- ranef[effName, 1] # (Intercept)
        est <- Estimate(effName, value = val, parameter = eff)
        eff$estimate <- append(eff$estimate, est)
        
        estimates <- append(estimates, est)
      }
    }
  }
  procPred$hasOutput <- estimates
  
  list(procPred)
}

getVarCorrEstimation <- function(mod) {
  
  procEst <- Process("varCompEstimation", processType="MODELPARAMETERESTIMATION")
  
  estimates <- list()
  
  vc <- VarCorr(mod)
  vcFrame <- as.data.frame(vc)
  
  for (i in 1:dim(vcFrame)[1]) {
    termName <- vcFrame[i, "grp"]
    term <- getEntity("RandomModelTerm", termName)
    if (is.null(term)) {
      if (termName == "Residual") {
        #TODO handle residual #FIXME
      } else {
        print(paste("No term for name:", termName))
      }
    } else {
      param <- term$covarianceStructure[[1]]$params[[1]] #TODO select param to fill
      val <- vcFrame[i, "vcov"]
      est <- Estimate(termName, value = val, parameter = param)
      #param$estimate <- append(param$estimate, est)
      
      estimates <- append(estimates, est)
    }
  }
  
  procEst$hasOutput <- estimates
  
  list(procEst)
}

getRandomTerms <- function(mod) {
  
  randomTerms <- list()
  
  for (r in names(ranef(mod))) {
    
    randTerm <- RandomModelTerm(label = r)
    
    varLabs <- unlist(strsplit(r,":")) #variable labels
    ranef <- ranef(mod)[[r]] #effects
    effLevLab <- row.names(ranef) #effectLevs
    effLevs <- strsplit(effLevLab, ":")
    
    # get levels and create variables
    vars <- list()
    for (i in 1:length(varLabs)){
      tmp <- character()
      lapply(effLevs, function(x) tmp<<-c(tmp,x[i]))
      levs <- unique(tmp)
      var <- CategoricalVariable(varLabs[i], levels=levs)
      vars <- append(vars, var)
    }
    randTerm$variable <- vars
    
    # get effects
    effects <- list()
    estimates <- list()
    for (i in 1:length(effLevLab)) {
      effLab <- effLevLab[i]
      levs <- effLevs[[i]]
      ef <- Effect(effLab, levels=levs, describesValueOf = "???") #TODO dependent variable (or other things in multidimensional model)
      effects <- append(effects, ef)
      value <- ranef[i,]
      est <- Estimate(label = effLab, value = value, parameter = ef)
      estimates <- append(estimates, est)
    }
    randTerm$effect <- effects;
    randTerm$estimate <- estimates;
    
    # get covariance structure for term
    paramName <- "sigma"
    covStr <- CovarianceStructure(label=r, params=paramName)
    param <- getEntity("ModelParameter", paramName)
    value <- summary(mod)$varcor[[r]][1]
    est <- Estimate(label = paramName, value = value, parameter = param)
    covStr$estimate <- list(est)
    
    randTerm$covarianceStructure <- list(covStr)
      
    randomTerms <- append(randomTerms, randTerm)
  }
  randomTerms
}
# a three-dimensional array with symmetric faces; each face contains the variance-covariance matrix for a particular level of the grouping factor
#r_condVar <- ranef(m1_basic, condVar=TRUE)
#attr(r_condVar[[1]], "postVar")


# get error (residual)
getErrorTerm <- function(mod) {
  
  errorTerm <- ErrorModelTerm(label = "e") #TODO change to ErrorTerm / ErrorModelTerm / ???
  
  effects <- list()
  estimates <- list()
  
  ef <- Effect("e", levels=NULL, describesValueOf = "???") #TODO dependent variable (or other things in multidimensional model) #TODO remove for error??
  effects <- append(effects, ef)
  
  errorTerm$effect <- effects;
  errorTerm$estimate <- estimates;
  
  paramName <- "identCovStr_e_sigma2_e"
  covStr <- CovarianceStructure(label="identCovStr_e", params=paramName)
  param <- getEntity("ModelParameter", paramName)
  assert_that(!is.null(param))
  value <- getME(mod, "devcomp")[["cmp"]]["sigmaREML"] #TODO other deviance components and fitness? e.g. sigmaML
  est <- Estimate(label = paramName, value = value^2, parameter = param) # unsure - covariance structure has a param (sigma_e), 
                                                                            # the param has estimated value (value) + ... s.e? 
                                                                            # in model the value of the param is given as "Residual Std.Dev."
  covStr$estimate <- list(est)
  errorTerm$covarianceStructure <- list(covStr)
  errorTerm$estimate <- list(est)
  
  list(errorTerm)
}

getDependentVariables <- function(m) {
  depVar <- list()
  var <- Variable(label = as.character(formula(m))[2])
  depVar <- append(depVar$dependentVariable, var)
  depVar
}


############################ run ################################


modelName <- deparse(quote(m6_basic)) # m0_basic
mod <- get(modelName)
#summary(m)$vcov # TODO

modelFitting <- Process("modelFitting0", processType="MODELFITTING")
#modelFitting$hasPart <- append(modelFitting$hasPart, c(
                               #Process("varCompEstimation", processType="MODELPARAMETERESTIMATION"),
                               #Process("paramPrediction", processType="MODELPARAMETERESTIMATION")
                               #Process("paramEstimation", processType="MODELPARAMETERESTIMATION"),
                               #Process("dfApproximation", processType="DFAPPROXIMATION"),
                               #Process("testing", processType="TESTING")
#))

lmm <- Lmm(label=modelName, formula = formula(mod))
lmm$dependentVariable = getDependentVariables(mod)
lmm$independentFixedTerm <- getFixedTerms(mod)
lmm$independentRandomTerm <- getRandomTerms2(mod) #getRandomTerms(mod)
lmm$errorTerm <- getErrorTerm(mod)

modelFitting$hasInput <- append(modelFitting$hasInput, lmm)
procs <- getFixedEstimation(mod)
modelFitting$hasPart <- append(modelFitting$hasPart, procs)
procs <- getVarCorrEstimation(mod)
modelFitting$hasPart <- append(modelFitting$hasPart, procs)
procs <- getRandomPrediction(mod)
modelFitting$hasPart <- append(modelFitting$hasPart, procs)

# print model:
#lmm$show()
cat(lmm$asTTL())
cat(modelFitting$asTTL())
modelFitting

capture.output(cat(prefixes), modelFitting, file = paste0("out", .Platform$file.sep, lmm$label, ".ttl"))
