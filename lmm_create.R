source("lmm_classes.R")
source("example.R")
require(lme4)
require(lmerTest)
require(emmeans)

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
    interact <- 0 # a matrix with all combinations of levels for the term
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
      for (e in 1:dim(interact)[1]) {
        effLevels <- list() # per effect
        effName <- ""
        for (l in 2:dim(interact)[2]) {
          levName <- as.character(interact[e,l])
          lev <- getEntity("VariableLevel",levName)
          effName <- paste(effName, levName, sep=":")
          if (is.null(lev)) {
            print(paste("PARSE ERROR. No VariableLevel found for: ", levName))
          } else {
            effLevels <- append(effLevels, levName)
          }
        }
        effName <- substr(effName, 2, nchar(effName))
        effect <- Effect(label=effName, levels=effLevels, describesValueOf="")
        #cat(effect$asTTL()) # inspect
        fixTer$effect <- append(fixTer$effect, effect)
      }
      
    }
    
    fixedTerms = append(fixedTerms, fixTer)
  }
  
  getEffects2(mod)
  
  fixedTerms
}

# add effects
getEffects <- function(mod) {
  
  fixefs <- fixef(mod, add.dropped=TRUE) # get estimated effects
  feLabs <- names(fixefs)
  
  lab <- attr(terms(mod), "term.labels")
  varLabs <- unique(unlist(strsplit(lab,":"))) # get variable names
  
  for (varLab in varLabs) {
    feLabs <- sub(feLabs, pattern = varLab, rep="") # remove variable names from effect labels
  }
  feLabs
  
  for (i in 1:length(feLabs)) {
    eff <- getEntity("Effect", feLabs[i]) # find effects for labels
    if (is.null(eff)) {
      print(paste("PARSE ERROR. No Effect found for: ", feLabs[i]))
    } else {
      #print(eff$asTTL())
      est <- Estimate(feLabs[i], value = fixefs[i], parameter = eff)
      eff$estimate <- append(eff$estimate, est) # add estimate to the effect
    }
  }
}
# calculating multiple contrasts (other then relevel, allowing only one reference level) :
# http://mindingthebrain.blogspot.com/2013/04/multiple-pairwise-comparisons-for.html

# 2nd approach, this time from summary(m) rather then m itself
getEffects2 <- function(mod) {
  
  fixefs <- summary(mod)$coefficients # get estimated effects
  feLabs <- row.names(fixefs)
  
  lab <- attr(terms(mod), "term.labels")
  varLabs <- unique(unlist(strsplit(lab,":"))) # get variable names
  
  for (varLab in varLabs) {
    feLabs <- sub(feLabs, pattern = varLab, rep="") # remove variable names from effect labels
  }
  feLabs
  
  for (i in 1:length(feLabs)) {
    eff <- getEntity("Effect", feLabs[i]) # find effects for labels
    if (is.null(eff)) {
      print(paste("PARSE ERROR. No Effect found for: ", feLabs[i]))
    } else {
      #print(eff$asTTL())
      est <- Estimate(feLabs[i], value = fixefs[i, "Estimate"], parameter = eff)
      est$se <- fixefs[i, "Std. Error"]
      
      #TODO attach these values to sth that is printed
      hypo <- Hypothesis(label=feLabs[i], pvalue=fixefs[i, "Pr(>|t|)"], modelParams = list(eff))
      tstat <- Statistic(label="t-stat", type="T-student statistic", value=fixefs[i, "t value"])
      ttest <- StatisticalTest(label="t-test", df=fixefs[i, "df"], pvalue=fixefs[i, "Pr(>|t|)"], testStatistic = list(tstat))
      ttest$hypothesis <- list(hypo)
      #fixefs[i, "df"]
      #fixefs[i, "t value"]
      #fixefs[i, "Pr(>|t|)"]
      
      eff$estimate <- append(eff$estimate, est) # add estimate to the effect
    }
  }
}

getRandomTerms <- function(mod) {
  
  randomTerms <- list()
  
  for (r in names(ranef(mod))) {
    
    randTerm <- RandomModelTerm(label = r)
    
    #variable labels
    varLabs <- unlist(strsplit(r,":"))
    #effects
    ranef <- ranef(mod)[[r]]
    #effectLevs
    effLevLab <- row.names(ranef)
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
  
  paramName <- "sigma2_e"
  covStr <- CovarianceStructure(label="identCovStr_e", params=paramName)
  param <- getEntity("ModelParameter", paramName)
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

m <- m0_basic
mod <- m
#summary(m)$vcov # TODO

lmm <- Lmm(label="m0", formula = formula(m))
lmm$dependentVariable = getDependentVariables(m)
lmm$independentFixedTerm <- getFixedTerms(m)
lmm$independentRandomTerm <- getRandomTerms(m)
lmm$errorTerm <- getErrorTerm(m)

# print model:
#lmm$show()
cat(lmm$asTTL())

#TODO replace all ":" in names with <something else > before generating the triples (GraphDB has problems with ids)

capture.output(cat(prefixes), lmm, file = "tmp.ttl")
#sink("tmp.ttl")
#cat(prefixes)
#cat("\n\n")
#lmm
#
#sink()
