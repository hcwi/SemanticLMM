require(lme4)
require(lmerTest)
require(pbkrtest)
require(emmeans)
require(assertthat)
source("lmm_classes.R")
source("example.R")
#contr.treat <- contr.treatment

getVariables <- function(mod) {
  
  # terms <- terms(m5_3Fixed)
  # all.vars(delete.response(terms))
  
  vars <- list()
  varGrid <- levels(ref_grid(mod))
  for (i in 1:length(varGrid)) {
    var <- CategoricalVariable(label = names(varGrid)[i], 
                               levels = varGrid[[i]], 
                               type="IndependentVariable")
    vars <- append(vars, var)
  }
  vars
}


# get fixed terms from model attributes
# for each term, add variables and their levels
getFixedTerms <- function(mod) {
  
  vars <- getVariables(mod)
  
  fixedTerms <- list()
  
  if (attr(terms(mod), "intercept") == 1) {
    fixTer <- FixedModelTerm(label = "Intercept", order = integer(0))
    fixedTerms <- append(fixedTerms, fixTer)
  }
  
  for (i in 1:length(terms(mod))) {
    
    lab <- attr(terms(mod), "term.labels")[i]
    order <- attr(terms(mod), "order")[i]
    fixTer <- FixedModelTerm(label = lab, order = order)
    
    # add variables and their levels (no interation of levels, just values for separate variable)
    varLabs <- unlist(strsplit(lab,":"))
    interact <- 0 # a matrix with all combinations of levels for the term, to be used for Effects in the next paragraph
    for (v in varLabs) {
      var <- getEntity("Variable", v)
      fixTer$variable <- append(fixTer$variable, var)
      
      # add new column with levels (constructing a cartesian product of levels of variables)
      levels_df <- as.data.frame(levels(ref_grid(mod))[v])
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
        effect <- Effect(label=effName, levels=effLevels, type="UNKNOWN")
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
getFixedEstimation <- function(mod, lmm) {
  
  #declare processes
  procEst <- Process("paramEstimation", processType="ModelParameterEstimation")
  procDfAppr <- Process("dfCalculation", processType="DfCalculation", type="SatterthwaiteApprox")
  procTest <- Process("testing", processType="testing")
  procTest$hasPart <- append(procTest$hasPart, procDfAppr)
  
  #get fixed coefs to produce "effects"
  fixcoefs <- summary(mod)$coefficients # get estimated effects
  feLabs <- row.names(fixcoefs)
  varLabs <- unique(unlist(strsplit(attr(terms(mod), "term.labels"),":"))) # get variable names
  for (varLab in varLabs) {
    feLabs <- sub(feLabs, pattern = varLab, rep="") # remove variable names from effect labels
  }
  
  modmatrix <- model.matrix(mod)
  termAssign <- attr(modmatrix, "assign")
  ref <- min(termAssign) # ass == ref -> EMM else Contrast 
  contrs <- attr(modmatrix, "contrasts")
  ok <- all(sapply(contrs, function(x) {typeof(x) == "character" && startsWith(x, "contr.treat")} ))
  assert_that(ok, msg = "Znaleziono inne kontrasty niz 'contr.treatment'. Ich wartosci nie zostana przetworzone.") # TODO zrobic ifa, jesli ok==TRUE
  if (ok) {
    print("Contrast ok. Processing standard contr.treatment:")
  }
  
  for (i in 1:length(feLabs)) {
    
    term <- lmm$independentFixedTerm[[termAssign[i] + ifelse(ref == 0, 1, 0)]] # term no. must be >= 1,  
    
    effType <- "UNKNOWN"
    lvls <- list()
    refEff <- list() 
    
    # effects
    if (termAssign[i] == ref) {
      effType = "emm" #TODO pass reference levels
      if (termAssign[i] == 0) {
        for (v in varLabs) {
          var <- getEntity("Variable", v)
          lvls <- append(lvls, var$levels[[1]]) # adding default 1st level
        }
      } else {
        for (v in term$variable) {
          lvls <- append(lvls, v$levels[[i]])
        }
        explicitVarLabs <- sapply(term$variable, function(x) x$label)
        missingDefaultVarLabs <- setdiff(varLabs, explicitVarLabs)
        for (v in missingDefaultVarLabs) {
          var <- getEntity("Variable", v)
          lvls <- append(lvls, var$levels[[1]]) # adding default 1st level
        }
      }
    } else {
      effType = "contrast"
      for (l in unlist(strsplit(feLabs[i], ":"))) {
        lvl <- getEntity("Level", l)
        lvls <- append(lvls, lvl)
      }
      if (sum(termAssign == ref) == 1 || # there is only one reference / intercept
          sum(termAssign == termAssign[i]) == 1) { # or just one param per term
        refEff <- append(refEff, lmm$independentFixedTerm[[1]]$effect[[1]])
      } 
      else {
        r <- i - sum(termAssign < termAssign[i]) # the number of param in sequence (within a term)
        refEff <- append(refEff, lmm$independentFixedTerm[[1]]$effect[[r + 1]]) # a corresponding effect from the first term
        # +1, bacause the contr.treatment matrix cuts off the first column (effect)
      }
    }
    effect <- Effect(label=feLabs[i], levels=lvls, type = effType, reference = refEff)
    term$effect <- append(term$effect, effect)
    
    # estimates
    est <- Estimate(feLabs[i], value = fixcoefs[i, "Estimate"], parameter = effect)
    est$se <- fixcoefs[i, "Std. Error"]
    effect$estimate <- append(effect$estimate, est) # add estimate to the effect
    procEst$hasOutput <- append(procEst$hasOutput, est)
    
    # testing 
    lab <- feLabs[i]
    hypo <- Hypothesis(label=lab, pvalue=fixcoefs[i, "Pr(>|t|)"], modelParams = list(effect))
    tstat <- Statistic(label=paste0("t-stat_", lab), type="Tstatistic", value=fixcoefs[i, "t value"])
    df <- Statistic(label=paste0("df_", lab), type="df", value=fixcoefs[i, "df"])
    pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=fixcoefs[i, "Pr(>|t|)"], isAbout=list(hypo))
    
    pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df)
    
    procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr)
    
    pTest <- Process(paste0("testing_", lab), processType="testing")
    pTest$hasInput <- append(pTest$hasInput, list(df, hypo, tstat))
    pTest$hasOutput <- append(pTest$hasOutput, list(pval))
    
    procTest$hasPart <- append(procTest$hasPart, pTest)
  }
  
  # testing for effects - general hypotheses
  ano <- anova(mod)
  effLabs <- row.names(ano)
  for (lab in effLabs) {
    term <- getEntity("Term", lab)
    assert_that(!is.null(term), msg = "No term for name from anova(mod) has been found")
    
    hypo <- Hypothesis(label=lab, pvalue=ano[lab, "Pr(>F)"], modelParams = list(term))
    fstat <- Statistic(label=paste0("f-stat_", lab), type="Fstatistic", value=ano[lab, "F value"])
    df_num <- Statistic(label=paste0("df_num_", lab), type=list("df", "df_num"), value=ano[lab, "NumDF"])
    df_den <- Statistic(label=paste0("df_den_", lab), type=list("df", "df_den"), value=ano[lab, "DenDF"]) 
    pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=ano[lab, "Pr(>F)"], isAbout=list(hypo))
    
    pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
    pTest <- Process(paste0("testing_", lab), processType="testing")
    
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_num)
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_den) #TODO separate process for DenDF approx than for numDF?
    pTest$hasInput <- append(pTest$hasInput, list(df_num, df_den, hypo, fstat))
    pTest$hasOutput <- append(pTest$hasOutput, list(pval))
    procTest$hasPart <- append(procTest$hasPart, pTest)
    procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr) 
  }
  
  list(procEst, procTest)
}

getEmmeans <- function(mod) {
  
  procEmms <- Process("EmmCalculation", processType="ModelParameterEstimation")
  procDfAppr <- Process("EmmDfCalculation", processType="DfCalculation", type="KenwardRogerApprox")
  procConfIntCalc <- Process("confIntCalculation", processType="ConfidenceIntervalCalculation")
  procConfIntCalc$hasPart <- append(procConfIntCalc$hasPart, procDfAppr)
  
  varLabs <- unique(unlist(strsplit(attr(terms(mod), "term.labels"),":"))) # get variable names
  for (i in 1:length(varLabs)) {
    com <- combn(varLabs, i)
    for (j in 1:(dim(com)[2])) {
      
      eq <- paste0("~", paste(com[,j], collapse = "*"))
      emms <- emmeans(mod, formula(eq))
      emmsLab <- paste(com[,j], collapse = ".")
      ems <- data.frame(emms)
      
      msg <- attr(summary(emms), "mesg")
      
      df_method <- sub(patt="Degrees-of-freedom method: ", x=grep("Degrees-of-freedom method: ", msg, v=T) , rep="")
      df_method <- paste0(sub(pat="-", x=df_method, ""), "Approx")
      df <- Statistic(label=paste0("df_emm_", emmsLab), type="df", value=ems[1, "df"]) #TODO czego dotyczy: effect czy est? # conf interval!
      pDfAppr <- Process(paste0("dfCalculation_", df_method, "_", emmsLab), processType="DfCalculation", type=df_method)
      pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df)
      procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr)
      
      conf_lev <- as.numeric(sub(patt="Confidence level used: ", x=grep("Confidence level used: ", msg, v=T), rep=""))
      confLevel <- Statistic(label=paste0("confLevel_", emmsLab), type="confidenceLevel", value=conf_lev)
      pConfIntCalc <- Process(paste0("confIntCalculation_", emmsLab), processType="ConfidenceIntervalCalculation")
      pConfIntCalc$hasInput <- append(pConfIntCalc$hasInput, c(confLevel, df))
      procConfIntCalc$hasPart <- append(procConfIntCalc$hasPart, pConfIntCalc)
        
      for (e in 1:dim(ems)[1]) {
        lvls <- list()
        #effLab <- "emm"
        for (l in 1:i) {
          levLab <- as.character(ems[e,l])
          #effLab <- paste0(effLab, ".", levLab) #CHECK why was it concatenated with 'emm'? some other effect conflicts with it?
          effLab <- levLab
          lev <- getEntity("Level", levLab)
          lvls <- append(lvls, lev)
        }
        effect <- Effect(paste0("emm_", effLab), levels = lvls, type = "emm")
        
        est <- Estimate(paste0('emm_', effLab), value = ems[e, "emmean"], parameter = effect)
        est$se <- ems[e, "SE"]
        effect$estimate <- append(effect$estimate, est)
        procEmms$hasOutput <- append(procEmms$hasOutput, est)
        
        confInt <- Statistic(label=paste0("confInt_", effLab), type="confidenceInterval", isAbout = list(effect))
        confLevel <- Statistic(label=paste0("confLevel_", effLab), type="confidenceLevel", value=conf_lev, isAbout=list(confInt))
        lowerCL <- Statistic(label=paste0("lcl_", effLab), type="lowerConfidenceLimit", value=ems[e, "lower.CL"])
        upperCL <- Statistic(label=paste0("ucl_", effLab), type="upperConfidenceLimit", value=ems[e, "upper.CL"])
        confInt$hasPart <- list(confLevel, lowerCL, upperCL)
        pConfIntCalc$hasOutput <- append(pConfIntCalc$hasOutput, confInt)
      }
      
    }
  }
  #TODO add overall mean:
  #emmeans(mod, ~1)
  
  list(procEmms, procConfIntCalc)
}

getVariables <- function(mod) {
  
  variables <- list()

  for (i in 1:length(mod@frame)) {
    varName <- names(mod@frame[i])
    varLevels <- levels(mod@frame[[i]])
    variable <- getEntity("Variable", varName)
    if (is.null(variable)) {
      if (is.null(varLevels)) {
        variable <- Variable(varName)
      } else {
        variable <- CategoricalVariable(varName, levels = varLevels)
      }
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
    sapply(termVariables, function(x) { x$type <- append(x$type,"IndependentVariable") })
    
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
      eff <- Effect(termEffectName, levels = effLevels)
      effects <- append(effects, eff)
    }
    effects
    randomTerm$effect <- effects
      
    randomTerms <- append(randomTerms, randomTerm)
  }
  
  randomTerms
}

getRandomPrediction <- function(mod) {
  
  procPred <- Process("paramPrediction", processType="ModelParameterEstimation")
  
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
  
  procEst <- Process("varCompEstimation", processType="ModelParameterEstimation")
  
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

# a three-dimensional array with symmetric faces; each face contains the variance-covariance matrix for a particular level of the grouping factor
#r_condVar <- ranef(m1_basic, condVar=TRUE)
#attr(r_condVar[[1]], "postVar")


# get error (residual)
getErrorTerm <- function(mod) {
  
  errorTerm <- ErrorModelTerm(label = "e") #TODO change to ErrorTerm / ErrorModelTerm / ???
  
  effects <- list()
  estimates <- list()
  
  ef <- Effect("e", levels=NULL) #TODO dependent variable (or other things in multidimensional model) #TODO remove for error??
  effects <- append(effects, ef)
  
  errorTerm$effect <- effects;
  errorTerm$estimate <- estimates;
  
  paramName <- "identCovStr_e_sigma2_e"
  covStr <- CovarianceStructure(label="identCovStr_e", params=paramName)
  param <- getEntity("ModelParameter", paramName)
  assert_that(!is.null(param))
  #TODO check sigma(mod)
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
  var <- Variable(label = as.character(formula(m))[2], type="DependentVariable")
  depVar <- append(depVar$dependentVariable, var)
  depVar
}

getModel <- function(mod) {
  
  lmm <- Lmm(label=format(Sys.time(), "%Y%m%d%H%M%S"), formula = formula(mod))
  lmm$criterionREML <- getME(mod, "devcomp")[["cmp"]]["REML"]
  if (is.na(lmm$criterionREML)) {
    lmm$criterionAICdf <- extractAIC(mod)[1]
    lmm$criterionAIC <- extractAIC(mod)[2]
  }
  lmm
}

############################ run ################################


exportModelToRDF <- function(mod) {
  
  lmm <- getModel(mod)
  lmm$dependentVariable = getDependentVariables(mod)
  lmm$independentFixedTerm <- getFixedTerms(mod)
  lmm$independentRandomTerm <- getRandomTerms2(mod) #getRandomTerms(mod)
  lmm$errorTerm <- getErrorTerm(mod)
  
  modelFitting <- Process("modelFitting0", processType="ModelFitting")
  modelFitting$hasInput <- append(modelFitting$hasInput, lmm)
  procs <- getFixedEstimation(mod, lmm)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- getVarCorrEstimation(mod)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- getRandomPrediction(mod)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- getEmmeans(mod)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  
  
  # print model:
  #lmm$show()
  #cat(lmm$asTTL())
  #cat(modelFitting$asTTL())
  modelFitting

}

tmp <- list(m0) #m_all

for (model in tmp) {
  #modelName <- deparse(quote(model))
  #mod <- get(modelName)
  modelFitting <- exportModelToRDF(model)
  graphName <- modelFitting$hasInput[[1]]$label
  capture.output(cat(prefixes), 
                 cat("<graphs/graph_", graphName, ">", sep=""), 
                 cat(" {\n"),
                 modelFitting, 
                 cat("}"),
                 file = paste0("out", .Platform$file.sep, graphName, ".trig"))
}

#summary(m)$vcov # TODO
