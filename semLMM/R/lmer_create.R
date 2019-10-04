getVariables.merMod <- function(mod) {

  log4r::debug(lenv$logger, paste(match.call(), class(mod)))
  vars <- getVariablesFromGrid(mod) # fixed vars from ref_grid
  vars
}


# calculating multiple contrasts (other then relevel, allowing only one reference level) :
# http://mindingthebrain.blogspot.com/2013/04/multiple-pairwise-comparisons-for.html

# get fixed effects
# this time from summary(m) rather then m itself
getFixedEstimation.merMod <- function(mod, lmm) {

  #declare processes
  procEst <- Process("paramEstimation", processType="ModelParameterEstimation", type="BLUE")
  procDfAppr <- Process("dfCalculation", processType="DfCalculation", type="SatterthwaiteApprox")
  procTest <- Process("testing", processType="testing")
  procTest$hasPart <- append(procTest$hasPart, procDfAppr)

  #get fixed coefs to produce "effects"
  fixcoefs <- summary(mod)$coefficients # get estimated effects
  feLabs <- row.names(fixcoefs)
  reLabs <- feLabs
  vars <- unique(unlist(strsplit(attr(terms(mod), "term.labels"),":"))) # get the set of variable names
  if (attr(terms(mod), "intercept")) vars <- c("(Intercept)", vars)

  # get variable levels per term
  feLabsSplit <- strsplit(feLabs,":")
  for (var in vars) {
    feLabsSplit <- lapply(feLabsSplit, function(x) sub(x, pattern = var, rep="", fixed = T))
  }
  # get variables per term
  reLabsSplit <- strsplit(reLabs, ":", perl = T)
  for (i in 1:length(reLabsSplit)) {
    for(j in 1:length(reLabsSplit[[i]])) {
      reLabsSplit[[i]][j] <- sub(reLabsSplit[[i]][j], pattern = feLabsSplit[[i]][j], replacement = "", fixed = F)
    }
  }

  modmatrix <- model.matrix(mod)
  termAssign <- attr(modmatrix, "assign")
  ref <- min(termAssign) # ass == ref -> EMM else Contrast
  contrs <- attr(modmatrix, "contrasts")
  ok <- all(sapply(contrs, function(x) {typeof(x) == "character" && startsWith(x, "contr.treat")} ))
  assertthat::assert_that(ok, msg = "ERROR. Found other contrasts than 'contr.treatment' which cannot be handled.")

  for (i in 1:length(feLabsSplit)) {

    term <- lmm$independentFixedTerm[[termAssign[i] + ifelse(ref == 0, 1, 0)]] # term no. must be >= 1,

    effType <- "UNKNOWN"
    lvls <- list()
    refEff <- list()

    # effects
    if (termAssign[i] == ref) { # check if a general reference level
      effType = c("fixedEffect", "directEffect") #c("emm", "effect")
      if (termAssign[i] == 0) { # for a general intercept add base levels/values of all variables
        for (v in vars) {
          var <- getEntity("Variable", v)
          if (is(var, "CategoricalVariable")) {
            lvls <- append(lvls, var$levels[[1]]) # adding default 1st level
          } else if (is(var, "ContinuousVariable")) {
            val <- ValueSpecification(label=paste0(var$label, "=0"), value=0, variable=var)
            var$levels <- append(var$levels, val)
            lvls <- append(lvls, val)
          }
        }
      } else { #TODO CHECK & test # not considering use cases with interaction terms only (i.e. without single covariate to refer to!)
        isRegression <- FALSE
        for (var in term$variable) {
          if (is(var, "CategoricalVariable")) {
            lvls <- append(lvls, var$levels[[i]])
          } else {
            isRegression <- TRUE
            levLab <- paste0(var$label,"=1")
            lvl <- getEntity("ValueSpecification", levLab, var$label)
            if (is.null(lvl)) {
              lvl <- ValueSpecification(levLab, value=1, variable=var)
              var$levels <- append(var$levels, lvl)
            }
            lvls <- append(lvls, lvl)
          }
        }
        if (!isRegression) {
          explicitVarLabs <- sapply(term$variable, function(x) x$label)
          missingDefaultVarLabs <- setdiff(vars, explicitVarLabs)
          for (v in missingDefaultVarLabs) {
            var <- getEntity("Variable", v)
            if (is(var, "CategoricalVariable")) {
              lvls <- append(lvls, var$levels[[1]]) # adding default 1st level
            } else {
              levLab <- paste0(var$label,"=1")
              lvl <- getEntity("ValueSpecification", levLab, var$label)
              if (is.null(lvl)) {
                lvl <- ValueSpecification(levLab, value=1, variable=var)
                var$levels <- append(var$levels, lvl)
              }
              lvls <- append(lvls,lvl)
            }
          }
        }
      }
    } else {
      effType = c("FixedEffect", "treatmentContrast", "relativeEffect")
      #here can add estType="contrastEstimate")

      for (j in 1:length(feLabsSplit[[i]])){
        l = feLabsSplit[[i]][j]
        if (l != "") {
          lvl <- getEntity("Level", l, reLabsSplit[[i]][j])
        } else {
          v <- reLabsSplit[[i]][j]
          var <- getEntity("Variable", v)
          levLab <- paste0(var$label,"=1")
          lvl <- getEntity("ValueSpecification", levLab, v)
          if (is.null(lvl)) {
            lvl <- ValueSpecification(levLab, value=1, variable=var)
            var$levels <- append(var$levels, lvl)
          }
        }
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

    lab <- ifelse(feLabs[i] == "", reLabs[i], feLabs[i])
    effect <- Parameter(label=lab, levels=lvls, type = as.list(effType), reference = refEff)
    term$effect <- append(term$effect, effect)

    # estimates
    est <- Estimate(lab, value = fixcoefs[i, "Estimate"], parameter = effect, type="Estimate")
    est$se <- fixcoefs[i, "Std. Error"]
    effect$estimate <- append(effect$estimate, est) # add estimate to the effect
    procEst$hasOutput <- append(procEst$hasOutput, est)

    # testing
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
    assertthat::assert_that(!is.null(term), msg = "No term for name from anova(mod) has been found")

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

getEmmeans.merMod <- function(mod) {

  log4r::debug(lenv$logger, paste(match.call(), class(mod)))

  procEmms <- Process("EmmCalculation", processType="ModelParameterEstimation")
  procDfAppr <- Process("EmmDfCalculation", processType="DfCalculation", type="KenwardRogerApprox")
  procConfIntCalc <- Process("confIntCalculation", processType="ConfidenceIntervalCalculation")
  procConfIntCalc$hasPart <- append(procConfIntCalc$hasPart, procDfAppr)

  var <- strsplit(attr(terms(mod), "term.labels"),":")
  varLabs <- unique(unlist(strsplit(attr(terms(mod), "term.labels"),":"))) # get variable names
  for (i in 1:length(varLabs)) {
    com <- combn(varLabs, i)
    for (j in 1:(dim(com)[2])) {

      eq <- paste0("~", paste(com[,j], collapse = "*"))
      emms <- emmeans::emmeans(mod, formula(eq))
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
        effLab <- ""
        for (l in 1:i) {
          levLab <- as.character(ems[e,l])
          effLab <- paste0(effLab, ".", levLab)
          lev <- getEntity("Level", levLab, names(ems)[l])
          if (is.null(lev)) {
            var <- getEntity("Variable", names(ems)[l])
            refLab <- paste0(var$label,"_",levLab)
            lev <- getEntity("ValueSpecification", refLab, names(ems)[l])
            if (is.null(lev)) {
              lev <- ValueSpecification(refLab, value = as.numeric(levLab), variable=var)
              var$levels <- append(var$levels, lev)
            }
          }
          lvls <- append(lvls, lev)
        }
        effect <- Parameter(paste0("emm_", effLab), levels = lvls, type = "emm")

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


getRandomTerms.merMod <- function(mod) {

  log4r::debug(lenv$logger, paste(match.call(), class(mod)))

  randomTerms <- list()
  for (i in 1:length(mod@flist)) { # iterate over terms

    termName <- names(mod@flist[i]) # get variable names
    termVarNames <- unlist(strsplit(termName,":"))
    termVariables <- list()
    for (tvn in termVarNames) {
        tvar <- getEntity("Variable", tvn)
        if (is.null(tvar)) {
          tvar <- CategoricalVariable(tvn, type=list("IndependentVariable"))
        }
        termVariables <- append(termVariables, tvar)
      }

    assertthat::assert_that(length(termVarNames) == length(termVariables))
    randomTerm <- RandomModelTerm(termName, variable = termVariables)

    randomTerm$covarianceStructure <- list(CovarianceStructure(termName))

    effects <- list()
    termEffectNames <- strsplit(levels(mod@flist[[i]]),":") # get effects and assign levels to them
    for (termEffectLevelNames in termEffectNames) {
      effLevels <- list()
      for (j in 1:length(termEffectLevelNames)) {
        tvn <- termEffectLevelNames[j]
        tvar <- getEntity(className = "Level", label = tvn, relatedClassLabel = termVarNames[j])
        if (is.null(tvar)) {
          #print(paste(tvn, "No variable level, adding"))
          var <- termVariables[[j]]
          tvar <- VariableLevel(tvn, value=tvn, variable=var)
          var$levels <- append(var$levels, tvar)
        }
        effLevels <- append(effLevels, tvar)
      }
      assertthat::assert_that(length(effLevels) == length(termEffectLevelNames))
      eff <- Parameter(paste0(termEffectLevelNames, collapse =":"), levels = effLevels, type=list("Effect")) #ModelParameter X, then what? #TODO eBLUP?
      effects <- append(effects, eff)
    }
    effects
    randomTerm$effect <- effects

    randomTerms <- append(randomTerms, randomTerm)
  }

  randomTerms
}

# probably out of use, not working for lmer
# getRandomTerms2 <- function(mod) {
#
#   variables <- getVariables(mod)
#
#   randomTerms <- list()
#   for (i in 1:length(mod@flist)) { # iterate over terms
#
#     termName <- names(mod@flist[i]) # get variable names
#     termVarNames <- unlist(strsplit(termName,":"))
#     termVariables <- listOfStringsToObjects("Variable", termVarNames)
#     sapply(termVariables, function(x) { x$type <- append(x$type,"IndependentVariable") })
#
#     assertthat::assert_that(length(termVarNames) == length(termVariables))
#     randomTerm <- RandomModelTerm(termName, variable = termVariables)
#     #cat(randomTerm$asTTL())
#
#     randomTerm$covarianceStructure <- list(CovarianceStructure(termName))
#
#     effects <- list()
#     termEffectNames <- levels(mod@flist[[i]]) # get effects and assign levels to them
#     for (termEffectName in termEffectNames) {
#       termEffectLevelNames <- unlist(strsplit(termEffectName,":"))
#       effLevels <- listOfStringsToObjects("Level", termEffectLevelNames)
#       assertthat::assert_that(length(effLevels) == length(termEffectLevelNames))
#       eff <- Parameter(termEffectName, levels = effLevels, type=list("Effect")) #ModelParameter X, then what? #TODO eBLUP?
#       effects <- append(effects, eff)
#     }
#     effects
#     randomTerm$effect <- effects
#
#     randomTerms <- append(randomTerms, randomTerm)
#   }
#
#   randomTerms
# }

getRandomPrediction.merMod <- function(mod) {

  procPred <- Process("paramPrediction", processType="ModelParameterEstimation", type="BLUP")

  estimates <- list()
  ranefs <- ranef(mod)
  for (ranef in ranefs) {
    effNames <- row.names(ranef)
    for (effName in effNames) {
      eff <- getEntity("Parameter", effName)
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

getVarCorrEstimation.merMod <- function(mod) {

  procEst <- Process("varCompEstimation", processType="ModelParameterEstimation", type="REML")

  estimates <- list()

  vc <- VarCorr(mod)
  vcFrame <- as.data.frame(vc)

  for (i in 1:dim(vcFrame)[1]) {
    termName <- vcFrame[i, "grp"]
    term <- getEntity("RandomModelTerm", termName)
    if (is.null(term)) {
      if (termName == "Residual") {
        #TODO handle residual #FIXME
        term <- getEntity("ErrorModelTerm", termName)

      } else {
        print(paste("No term for name:", termName))
        break;
      }
    }
    param <- term$covarianceStructure[[1]]$params[[1]] #TODO select param to fill
    val <- vcFrame[i, "vcov"]
    est <- Estimate(termName, value = val, parameter = param, type="Estimate")
    param$estimate <- list(est)
    #param$estimate <- append(param$estimate, est)

    estimates <- append(estimates, est)
  }

  procEst$hasOutput <- estimates

  list(procEst)
}

# a three-dimensional array with symmetric faces; each face contains the variance-covariance matrix for a particular level of the grouping factor
#r_condVar <- ranef(m1_basic, condVar=TRUE)
#attr(r_condVar[[1]], "postVar")


# get error (residual)
getErrorTerm.merMod <- function(mod) {

  log4r::debug(lenv$logger, paste(match.call(), class(mod)))

  errorTerm <- ErrorModelTerm(label = "Residual")

  # adding individual effects for the error term
  # for the time being let's not add individual effects for the error term, as they are not interesting for the audience
  #effects <- list()
  #stimates <- list()
  #ef <- Parameter("e", levels=NULL, type="ModelParameter") #TODO dependent variable (or other things in multidimensional model)
  #effects <- append(effects, ef)
  #errorTerm$effect <- effects;
  #errorTerm$estimate <- estimates;

  paramName <- "Resid_sigma2e"
  covStr <- CovarianceStructure(label="ResidualCovStr", params=paramName, type="covIdentity")
  param <- getEntity("Parameter", paramName)
  assertthat::assert_that(!is.null(param))
  # alternatives:
  #1: check sigma(mod)
  #2: value <- getME(mod, "devcomp")[["cmp"]]["sigmaREML"] #TODO other deviance components and fitness? e.g. sigmaML
  #est <- Estimate(label = paramName, value = value^2, parameter = param, type="Estimate") # unsure - covariance structure has a param (sigma_e),
                                                                            # the param has estimated value (value) + ... s.e?
                                                                            # in model the value of the param is given as "Residual Std.Dev."
  #covStr$estimate <- list(est)
  errorTerm$covarianceStructure <- list(covStr)
  #errorTerm$estimate <- list(est) #CHECK why the estimates were assigned to the terms, let's comment it for now and see what comes out

  list(errorTerm)
}


getModel.merMod <- function(mod) {

  log4r::debug(lenv$logger, paste(match.call(), class(mod)))

  vars <- getVariablesFromGrid(mod)
  form <- paste(trimws(deparse(formula(mod)), which="left"), collapse = "")
  lab <- gsub(form, pattern = "~", rep="-")
  lab <- gsub(lab, pattern = " ", rep="")
  lab <- gsub(lab, pattern = "*", rep="", fixed=T)
  lab <- gsub(lab, pattern = "+", rep=".", fixed=T)
  lab <- substr(lab, 1, grepRaw(lab, pat="(", fixed=T)-2)
  lmm <- Lmm(label=paste0("model_", lab), formula = form, vars = vars)

  lmm$criterionREML <- getME(mod, "devcomp")[["cmp"]]["REML"]
  lmm$criterionAIC <- AIC(mod) #summary(mod)$AIC
  lmm$criterionBIC <- BIC(mod) #summar
  #if (is.na(lmm$criterionREML)) {
  #  lmm$criterionAICdf <- extractAIC(mod)[1]
  #  lmm$criterionAIC <- extractAIC(mod)[2]
  #}
  lmm
}

############################ run ################################


exportModelToRDF.merMod <- function(mod, ds=list()) {

  log4r::debug(lenv$logger, paste(match.call(), class(mod)))

  init()
  lmm <- getModel(mod)
  lmm$dependentVariable = getDependentVariables(mod)
  lmm$independentFixedTerm <- getFixedTerms(mod)
  lmm$independentRandomTerm <- getRandomTerms(mod)
  lapply(lmm$independentRandomTerm, function(x) {
    lmm$variables <- append(lmm$variables, x$variable)}) #hopefully tmp,
  # #adding random variables to overall model variables

  lmm$errorTerm <- getErrorTerm(mod)

  modelFitting <- Process("modelFitting", processType="ModelFitting", comments=list(paste0("rdfs:comment \"Results obtained by R lme4 package, lmer function\"")))
  modelFitting$hasInput <- append(modelFitting$hasInput, lmm)
  modelFitting$hasInput <- append(modelFitting$hasInput, getDataset(lmm, ds))
  modelFitting$hasOutput <- lmm$getQuality()

  procs <- getFixedEstimation(mod, lmm)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- getVarCorrEstimation.merMod(mod)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- getRandomPrediction.merMod(mod)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- getEmmeans(mod)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)

  modelFitting
}


#####
# run <- function() {
#
#   tmp <-  models[1:1]
#
#   for (mod in tmp) {
#     #mod <- m0 #m_cov_noint #m_cov_noint #m0  #m_cov
#     #modelName <- deparse(quote(model))
#     #mod <- get(modelName)
#     init()
#     print(formula(mod))
#     modelFitting <- exportModelToRDF(mod)
#     graphName <- modelFitting$hasInput[[1]]$id
#     print(graphName)
#     capture.output(cat(prefixes),
#                    cat("<graphs/graph_", graphName, ">", sep=""),
#                    cat(" {\n"),
#                    modelFitting,
#                    cat("}"),
#                    file = paste0("out", .Platform$file.sep, graphName, ".trig"))
#   }
# }

