run <- function() {
  m_lmer <- example1_lmer()
  m_lme <- example1_lme()

  mod <- m_lme
  print(formula(mod))
  modelFitting <- exportModelToRDF_2(mod)
  saveTriples(modelFitting)

}
#run()


exportModelToRDF_2 <- function(mod, ds=list()) {

  init()
  lmm <- getModel_2(mod) # need to add random variables to lmm$variables
  lmm$dependentVariable <- getDependentVariables(mod)
  lmm$independentFixedTerm <- getFixedTerms(mod)
  lmm$errorTerm <- getErrorTerm_2(mod) #?
  random_all <- getRandomTerms_2(mod)
  lmm$independentRandomTerm <- random_all[[1]]

  modelFitting <- Process("modelFitting", processType="ModelFitting", comments=list(paste0("rdfs:comment \"Results obtained by R nlme package, lme function\"")))
  modelFitting$hasInput <- append(modelFitting$hasInput, lmm)
  modelFitting$hasInput <- append(modelFitting$hasInput, Dataset(label = ifelse(!is.null(ds[["label"]]), ds[["label"]], "Dataset"),
                                                                 url = ifelse(!is.null(ds[["url"]]), ds[["url"]], "url unavailable"),
                                                                 comments = {if (!is.null(ds[["comments"]])) ds[["comments"]] else list()},
                                                                 variables = append(lmm$variables, lmm$dependentVariable)))
  modelFitting$hasOutput <- lmm$getQuality()

  procs <- getFixedEstimation_2(mod, lmm) #? result manually guided: list(procEst, procTest)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- random_all[[2]] #getVarCorrEstimation(mod)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- random_all[[3]] #getRandomPrediction(mod)
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)
  procs <- getEmmeans_2(mod) #?
  modelFitting$hasPart <- append(modelFitting$hasPart, procs)

  modelFitting

}

saveTriples <- function(modelFitting, graphName = NULL) {
  if(is.null(graphName)) {
    graphName <- modelFitting$hasInput[[1]]$id
  }
  capture.output(cat(prefixes),
                 cat("<graphs/graph_", graphName, ">", sep=""),
                 cat(" {\n"),
                 modelFitting,
                 cat("}"),
                 file = paste0("out", .Platform$file.sep, graphName, ".trig"))
  print(paste0("Exported to ", graphName, ".trig"))
}


getModel_2 <- function(mod) {

  vars <- getVariables_2(mod)
  lab <- gsub(deparse(formula(mod)),pattern = " ", rep="")
  lab <- gsub(lab, pattern = "~", rep="-")
  lab <- gsub(lab, pattern = "*", rep="", fixed=T)
  lab <- gsub(lab, pattern = "+", rep=".", fixed=T)
  if (length(grepRaw(lab, pat="(", fixed=T))>0) { #!
    lab <- substr(lab, 1, grepRaw(lab, pat="(", fixed=T)-2)
  }
  lmm <- Lmm(label=paste0("model_", lab),
             formula = formula(mod), vars = vars)

  #TODO: czy potrzebne REML criterion? nie ma! czy logLik dodac wszedzie? czy nic?
  {#if(mod$method == "REML") {
  #  lmm$criterionREML <- mod$
  #}
  #else if (mod$method == "ML") {???}
  #lmm$criterionAIC <- extractAIC(mod)[1]
  }
  #TODO df
  lmm$criterionAIC <- AIC(mod) #summary(mod)$AIC
  lmm$criterionBIC <- BIC(mod) #summar
  #lmm$criterionLogLik <- mod$logLik #summar
  lmm
}

getVariables_2 <- function(mod) {

  vars <- getVariables(mod) # fixed vars from ref_grid

  rVarLabs <- names(mod$groups)
  for (rvl in rVarLabs) {
    if (!any(lapply(vars, function(x) {x$label == rvl}))) {
      var <- getEntity("Variable", rvl)
      if (is.null(var)) {
        rvLevels <- levels(mod$data[[rvl]])
        if (is.null(rvLevels)) {
          var <- ContinuousVariable(label = rvl, type="IndependentVariable")
        } else {
          var <- CategoricalVariable(label = rvl,
                                     levels = rvLevels,
                                     type=list("IndependentVariable", "BlockingVariable"))
        }
      }
      vars <- append(vars, var)
    }
  }

  # can use mod$groups[[i]] to add random effects (combinations) !

  vars
}



getRandomTerms_2 <- function(mod) {

  randomTerms <- list() # dla nlme bedzie tylko jeden, moze byc zagniezdzony -> wszystkie reStruct do tego samego randomTermu
  procEst <- Process("varCompEstimation", processType="ModelParameterEstimation", type="REML")
  procPred <- Process("paramPrediction", processType="ModelParameterEstimation", type="BLUP")

  upperLeverOfNesting <- list()
  for (i in length(mod$modelStruct$reStruct):1) { # iterate over terms (lets assume that the first element of reStruct is standalone term
                                                  # and the following elements are nested (interaction terms with the first one))
                                                  #backward iteration because they are numbered from the most nested to the top one
    reStr <- mod$modelStruct$reStruct[i]
    termName <- names(reStr) # get variable names
    termVarNames <- unlist(strsplit(termName,":")) #probably not the case for nlme
    termVarNames <- upperLeverOfNesting <- append(upperLeverOfNesting, termVarNames)
    fullTermName <- paste(termVarNames, collapse = ":")

    #get newest, term specific variables
    termSpecVars <- list()
    {
      for (tvn in unlist(strsplit(termName,":"))) {
      var <- getEntity("Variable", tvn)
      if (is.null(var)) {
        var <- CategoricalVariable(label = tvn, type="IndependentVariable")
      }
      termSpecVars <- append(termSpecVars, var)
      }
    }

    # get all variables
    termVariables <- list() #listOfStringsToObjects("Variable", termVarNames)
    {
      for (tvn in termVarNames) {
        var <- getEntity("Variable", tvn)
        if (is.null(var)) {
          var <- CategoricalVariable(label = tvn, type="IndependentVariable")
        }
        termVariables <- append(termVariables, var)
      }
      assertthat::assert_that(length(termVarNames) == length(termVariables))
    }

    randomTerm <- RandomModelTerm(fullTermName, variable = termVariables)
    #cat(randomTerm$asTTL())

    #getting covariance structure, its parameteres and their BLUEs
    {
      #selecting ids (start:end) of rows of BLUEs for particular term
      start <- grep(row.names(VarCorr(mod)), patt=paste0(termName, " ="), perl = T) + 1
      if (length(start) == 0) {
        start <- 1
      }
      all <- grep(row.names(VarCorr(mod)), patt="( =)|(Residual)", perl = T)
      end <- min(all[all > start])

      pars <- list()
      estimates <- list()
      covMod <- class(reStr[[1]])[1]
      if (covMod == "pdLogChol" || covMod == "pdIdent") {
        covMod <- "pdIdent"
        var <- as.numeric(VarCorr(mod)[start,"Variance"])
        #TODO: name of factor (for nested Block:Variance) or individual levels?
        name <- paste0("sigma2_", fullTermName) #row.names(VarCorr(mod))[start])

        param <- Parameter(name, type=list("Variance", "VarianceParameter")) # variance of ??? corresponds to each level!
        est <- Estimate(name, value = var, parameter = param, type="Estimate")
        param$estimate <- list(est)
        estimates <- append(estimates, est)
        pars <- append(pars, param)

        #otherParams <- list()
        #levels <- termSpecVars[[1]]$levels
        #for (lev in levels) {
        #  p <- Parameter(lev$label, type=list("Variance"), levels=lev )
          #e <- Estimate(lev$label, value = var, parameter = param, type="Estimate")
        #  est
        # TODO: est as a value of variance
        #}




      } else if (covMod == "pdCompSymm") {
        var <- as.numeric(VarCorr(mod)[start,"Variance"])
        #TODO name of factor of level?
        name <- paste0("sigma2_", termName) #row.names(VarCorr(mod))[start])
        param <- Parameter(name, type=list("Variance", "VarianceParameter")) # variance of ??? corresponds to each level!
        est <- Estimate(name, value = var, parameter = param, type="Estimate")
        param$estimate <- list(est)
        estimates <- append(estimates, est)
        pars <- append(pars, param)

        cor <- VarCorr(mod)[start + 1,"Corr"]
        #TODO names of factor or levels?
        name <- paste0("corr_", termName)#row.names(VarCorr(mod))[start])
        param <- Parameter(name, type=list("Correlation", "VarianceParameter")) # of ??? corresponds to each level!
        est <- Estimate(name, value = var, parameter = param, type="Estimate")
        param$estimate <- list(est)
        estimates <- append(estimates, est)
        pars <- append(pars, param)
      } else if (covMod == "pdDiag") {
        #TODO implement pdDiag
        for (i in (start:(end-1))) {
          var <- VarCorr(mod)[start,"Variance"]

        }

        stop("Diag not implemented")

      }

      randomTerm$covarianceStructure <- list(CovarianceStructure(fullTermName, # replace with termName
                                                                   covModel = covMod, params=pars, vars = termSpecVars))
      procEst$hasOutput <- append(procEst$hasOutput, estimates)
    }


    #getting effects and their BLUPs
    {
      effects <- list()
      estimates <- list()

      coefs <- mod$coefficients$random[[termName]]
      {
        formul <- formula(mod$modelStruct$reStruct[[termName]])
        if (formul == "~1") {
          cols <- rep(1, dim(coefs)[1])
        } else if (formul == paste0("~", termName, " - 1")) {
          cols <- 1:dim(coefs)[1]
        } else {
          stop("Not supported conditioning of BLUPs")
        }
      } # check if random coef

      termEffectNames <- row.names(coefs)
      for (j in 1:length(termEffectNames)) {
        termEffectName <- termEffectNames[j]
        termEffectLevelNames <- unlist(strsplit(termEffectName,"/"))
        effLevels <- list()
        for (teln in termEffectLevelNames) {
          lvl <- getEntity("Level", teln)
          if (is.null(lvl)) {
            lvl <- VariableLevel(label=teln, variable = getEntity("Variable", termName))
          }
          effLevels <- append(effLevels, lvl)
        }
        assertthat::assert_that(length(effLevels) == length(termEffectLevelNames))
        eff <- Parameter(termEffectName, levels = effLevels, type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
        val <- coefs[termEffectName,cols[j]] # (Intercept)
        est <- Estimate(termEffectName, value = val, parameter = eff)
        eff$estimate <- append(eff$estimate, est)

        estimates <- append(estimates, est)
        effects <- append(effects, eff)
      }

      procPred$hasOutput <- append(procPred$hasOutput, estimates)

      randomTerm$effect <- effects

    }

    randomTerms <- append(randomTerms, randomTerm)

  }

  #getting error params and BLUEs
  {
    res <- grep(row.names(VarCorr(mod)), patt="Residual", perl = T)

    estimates <- list()

    var <- as.numeric(VarCorr(mod)[res,"Variance"])
    name <- paste0("sigma2_", "Residual") #row.names(VarCorr(mod))[start])

    param <- getEntity("Parameter", name) # variance of ??? corresponds to each level!
    est <- Estimate(name, value = var, parameter = param, type="Estimate")
    param$estimate <- list(est)
    estimates <- append(estimates, est)

    procEst$hasOutput <- append(procEst$hasOutput, estimates)
  }

  # return list(procPred)
  # return list(procEst)
  list(randomTerms, list(procEst), list(procPred))
}
#rt <- getRandomTerms_2(adv5_nested)


getErrorTerm_2 <- function(mod = NULL) {

  label <- "Residual"
  errorTerm <- ErrorModelTerm(label = label)

  paramName <- paste0("sigma2_", label)
  param <- Parameter(label=paramName, type=list("VarianceParameter")) # no variable in the effect for 1 dep variable in the model
  covStr <- CovarianceStructure(label="ResidualCovStr", params=list(param), type="pdIdent")
    param <- getEntity("Parameter", paramName)
    assertthat::assert_that(!is.null(param))
  errorTerm$covarianceStructure <- list(covStr)
  #errorTerm$estimate <- list(est) #CHECK why the estimates were assigned to the terms, let's comment it for now and see what comes out

  list(errorTerm)
}

# get fixed effects
# this time from summary(m) rather then m itself
getFixedEstimation_2 <- function(mod, lmm) {

  #declare processes
  procEst <- Process("paramEstimation", processType="ModelParameterEstimation", type="BLUE")
  procDfAppr <- Process("dfCalculation", processType="DfCalculation", type="SatterthwaiteApprox")
  procTest <- Process("testing", processType="testing")
  procTest$hasPart <- append(procTest$hasPart, procDfAppr)

  #get fixed coefs to produce "effects"
  fixcoefs <- summary(mod)$tTable #mod$coefficients$fixed # get estimated effects
  feLabs <- row.names(fixcoefs)
  reLabs <- feLabs
  varLabs <- unique(unlist(strsplit(attr(terms(mod), "term.labels"),":"))) # get variable names
  for (varLab in varLabs) {
    feLabs <- sub(feLabs, pattern = varLab, rep="") # remove variable names from effect labels
  }

  # create 'termAssign' vector
  {
    termAssign <- rep(-1, length(feLabs))

    feLabsTerm <- row.names(fixcoefs)
    for (i in 1:length(feLabsTerm)) {
      feLabsTerm[i]
      levs <- unlist(strsplit(feLabs[i],":"))
      for (lev in levs) {
        feLabsTerm[i] <- sub(feLabsTerm[i], pattern = lev, rep="")
      }
      #print(paste(levs, "->", feLabsTerm[i]))
    }
    feLabsTerm

    tlabs <- attr(mod$terms, "term.labels")
    for (i in 1:length(termAssign)) {
      termId <- (1:length(tlabs))[feLabsTerm[i] == tlabs]
      if (length(termId) == 0) {
        termId <- 0
      }
      termAssign[i] <- termId
    }
  }
  termAssign


  #modmatrix <- model.matrix(mod)
  #termAssign <- attr(modmatrix, "assign")
  ref <- min(termAssign) # ass == ref -> EMM else Contrast
  #contrs <- attr(modmatrix, "contrasts")
  #ok <- all(sapply(contrs, function(x) {typeof(x) == "character" && startsWith(x, "contr.treat")} ))
  #assertthat::assert_that(ok, msg = "ERROR. Found other contrasts than 'contr.treatment' which cannot be handled.")

  for (i in 1:length(feLabs)) {

    term <- lmm$independentFixedTerm[[termAssign[i] + ifelse(ref == 0, 1, 0)]] # term no. must be >= 1,

    effType <- "UNKNOWN"
    lvls <- list()
    refEff <- list()

    # effects
    if (termAssign[i] == ref) {
      effType = c("fixedEffect", "directEffect") #, "emm")
      if (termAssign[i] == 0) { # for a general intercept add base levels/values of all variables
        for (v in varLabs) {
          var <- getEntity("Variable", v)
          if (is(var, "CategoricalVariable")) {
            lvls <- append(lvls, var$levels[[1]]) # adding default 1st level
          } else {
            # lvls <- append(lvls, Statistic("baseLevel", value=0, isAbout=list(var), type="ValueSpecification")
            val <- ValueSpecification(label=paste0(var$label, "=0"), value=0, variable=var)
            var$levels <- append(var$levels, val)
            lvls <- append(lvls, val) # !!!
            # baseLevel -> Mass_0
            # Statistic -> ValueSpecification
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
            lvl <- getEntity("ValueSpecification", levLab)
            if (is.null(lvl)) {
              lvl <- ValueSpecification(levLab, value=1, variable=var)#, type="ValueSpecification")
              var$levels <- append(var$levels, lvl)
            }
            lvls <- append(lvls, lvl)
          }
        }
        if (!isRegression) {
          explicitVarLabs <- sapply(term$variable, function(x) x$label)
          missingDefaultVarLabs <- setdiff(varLabs, explicitVarLabs)
          for (v in missingDefaultVarLabs) {
            var <- getEntity("Variable", v)
            if (is(var, "CategoricalVariable")) {
              lvls <- append(lvls, var$levels[[1]]) # adding default 1st level
            } else {
              levLab <- paste0(var$label,"=1")
              lvl <- getEntity("ValueSpecification", levLab)
              if (is.null(lvl)) {
                lvl <- ValueSpecification(levLab, value=1, variable=var)#Statistic(levLab, value=1, isAbout=list(var), type="ValueSpecification")
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

      if (feLabs[i] != "") {
        for (l in unlist(strsplit(feLabs[i], ":"))) {
          lvl <- getEntity("Level", l)
          lvls <- append(lvls, lvl)
        }
      } else {
        v <- reLabs[i]
        var <- getEntity("Variable", v)
        levLab <- paste0(var$label,"=1")
        lvl <- getEntity("ValueSpecification", levLab)
        if (is.null(lvl)) {
          lvl <- ValueSpecification(levLab, value=1, variable=var) #Statistic(levLab, value=1, isAbout=list(var), type="ValueSpecification")
          var$levels <- append(var$levels, lvl)
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
    est <- Estimate(lab, value = fixcoefs[i, "Value"], parameter = effect, type="Estimate") #!!!
    est$se <- fixcoefs[i, "Std.Error"] #!!!
    effect$estimate <- append(effect$estimate, est) # add estimate to the effect
    procEst$hasOutput <- append(procEst$hasOutput, est)

    # testing
    hypo <- Hypothesis(label=lab, pvalue=fixcoefs[i, "p-value"], modelParams = list(effect)) #!!!
    tstat <- Statistic(label=paste0("t-stat_", lab), type="Tstatistic", value=fixcoefs[i, "t-value"])
    df <- Statistic(label=paste0("df_", lab), type="df", value=fixcoefs[i, "DF"])
    pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=fixcoefs[i, "p-value"], isAbout=list(hypo))

    pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df)

    procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr)

    pTest <- Process(paste0("testingEffect_", lab), processType="testing")
    pTest$hasInput <- append(pTest$hasInput, list(df, hypo, tstat))
    pTest$hasOutput <- append(pTest$hasOutput, list(pval))

    procTest$hasPart <- append(procTest$hasPart, pTest)
  }

  # testing for effects - general hypotheses
  ano <- anova(mod)
  effLabs <- row.names(ano)
  for (lab in effLabs) { #effLabs[-1] # TODO: solve problem with lacking (Intercept) term!!!
    term <- getEntity("Term", lab)
    assertthat::assert_that(!is.null(term), msg = paste("No term for name", lab, " from anova(mod) has been found"))

    hypo <- Hypothesis(label=lab, pvalue=ano[lab, "p-value"], modelParams = list(term))
    fstat <- Statistic(label=paste0("f-stat_", lab), type="Fstatistic", value=ano[lab, "F-value"])
    df_num <- Statistic(label=paste0("df_num_", lab), type=list("df", "df_num"), value=ano[lab, "numDF"])
    df_den <- Statistic(label=paste0("df_den_", lab), type=list("df", "df_den"), value=ano[lab, "denDF"])
    pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=ano[lab, "p-value"], isAbout=list(hypo))

    pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
    pTest <- Process(paste0("testingTerm_", lab), processType="testing")

    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_num)
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_den) #TODO separate process for DenDF approx than for numDF?
    pTest$hasInput <- append(pTest$hasInput, list(df_num, df_den, hypo, fstat))
    pTest$hasOutput <- append(pTest$hasOutput, list(pval))
    procTest$hasPart <- append(procTest$hasPart, pTest)
    procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr)
  }

  list(procEst, procTest)
}


getEmmeans_2 <- function(mod) {

  procEmms <- Process("EmmCalculation", processType="ModelParameterEstimation")
  procDfAppr <- Process("EmmDfCalculation", processType="DfCalculation", type="containment")
  procConfIntCalc <- Process("confIntCalculation", processType="ConfidenceIntervalCalculation")
  procConfIntCalc$hasPart <- append(procConfIntCalc$hasPart, procDfAppr)

  varLabs <- unique(unlist(strsplit(attr(terms(mod), "term.labels"),":"))) # get variable names
  isCont <- function(x) {is(getEntity("Variable", x),"ContinuousVariable")}
  contVarLabs <- varLabs[unlist(lapply(varLabs, isCont))]
  varLabs <- varLabs[!unlist(lapply(varLabs, isCont))]

  for (i in 1:length(varLabs)) {
    if (i == 1) {
      com <- combn(c(varLabs, contVarLabs), i)
    } else if (i > 1) {
      com <- combn(varLabs, i)
    } else {
      break
    }
    for (j in 1:(dim(com)[2])) {

      eq <- paste0("~", paste(com[,j], collapse = "*"))
      emms <- emmeans::emmeans(mod, formula(eq))
      emmsLab <- paste(com[,j], collapse = ".")
      ems <- data.frame(emms)

      msg <- attr(summary(emms), "mesg")

      df_method <- sub(patt="Degrees-of-freedom method: ", x=grep("Degrees-of-freedom method: ", msg, v=T) , rep="")
      df_method <- paste0(sub(pat="-", x=df_method, ""))#, "Method")
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
          lev <- getEntity("Level", levLab)
          if (is.null(lev)) {
            var <- getEntity("Variable", names(ems)[l])
            refLab <- paste0(var$label,"=",levLab)
            lev <- getEntity("ValueSpecification", refLab)
            if (is.null(lev)) {
              lev <- ValueSpecification(refLab, value = as.numeric(levLab), variable=var) #isAbout=list(var), type="EMM")
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

