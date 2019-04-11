
# Identifying relations for fixed effects
tmp2 <- lme(Yield ~ Variety*Infection, data=dataFus, random=~1|Block)

mmx <- model.matrix(tmp2, dataFus)
attr <- attr(mmx, "assign")

for (i in 1:dim(mmx)[2]) {
  patternId <- names(mmx[,1])[mmx[,i] == 1][1]
  mmx[patternId,]
  ref <- colnames(mmx)[attr < attr[i] & mmx[patternId,] == TRUE]
  print(paste(colnames(mmx)[i], "relative to", ref))
}

# Manual input to create a the model in RDF

# independent variables

  init()
  modelFitting <- Process("modelFitting", processType="ModelFitting")
  modelFitting$hasInput <- append(modelFitting$hasInput, Dataset("FusariumField", url="http://cropnet.pl/plantphenodb/index.php?id=103"))


var1 <- ContinuousVariable(label = "Mass",
                           levels = 1,
                           type=list("IndependentVariable"))
var2 <- CategoricalVariable(label = "Variety", 
                    levels = list("Variety1", "Variety2"), 
                    type="IndependentVariable")
var3 <- CategoricalVariable(label = "Block", 
                            levels = list("Block1", "Block2"), 
                            type="IndependentVariable")

# model
lmm <- Lmm(label=paste0("model_", "ManualModel"), 
           formula = as.formula("Y ~ Mass + Variety + (1|Block)"), 
           vars = list(var1, var2, var3))
  
  modelFitting$hasInput <- append(modelFitting$hasInput, lmm)

# dependent variables
lmm$dependentVariable <- list(ContinuousVariable(label = "Y", type="DependentVariable"))

# fixed terms 

fixTer1 <- FixedModelTerm(label = "(Intercept)", order = as.integer(0))
fixTer2 <- FixedModelTerm(label = "Mass", order = as.integer(1), variable = list(var1))
fixTer3 <- FixedModelTerm(label = "Variety", order = as.integer(2), variable = list(var2))
  lmm$independentFixedTerm <- list(fixTer1, fixTer2, fixTer3)

# error term
  lmm$errorTerm <- getErrorTerm_2()

  
# random terms
  lmm$independentRandomTerm <- list()
  
.termName <- "Block"
randomTerm <- RandomModelTerm(.termName, variable = list(var3))
  lmm$independentRandomTerm <- append(lmm$independentRandomTerm, randomTerm)

  procEst <- Process("varCompEstimation", processType="ModelParameterEstimation", type="REML")
  modelFitting$hasPart <- append(modelFitting$hasPart, list(procEst))

# varCov + params
  randomTerm$covarianceStructure <- list()
  cs <- CovarianceStructure_2(.termName, #unless there are mone covStr for one term -> covVarName
                          covModel = "pdIdent", 
                          params=list(param),
                          vars = list(var3)) # cov specific
  randomTerm$covarianceStructure <- append(randomTerm$covarianceStructure, cs)

  name <- paste0("sigma2_", .termName)
  param <- Parameter(name, type=list("Variance", "VarianceParameter")) 
est <- Estimate(name, value = 123, parameter = param, type="Estimate")
  param$estimate <- list(est)
  cs$params <- append(cs$params, param)
  procEst$hasOutput <- append(procEst$hasOutput, est)

#name <- paste0("corr_", .termName)
#param <- Parameter(name, type=list("Correlation", "VarianceParameter")) 
#est <- Estimate(name, value = 123, parameter = param, type="Estimate")
#param$estimate <- list(est)
#cs$params <- append(cs$params, param)
#procEst$hasOutput <- append(procEst$hasOutput, est)


#residual
  name <- "sigma2_Residual"
  param <- getEntity("Parameter", name)
est <- Estimate(name, value = 0.123, parameter = param, type="Estimate")
  param$estimate <- list(est)
  procEst$hasOutput <- append(procEst$hasOutput, est)



# random effects + BLUPS
  randomTerm$effect <- list()
  procPred <- Process("paramPrediction", processType="ModelParameterEstimation", type="BLUP")
  modelFitting$hasPart <- append(modelFitting$hasPart, list(procPred))

randomEffectName = "Block1"
eff <- Parameter(randomEffectName, levels = var3$levels[1], type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 123, parameter = eff)
  eff$estimate <- append(eff$estimate, est)
  randomTerm$effect <- append(randomTerm$effect, eff)
  procPred$hasOutput <- append(procPred$hasOutput, est)

randomEffectName = "Block2"
eff <- Parameter(randomEffectName, levels = var3$levels[2], type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 999, parameter = eff)
  eff$estimate <- append(eff$estimate, est)
  randomTerm$effect <- append(randomTerm$effect, eff)
  procPred$hasOutput <- append(procPred$hasOutput, est)

  
# BLUEs
  procEst <- Process("paramEstimation", processType="ModelParameterEstimation", type="BLUE")
  procDfAppr <- Process("dfCalculation", processType="DfCalculation", type="SatterthwaiteApprox")
  procTest <- Process("testing", processType="testing")
  procTest$hasPart <- append(procTest$hasPart, procDfAppr)

  #1
  {
  lab <- "(Intercept)"
    effect <- Parameter(label=lab, levels=list(), type = list("fixedEffect", "directEffect"), reference = list())
  lmm$independentFixedTerm[[1]]$effect <- append(lmm$independentFixedTerm[[1]]$effect, effect)
    
    # estimates
  est <- Estimate(lab, value = 999, parameter = effect, type="Estimate")
  est$se <- 0.999
    effect$estimate <- append(effect$estimate, est)
    procEst$hasOutput <- append(procEst$hasOutput, est)
    
    # testing effects
  hypo <- Hypothesis(label=lab, pvalue=0.999, modelParams = list(effect))
  tstat <- Statistic(label=paste0("t-stat_", lab), type="Tstatistic", value=999)
  df <- Statistic(label=paste0("df_", lab), type="df", value=999)
    pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=hypo$pvalue, isAbout=list(hypo)) #same pvalue as in hypo
    
    pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df)
    procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr)
    
    pTest <- Process(paste0("testingEffect_", lab), processType="testing")
    pTest$hasInput <- append(pTest$hasInput, list(df, hypo, tstat))
    pTest$hasOutput <- append(pTest$hasOutput, list(pval))
    
    procTest$hasPart <- append(procTest$hasPart, pTest)
  }
  
  #2
  {
    lab <- "Mass"
    effect <- Parameter(label=lab, 
                        levels=list(getEntity("ValueSpecification", "Mass=1")), 
                        type = list("fixedEffect", "relativeEffect", "Contrast"), 
                        reference = list(lmm$independentFixedTerm[[1]]$effect[[1]]))
    lmm$independentFixedTerm[[2]]$effect <- append(lmm$independentFixedTerm[[2]]$effect, effect)
    
    # estimates
    est <- Estimate(lab, value = 999, parameter = effect, type="Estimate")
    est$se <- 0.999
      effect$estimate <- append(effect$estimate, est)
      procEst$hasOutput <- append(procEst$hasOutput, est)
    
    # testing effects
    hypo <- Hypothesis(label=lab, pvalue=0.999, modelParams = list(effect))
    tstat <- Statistic(label=paste0("t-stat_", lab), type="Tstatistic", value=999)
    df <- Statistic(label=paste0("df_", lab), type="df", value=999)
      pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=hypo$pvalue, isAbout=list(hypo)) #same pvalue as in hypo
    
      pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
      pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df)
      procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr)
    
      pTest <- Process(paste0("testingEffect_", lab), processType="testing")
      pTest$hasInput <- append(pTest$hasInput, list(df, hypo, tstat))
      pTest$hasOutput <- append(pTest$hasOutput, list(pval))
      
      procTest$hasPart <- append(procTest$hasPart, pTest)
  }
  
  #3
  {
    lab <- "Variety2"
    effect <- Parameter(label=lab, 
                        levels=list(getEntity("Level", lab)), 
                        type = list("fixedEffect", "relativeEffect", "Contrast"), 
                        reference = list(lmm$independentFixedTerm[[1]]$effect[[1]]))
    lmm$independentFixedTerm[[3]]$effect <- append(lmm$independentFixedTerm[[3]]$effect, effect)
    
    # estimates
    est <- Estimate(lab, value = 999, parameter = effect, type="Estimate")
    est$se <- 0.999
      effect$estimate <- append(effect$estimate, est)
      procEst$hasOutput <- append(procEst$hasOutput, est)
      
    # testing effects
    hypo <- Hypothesis(label=lab, pvalue=0.999, modelParams = list(effect))
    tstat <- Statistic(label=paste0("t-stat_", lab), type="Tstatistic", value=999)
    df <- Statistic(label=paste0("df_", lab), type="df", value=999)
      pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=hypo$pvalue, isAbout=list(hypo)) #same pvalue as in hypo
    
      pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
      pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df)
      procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr)
      
      pTest <- Process(paste0("testingEffect_", lab), processType="testing")
      pTest$hasInput <- append(pTest$hasInput, list(df, hypo, tstat))
      pTest$hasOutput <- append(pTest$hasOutput, list(pval))
      
      procTest$hasPart <- append(procTest$hasPart, pTest)
  }
  
  
# testing terms

#1
{
  term <- lmm$independentFixedTerm[[1]]
  lab <- term$label
  
hypo <- Hypothesis(label=lab, pvalue=0.999, modelParams = list(term))
fstat <- Statistic(label=paste0("f-stat_", lab), type="Fstatistic", value=999)
df_num <- Statistic(label=paste0("df_num_", lab), type=list("df", "df_num"), value=999)
df_den <- Statistic(label=paste0("df_den_", lab), type=list("df", "df_den"), value=999) 
  pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=hypo$pvalue, isAbout=list(hypo))

  pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
  pTest <- Process(paste0("testingTerm_", lab), processType="testing")
  
  pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_num)
  pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_den) #TODO separate process for DenDF approx than for numDF?
  pTest$hasInput <- append(pTest$hasInput, list(df_num, df_den, hypo, fstat))
  pTest$hasOutput <- append(pTest$hasOutput, list(pval))
  procTest$hasPart <- append(procTest$hasPart, pTest)
  procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr) 
  
  modelFitting$hasPart <- append(modelFitting$hasPart, procEst)
  modelFitting$hasPart <- append(modelFitting$hasPart, procTest)
}

#2
  {
  term <- lmm$independentFixedTerm[[2]]
    lab <- term$label
    
  hypo <- Hypothesis(label=lab, pvalue=0.999, modelParams = list(term))
  fstat <- Statistic(label=paste0("f-stat_", lab), type="Fstatistic", value=999)
  df_num <- Statistic(label=paste0("df_num_", lab), type=list("df", "df_num"), value=999)
  df_den <- Statistic(label=paste0("df_den_", lab), type=list("df", "df_den"), value=999) 
    pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=hypo$pvalue, isAbout=list(hypo))
    
    pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
    pTest <- Process(paste0("testingTerm_", lab), processType="testing")
    
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_num)
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_den) #TODO separate process for DenDF approx than for numDF?
    pTest$hasInput <- append(pTest$hasInput, list(df_num, df_den, hypo, fstat))
    pTest$hasOutput <- append(pTest$hasOutput, list(pval))
    procTest$hasPart <- append(procTest$hasPart, pTest)
    procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr) 
    
    modelFitting$hasPart <- append(modelFitting$hasPart, procEst)
    modelFitting$hasPart <- append(modelFitting$hasPart, procTest)
  }
  
  #3
  {
  term <- lmm$independentFixedTerm[[3]]
    lab <- term$label
    
  hypo <- Hypothesis(label=lab, pvalue=0.999, modelParams = list(term))
  fstat <- Statistic(label=paste0("f-stat_", lab), type="Fstatistic", value=999)
  df_num <- Statistic(label=paste0("df_num_", lab), type=list("df", "df_num"), value=999)
  df_den <- Statistic(label=paste0("df_den_", lab), type=list("df", "df_den"), value=999) 
    pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=hypo$pvalue, isAbout=list(hypo))
    
    pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation", type="SatterthwaiteApprox")
    pTest <- Process(paste0("testingTerm_", lab), processType="testing")
    
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_num)
    pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_den) #TODO separate process for DenDF approx than for numDF?
    pTest$hasInput <- append(pTest$hasInput, list(df_num, df_den, hypo, fstat))
    pTest$hasOutput <- append(pTest$hasOutput, list(pval))
    procTest$hasPart <- append(procTest$hasPart, pTest)
    procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr) 
    
    modelFitting$hasPart <- append(modelFitting$hasPart, procEst)
    modelFitting$hasPart <- append(modelFitting$hasPart, procTest)
  }
  


  
    procEmms <- Process("EmmCalculation", processType="ModelParameterEstimation")
    procDfAppr <- Process("EmmDfCalculation", processType="DfCalculation", type="containment")
    procConfIntCalc <- Process("confIntCalculation", processType="ConfidenceIntervalCalculation")
    procConfIntCalc$hasPart <- append(procConfIntCalc$hasPart, procDfAppr)
    
  #1
  {
      emmsLab = "TEST_EMM"
      df <- Statistic(label=paste0("df_emm_", emmsLab), type="df", value=999) 
      df_method <- "containment"
        pDfAppr <- Process(paste0("dfCalculation_", df_method, "_", emmsLab), 
                         processType="DfCalculation", type=df_method)
        pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df)
        procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr)
      
      conf_lev = 0.999
        confLevel <- Statistic(label=paste0("confLevel_", emmsLab), type="confidenceLevel", value=conf_lev)
        pConfIntCalc <- Process(paste0("confIntCalculation_", emmsLab), 
                              processType="ConfidenceIntervalCalculation")
        pConfIntCalc$hasInput <- append(pConfIntCalc$hasInput, c(confLevel, df))
        procConfIntCalc$hasPart <- append(procConfIntCalc$hasPart, pConfIntCalc)
      
      effLab <- "TEST_EFFLAB"
      effect <- Parameter(paste0("emm_", effLab), type = "emm",
                          levels = list(getEntity("Level", "Variety1")))
      est <- Estimate(paste0('emm_', effLab), value = 999, parameter = effect)
      est$se <- 0.999
        effect$estimate <- append(effect$estimate, est)
        procEmms$hasOutput <- append(procEmms$hasOutput, est)
          
          confInt <- Statistic(label=paste0("confInt_", effLab), type="confidenceInterval", isAbout = list(effect))
          confLevel <- Statistic(label=paste0("confLevel_", effLab), type="confidenceLevel", value=conf_lev, isAbout=list(confInt))
        lowerCL <- Statistic(label=paste0("lcl_", effLab), type="lowerConfidenceLimit", value=0.999)
        upperCL <- Statistic(label=paste0("ucl_", effLab), type="upperConfidenceLimit", value=999)
          confInt$hasPart <- list(confLevel, lowerCL, upperCL)
          pConfIntCalc$hasOutput <- append(pConfIntCalc$hasOutput, confInt)
  }
  
modelFitting$hasPart <- append(modelFitting$hasPart, procEmms)
modelFitting$hasPart <- append(modelFitting$hasPart, procConfIntCalc)


graphName <- modelFitting$hasInput[[1]]$id
print(graphName)
capture.output(cat(prefixes), 
               cat("<graphs/graph_", graphName, ">", sep=""), 
               cat(" {\n"),
               modelFitting, 
               cat("}"),
               file = paste0("out", .Platform$file.sep, graphName, ".trig"))
