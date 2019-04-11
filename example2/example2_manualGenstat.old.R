
# Manual input to create a the model in RDF

# independent variables

init()
modelFitting <- Process("modelFittingGenstat", processType="ModelFitting")
modelFitting$hasInput <- append(modelFitting$hasInput, Dataset("example2.xlsx", url="http://cropnet.pl/plantphenodb/index.php?id=103"))


var1 <- CategoricalVariable(label = "Timepoint",
                           levels = list("y1", "y2", "y3", "y4"),
                           type=list("IndependentVariable"))
var2 <- CategoricalVariable(label = "Treatment", 
                            levels = list("Treatment1", "Treatment2", "Treatment3"), 
                            type="IndependentVariable")
var3 <- CategoricalVariable(label = "Block", 
                            levels = list("Block1", "Block2"), 
                            type="IndependentVariable")
var4 <- ContinuousVariable(label = "Y", 
                           type="DependentVariable")

# model
lmm <- Lmm(label=paste0("model_", "ManualModel"), 
           formula = "VCOMPONENTS [FIXED=%_Timepoint.%_Treatment; CONSTANT=omit];
random = %_Timepoint.%_Block + %_Timepoint.%_units;CONSTRAINTS=positive;
VSTRUCTURE [TERMS=%_Timepoint.%_Treatment] FACTOR=%_Timepoint; MODEL=ar;
VSTRUCTURE [TERMS=%_Timepoint.%_units] FACTOR=%_Timepoint; MODEL=diag;", 
           vars = list(var1, var2, var3))
lmm$dependentVariable <- list(var4)

modelFitting$hasInput <- append(modelFitting$hasInput, lmm)


# fixed terms 

fixTer1 <- FixedModelTerm(label = "Timepoint:Treatment", order = as.integer(0), variable = list(var1, var2))
lmm$independentFixedTerm <- list(fixTer1)

# error term
#lmm$errorTerm <- getErrorTerm_2() #to chyba nie!


procEst <- Process("varCompEstimation", processType="ModelParameterEstimation", type="REML")
modelFitting$hasPart <- append(modelFitting$hasPart, list(procEst))

# random terms
lmm$independentRandomTerm <- list()

#1
.termName <- "Timepoint:Block"
randomTerm <- RandomModelTerm(.termName, variable = list(var1, var3))
lmm$independentRandomTerm <- append(lmm$independentRandomTerm, randomTerm)

# varCov + params
randomTerm$covarianceStructure <- list()

cs <- CovarianceStructure_2(var1$label,
                        covModel = "pdAR", 
                        params=list(),
                        vars = list(var1))
randomTerm$covarianceStructure <- append(randomTerm$covarianceStructure, cs)
  
name <- paste0("phi_", var1$label)
param <- Parameter(name, type=list("Correlation", "VarianceParameter")) 
est <- Estimate(name, value = -0.995, parameter = param, type="Estimate")
param$estimate <- list(est)
cs$params <- append(cs$params, param)
procEst$hasOutput <- append(procEst$hasOutput, est)

name <- paste0("sigma2_", .termName)
param <- Parameter(name, type=list("Variance", "VarianceParameter")) 
est <- Estimate(name, value = 1.175, parameter = param, type="Estimate", se=1.65)
param$estimate <- list(est)
cs$params <- append(cs$params, param)
procEst$hasOutput <- append(procEst$hasOutput, est)

tmp <- param
cs <- CovarianceStructure_2(var3$label,
                            covModel = "pdIdent", 
                            params=list(tmp),
                            vars = list(var3)) # cov specific
randomTerm$covarianceStructure <- append(randomTerm$covarianceStructure, cs)


#2 (random errorTerm)
.termName <- "Timepoint"
randomTerm <- RandomModelTerm(.termName, variable = list(var1), type="ErrorModelTerm")
lmm$errorTerm <- append(lmm$errorTerm, randomTerm)
# varCov + params
randomTerm$covarianceStructure <- list()

cs <- CovarianceStructure_2(var1$label,
                            covModel = "pdDiag", 
                            params=list(),
                            vars = list(var1))
randomTerm$covarianceStructure <- append(randomTerm$covarianceStructure, cs)

name <- paste0("sigma2_", var1$label, 1)
param <- Parameter(name, type=list("Variance", "VarianceParameter")) 
est <- Estimate(name, value = 3.987, se=3.1, parameter = param, type="Estimate")
param$estimate <- list(est)
cs$params <- append(cs$params, param)
procEst$hasOutput <- append(procEst$hasOutput, est)

name <- paste0("sigma2_", var1$label, 2)
param <- Parameter(name, type=list("Variance", "VarianceParameter")) 
est <- Estimate(name, value = 6.697, se=6.226, parameter = param, type="Estimate")
param$estimate <- list(est)
cs$params <- append(cs$params, param)
procEst$hasOutput <- append(procEst$hasOutput, est)

name <- paste0("sigma2_", var1$label, 3)
param <- Parameter(name, type=list("Variance", "VarianceParameter")) 
est <- Estimate(name, value = 2.146, se=1.764, parameter = param, type="Estimate")
param$estimate <- list(est)
cs$params <- append(cs$params, param)
procEst$hasOutput <- append(procEst$hasOutput, est)

name <- paste0("sigma2_", var1$label, 4)
param <- Parameter(name, type=list("Variance", "VarianceParameter")) 
est <- Estimate(name, value = 0.1677, se=0.168, parameter = param, type="Estimate")
param$estimate <- list(est)
cs$params <- append(cs$params, param)
procEst$hasOutput <- append(procEst$hasOutput, est)

tmp <- cs$params
cs <- CovarianceStructure_2("Residual",
                            covModel = "pdIdent", 
                            params=tmp,
                            vars = list()) # cov specific
randomTerm$covarianceStructure <- append(randomTerm$covarianceStructure, cs)



# random effects + BLUPS
randomTerm$effect <- list()
procPred <- Process("paramPrediction", processType="ModelParameterEstimation", type="BLUP")
modelFitting$hasPart <- append(modelFitting$hasPart, list(procPred))

i <- 1; j <- 1;
randomEffectName = paste(var1$levels[[i]]$label, var3$levels[[j]]$label, sep=":")
eff <- Parameter(randomEffectName, levels = list(var1$levels[[i]], var3$levels[[j]]), type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 6.276, se=0.8378, parameter = eff)
eff$estimate <- append(eff$estimate, est)
randomTerm$effect <- append(randomTerm$effect, eff)
procPred$hasOutput <- append(procPred$hasOutput, est)

i <- 1; j <- 2;
randomEffectName = paste(var1$levels[[i]]$label, var3$levels[[j]]$label, sep=":")
eff <- Parameter(randomEffectName, levels = list(var1$levels[[i]], var3$levels[[j]]), type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 4.724, se=0.8378, parameter = eff)
eff$estimate <- append(eff$estimate, est)
randomTerm$effect <- append(randomTerm$effect, eff)
procPred$hasOutput <- append(procPred$hasOutput, est)

i <- 2; j <- 1;
randomEffectName = paste(var1$levels[[i]]$label, var3$levels[[j]]$label, sep=":")
eff <- Parameter(randomEffectName, levels = list(var1$levels[[i]], var3$levels[[j]]), type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 3.557, se=1.0718, parameter = eff)
eff$estimate <- append(eff$estimate, est)
randomTerm$effect <- append(randomTerm$effect, eff)
procPred$hasOutput <- append(procPred$hasOutput, est)

i <- 2; j <- 2;
randomEffectName = paste(var1$levels[[i]]$label, var3$levels[[j]]$label, sep=":")
eff <- Parameter(randomEffectName, levels = list(var1$levels[[i]], var3$levels[[j]]), type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 5.110, se=1.0718, parameter = eff)
eff$estimate <- append(eff$estimate, est)
randomTerm$effect <- append(randomTerm$effect, eff)
procPred$hasOutput <- append(procPred$hasOutput, est)

i <- 3; j <- 1;
randomEffectName = paste(var1$levels[[i]]$label, var3$levels[[j]]$label, sep=":")
eff <- Parameter(randomEffectName, levels = list(var1$levels[[i]], var3$levels[[j]]), type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 6.283, se=0.6207, parameter = eff)
eff$estimate <- append(eff$estimate, est)
randomTerm$effect <- append(randomTerm$effect, eff)
procPred$hasOutput <- append(procPred$hasOutput, est)

i <- 3; j <- 2;
randomEffectName = paste(var1$levels[[i]]$label, var3$levels[[j]]$label, sep=":")
eff <- Parameter(randomEffectName, levels = list(var1$levels[[i]], var3$levels[[j]]), type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 4.717, se=0.6207, parameter = eff)
eff$estimate <- append(eff$estimate, est)
randomTerm$effect <- append(randomTerm$effect, eff)
procPred$hasOutput <- append(procPred$hasOutput, est)

i <- 4; j <- 1;
randomEffectName = paste(var1$levels[[i]]$label, var3$levels[[j]]$label, sep=":")
eff <- Parameter(randomEffectName, levels = list(var1$levels[[i]], var3$levels[[j]]), type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 4.712, se=0.227, parameter = eff)
eff$estimate <- append(eff$estimate, est)
randomTerm$effect <- append(randomTerm$effect, eff)
procPred$hasOutput <- append(procPred$hasOutput, est)

i <- 4; j <- 2;
randomEffectName = paste(var1$levels[[i]]$label, var3$levels[[j]]$label, sep=":")
eff <- Parameter(randomEffectName, levels = list(var1$levels[[i]], var3$levels[[j]]), type=list("RandomEffect"), effectType="random") #ModelParameter X, then what? #TODO eBLUP?
est <- Estimate(randomEffectName, value = 6.288, se=0.227, parameter = eff)
eff$estimate <- append(eff$estimate, est)
randomTerm$effect <- append(randomTerm$effect, eff)
procPred$hasOutput <- append(procPred$hasOutput, est)









# BLUEs
procEst <- Process("paramEstimation", processType="ModelParameterEstimation", type="BLUE")
procDfAppr <- Process("dfCalculation", processType="DfCalculation")
procTest <- Process("testing", processType="testing")
procTest$hasPart <- append(procTest$hasPart, procDfAppr)


#1-12
values <- matrix(c(5,2.5, 5, 5, 5.5, 3, 5, 3, 6, 7.5, 6.5, 8.5), ncol = 3)
for (i in 1:dim(values)[1]) {
  for (j in 1:dim(values)[2]) {
    l1 <- var1$levels[[i]]
    l2 <- var2$levels[[j]]
    lab <- paste0(l1$label, ":", l2$label)
    effect <- Parameter(label=lab, 
                        levels=list(l1,l2), 
                        type = list("fixedEffect", "emm"))
    lmm$independentFixedTerm[[1]]$effect <- append(lmm$independentFixedTerm[[1]]$effect, effect)
    
    # estimates
    est <- Estimate(lab, value = values[i,j], parameter = effect, type="Estimate")
    est$se <- 1.425
    effect$estimate <- append(effect$estimate, est)
    procEst$hasOutput <- append(procEst$hasOutput, est)
    
  }
}

# variances and covariances of fixed effects

pEst <- Process("covEstimation", processType="ModelParameterEstimation")
procEst$hasPart <- append(procEst$hasPart, pEst)


tmp_cov <- c(2.5809, 0.5873, 2.5809, 0.5873, 0.5873, 2.5809, -0.5843,-0.5843,-0.5843, 3.9359, -0.5843,-0.5843,-0.5843, 0.5873, 3.9359, 
             -0.5843,-0.5843,-0.5843, 0.5873, 0.5873, 3.9359, 0.5814, 0.5814, 0.5814,-0.5843,-0.5843,-0.5843, 1.6601, 0.5814, 0.5814, 
             0.5814,-0.5843,-0.5843,-0.5843, 0.5873, 1.6601, 0.5814, 0.5814, 0.5814,-0.5843,-0.5843,-0.5843, 0.5873, 0.5873, 1.6601, 
             -0.5785,-0.5785,-0.5785, 0.5814, 0.5814, 0.5814,-0.5843,-0.5843,-0.5843,0.6711, -0.5785,-0.5785,-0.5785, 0.5814, 0.5814, 
             0.5814,-0.5843,-0.5843,-0.5843,0.5873,0.6711, -0.5785,-0.5785,-0.5785, 0.5814, 0.5814, 0.5814,-0.5843,-0.5843,-0.5843,
             0.5873,0.5873,0.6711)
cov <- matrix(0, nrow = 12, ncol=12)
cov[!lower.tri(cov)] <- tmp_cov
cov <- t(cov)

tmp_se <- c(1.6065,0.7663,1.6065,0.7663,0.7663,1.6065,NA,NA,NA,1.9839,NA,NA,NA,0.7663,1.9839,NA,NA,NA,0.7663,0.7663,1.9839,0.7625,0.7625,
            0.7625,NA,NA,NA,1.2884,0.7625,0.7625,0.7625,NA,NA,NA,0.7663,1.2884,0.7625,0.7625,0.7625,NA,NA,NA,0.7663,0.7663,1.2884,NA,NA,NA,
            0.7625,0.7625,0.7625,NA,NA,NA,0.8192,NA,NA,NA,0.7625,0.7625,0.7625,NA,NA,NA,0.7663,0.8192,NA,NA,NA,0.7625,0.7625,0.7625,NA,NA,
            NA,0.7663,0.7663,0.8192)
se <- matrix(0, nrow = 12, ncol=12)
se[!lower.tri(se)] <- tmp_se
se <- t(se)

for (i in 1:length(var1$levels)) {
  l1 <- var1$levels[[i]]
  for (j in 1:length(var2$levels)) {
    l2 <- var2$levels[[j]]
    lab1 <- paste0(l1$label, ":", l2$label)
    eff1 <- getEntity("Parameter", lab1)
    #print(eff1$label)
    row <- (i-1)*length(var2$levels)+j
    for (col in 1:row) {
      t <- ifelse(col==row, yes = "variance", "covariance")
      ii <- (col-1)%/%length(var2$levels)+1
      jj <- (col-1)%%length(var2$levels)+1
      l11 <- var1$levels[[ii]]
      l22 <- var2$levels[[jj]]
      lab2 <- paste0(l11$label, ":", l22$label)
      eff2 <- getEntity("Parameter", lab2)
      
      #print(paste(t, i,j, "<>", ii,jj, "|", eff1$label, "<>", eff2$label))
      #print(paste(i,j, "row=", row, "col=", col, "value=", cov[row, col], "se=", se[row,col]))
      #print(paste(t, lab))
      lab <- paste(t,lab1,lab2,sep="_")
      effect <- Parameter(label=lab, 
                          levels=list(eff1,eff2), 
                          type = list(t))
      #lmm$independentFixedTerm[[1]]$effect <- append(lmm$independentFixedTerm[[1]]$effect, effect)
      
      # estimates
      est <- Estimate(lab, value = cov[row,col], parameter = effect, type="Estimate")
      est$se <- se[row,col]
      effect$estimate <- append(effect$estimate, est)
      pEst$hasOutput <- append(pEst$hasOutput, est)
    }
  }
}


# testing terms

#1
{
  term <- lmm$independentFixedTerm[[1]]
  lab <- term$label
  
  hypo <- Hypothesis(label=lab, pvalue=0.001, modelParams = list(term))
  fstat <- Statistic(label=paste0("Wald-stat_", lab), type="WaldStatistic", value=671.31)
  df_num <- Statistic(label=paste0("df_", lab), type=list("df"), value=12)
  pval <- Statistic(label=paste0("pvalue_", lab), type="pvalue", value=hypo$pvalue, isAbout=list(hypo))
  
  pDfAppr <- Process(paste0("dfCalculation_", lab), processType="DfCalculation")
  pTest <- Process(paste0("testingTerm_", lab), processType="testing", type=list("WaldTest","testingOfTerm"))
  
  pDfAppr$hasOutput <- append(pDfAppr$hasOutput, df_num)
  pTest$hasInput <- append(pTest$hasInput, list(df_num, hypo, fstat))
  pTest$hasOutput <- append(pTest$hasOutput, list(pval))
  procTest$hasPart <- append(procTest$hasPart, pTest)
  procDfAppr$hasPart <- append(procDfAppr$hasPart, pDfAppr) 
  
  modelFitting$hasPart <- append(modelFitting$hasPart, procEst)
  modelFitting$hasPart <- append(modelFitting$hasPart, procTest)
}



graphName <- modelFitting$hasInput[[1]]$id
print(graphName)
capture.output(cat(prefixes), 
               cat("<graphs/graph_", graphName, ">", sep=""), 
               cat(" {\n"),
               modelFitting, 
               cat("}"),
               file = paste0("out", .Platform$file.sep, graphName, ".trig"))
