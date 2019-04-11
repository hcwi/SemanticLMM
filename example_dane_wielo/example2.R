{
  setwd(dir = "Code/R_oom/example_dane_wielo")
  ex2 <- read.table("dane_wielo.txt", header = T, sep="\t", dec = ",")
  str(ex2)
  ex2[,5] <- as.factor(ex2[,5])
  ex2[,6] <- as.factor(ex2[,6])
  ex2$const <- as.factor(1)
  ex2$id <- paste(ex2$Fact, ex2$Block, sep="")
  require(reshape2)
  ex2_long <- melt(ex2, id.vars = c(5:8))
  ex2_long$id2 <- as.numeric(paste(ex2_long$id, sub("y", "", x = ex2_long$variable), sep=""))
  ex2_long$Block2 <-  rep(c(rep(8,3), rep(9,3)),4)
  #names(ex2_long) <- toupper(names(ex2_long))
  ex2_long
} # get data

require(nlme)

{
  mex2 <- lme(value ~ 0 + Fact:variable, data = ex2_long, random=~1|variable/Block) 
  mex2
  mex2$modelStruct$reStruct$variable
  mex2$modelStruct$reStruct$Block
  ranef(mex2)
  VarCorr(mex2)
  vcov(mex2)
} # model Variable / Block

{
  mex2t <- lme(value ~ 0 + Fact:variable, data = ex2_long, random=~1|Block/variable) 
  mex2t
  mex2t$modelStruct$reStruct$variable
  mex2t$modelStruct$reStruct$Block
  coef(mex2t$modelStruct$reStruct$Block, unconstrained = F)
  
  ranef(mex2t)
  VarCorr(mex2t)
  vcov(mex2t)
} # model Block / variable

{
  test0 <- lme(value ~ 0 + Fact:variable, data = ex2_long 
               , random=list(Block=pdIdent(~1)) # == random = ~1|Block
  )
  test0
  VarCorr(test0)
  corMatrix(test0$modelStruct$reStruct$Block)
} # model Block = 1

  
{
  test02 <- lme(value ~ 0 + Fact:variable, data = ex2_long 
               , random=list(Block2=pdIdent(~1))
  )
  test02
  VarCorr(test02)
} # model Block = 1

{
  test <- lme(value ~ 0 + Fact:variable, data = ex2_long 
              , random=list(Block=pdCompSymm(~Block-1))
  )
  test
  VarCorr(test)
  corMatrix(test$modelStruct$reStruct$Block)
  ranef(test)
  coef(test$modelStruct$reStruct$Block, unconstrained = T) # == attr(corMatrix(mex2c$modelStruct$reStruct$Block, unconstrained=F),"stdDev")
} # model Block = CS(Block)

{
  test2 <- lme(value ~ 0 + Fact:variable, data = ex2_long 
              , random=list(Block2=pdCompSymm(~Block-1))
  )
  test2
  VarCorr(test2)
  corMatrix(test2$modelStruct$reStruct$Block2)
  coef(test$modelStruct$reStruct$Block, unconstrained = T) # == attr(corMatrix(mex2c$modelStruct$reStruct$Block, unconstrained=F),"stdDev")
} # model Block = CS(Block)


{
  testV <- lme(value ~ 0 + Fact, data = ex2_long 
              , random=list(variable=pdSymm(~variable-1))
  )
  testV
  VarCorr(testV)
  corMatrix(testV$modelStruct$reStruct$variable)
  coef(testV$modelStruct$reStruct$variable, unconstrained = F) # == attr(corMatrix(mex2c$modelStruct$reStruct$Block, unconstrained=F),"stdDev")
} # model variable = CS(variable)


{
  mex2b <- lme(value ~ 0 + Fact:variable, data = ex2_long 
               , random=list(Block=pdBlocked(list(pdIdent(~Block-1), 
                                                  pdCompSymm(~variable-1))))
               #, correlation = corIdent(~variable-1)
  ) 
  mex2b
  VarCorr(mex2b) # == coef(mex2b$modelStruct$reStruct$Block, unconstrained = F) * mex2b$sigma
  intervals(mex2b) #, which="fixed")
  #vcov(mex2b)
  round(vcov(mex2b), d=3)
  mex2b$modelStruct$reStruct$Block
  coef(mex2b$modelStruct$reStruct$Block, unconstrained = F)
  #coef(mex2b$modelStruct$reStruct$Block, unconstrained = F) * mex2b$sigma == VarCorr
  ranef(mex2b)
} # model Block = pdIdent(Block), pdSymm(variable)

{
  mex2b2 <- lme(value ~ 0 + Fact:variable, data = ex2_long 
               , random=list(Block=pdIdent(~Block-1), 
                             variable = pdCompSymm(~variable-1))
               #, correlation = corIdent(~variable-1)
  ) 
  mex2b2
  VarCorr(mex2b2)
}