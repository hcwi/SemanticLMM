setwd(dir = "Code/R_oom/")
dat <- read.table("dane_wielowymiarowe_polapgen/przyklad_polapgen_metabo_2.txt", header = T, sep="\t", dec = ",")
head(dat)
str(dat)
dim(dat)

library(reshape2)
vecc_full <- melt(dat, id.vars = c(1:4))
str(vecc_full)
dim(vecc_full)
vecc_full$varTreat <- factor(paste(vecc_full$variable, vecc_full$Treatment, sep="_"))
head(vecc_full)

veccT3 <- vecc_full[vecc_full$Timepoint == "T3",]
dim(veccT3)  
veccT3R1 <- veccT3[veccT3$Replication == "R01",]
dim(veccT3R1)  
vecc <- droplevels(veccT3R1)
str(vecc) 
head(vecc)

veccV1 <- vecc[vecc$variable == "vtrait1",]
veccV1[20,]$value <- 29
veccV1 <- veccV1[-19,]
dim(veccV1); str(veccV1)

# reduced dataset -> 3 variables
vecctest <- droplevels(vecc[as.numeric(vecc$variable) < 4,])

{
  require(lme4)
  m <- lmer(value ~ Variety + (1|Treatment), data = veccV1, verbose = T)
  m  
  summary(m)
  fixef(m)
  coef(m)
  ranef(m)
  VarCorr(m)
  
  require(nlme)
  m0 <- lme(value~Variety, data=veccV1, random = ~1|Treatment)
  m0
  summary(m0)
  VarCorr(m0)
  anova(m, m0)
  ACF(m0); plot(ACF(m0), alpha = 0.01)
  #Variogram(m0, res="r", ); plot(Variogram(m0)) # analiza błędów nie ma sensu, bo wszystkie odmiany są od siebie "równo odległe"
  resid(m0)
  ranef(m0)
  summary(m0)$corFixed
  m0$modelStruct #parametery opisujace strukturę CS,  == coef(m0$modelStruct$reStruct, unconstrained = T) 
  coef(m0$modelStruct$reStruct, unconstrained = F) # var
  corMatrix(m0$modelStruct$reStruct$Treatment)
  ranef(m0)
  
  m0b <- update(m0, random = ~1|Treatment/Variety)
  m0b
  
  m1 <- update(m0, random = list(Treatment = pdIdent(~1)))
  m1
  summary(m1)
  VarCorr(m1)
  anova(m0, m1)
  ranef(m1)
  corMatrix(m1$modelStruct$reStruct$Treatment)
  m1$modelStruct$reStruct
  summary(m1)$corFixed
  
  m2 <- update(m0, random = list(Treatment = pdIdent(~Variety-1)))
  m2
  summary(m2)
  VarCorr(m2)
  m2$modelStruct$reStruct
  ranef(m2)  
  coef(m2)
  corMatrix(m2$modelStruct$reStruct$Treatment)
  
  m3 <- update(m0, random = list(Treatment = pdCompSymm(~Variety-1)))
  m3
  summary(m3)
  VarCorr(m3)
  m3$modelStruct #parametery opisujace strukturę CS - sigma1, sigma2, jak poniżej unconstraned=t
  coef(m3$modelStruct$reStruct, unconstrained = F) # StdDev i cor
  corMatrix(m3$modelStruct$reStruct$Treatment)
  ranef(m3)

  m3b <- lme(value~Variety, data=veccV1, random = ~1|Treatment, corr=corCompSymm(form=~Variety))
  m3b
  #compareFits(coef(m3), coef(m3b))
  summary(m3b)
  VarCorr(m3b)
  m3b$modelStruct #parametery opisujace strukturę CS - sigma1, sigma2, jak poniżej unconstraned=t
  coef(m3b$modelStruct$reStruct, unconstrained = F) # StdDev i cor
  m3b$modelStruct$corStruct
  corMatrix(m3b$modelStruct$corStruct)
  ranef(m3b)
  #korelacja -> kowariancja ??? rho macierzy korelacji * odchylenie zmiennej zagniezdzajacej? 
  coef(m3b$modelStruct$corStruct, unconstrained = F)*as.numeric(VarCorr(m3b)[1,2])
  # jakich ma byc jednostkach? czy wszystkie wartosci bezwzględnie?
  
  
  
  m4 <- update(m0, random = list(Treatment = pdDiag(~Variety-1)))
  m4
  summary(m4)
  VarCorr(m4)
  ranef(m4)
  m4$modelStruct #parametery opisujace strukturę CS - sigma1, sigma2, jak poniżej unconstraned=t
  coef(m4$modelStruct$reStruct, unconstrained = F) # przeskalowane dla resid = 1, zeby dostać to co w summary: *m4$sigma
  corMatrix(m4$modelStruct$reStruct$Treatment)
  attr(corMatrix(m4$modelStruct$reStruct$Treatment), "stdDev")*m4$sigma #random effects - sigma for each random
  
  #
  m5 <- update(m0, random = list(Treatment=pdBlocked(list(pdIdent(~1), pdIdent(~Variety-1)))))
  m5
  intervals(m5) # widać, że za mało obserwacji! losowe i błąd wychodzą odwrotnie (symetrycznie) do m2 
  summary(m5)
  VarCorr(m5)
  ranef(m4)
  m4$modelStruct #parametery opisujace strukturę CS - sigma1, sigma2, jak poniżej unconstraned=t
  coef(m4$modelStruct$reStruct, unconstrained = F) # przeskalowane dla resid = 1, zeby dostać to co w summary: *m4$sigma
  corMatrix(m4$modelStruct$reStruct$Treatment)
  attr(corMatrix(m4$modelStruct$reStruct$Treatment), "stdDev")*m4$sigma #random effects - sigma for each random
  

}




# lme4
{
  require(lme4)

  # model for full dataset, to verify the estimated variance components without specific covariance structure agains GenStat results -> ok
  mod <- lmer(value ~ 0+Variety:variable + (1|variable:Treatment), data = vecc, verbose = T)
  #mod # too big
  s <- summary(mod)
  s$coefficients
  s$sigma
  #s$vcov # too big
  as.data.frame(s$varcor)
  anova(mod)
  
  #anova(mod_id), confint(mod_id), fixef(mod_id) # Fixed effects estimators (BLUE)
  #require(emmeans)
  #ref_grid(mod_id), emmeans(mod_id, ~Variety*variable)
  as.data.frame(VarCorr(mod))
  head(ranef(mod)[[1]])
  
  # the same model, but for interaction term replaced by a dummy interaction column
  mod_vt <- lmer(value ~ 0+Variety:variable + (1|varTreat), data = vecc, verbose = T)
  s <- summary(mod_vt)
  s$coefficients
  s$sigma
  s$varcor
  as.data.frame(VarCorr(mod_vt))
  head(ranef(mod_vt)[[1]])
  anova(mod_vt, mod)
  
  # now, using reduced dataset to get a lme4 model and compare it with nlme results below (works slower, so need to use less data)
  
  # lme4 model without intercept for reduced data
  mod_test <- lmer(value ~ 0+Variety:variable + (1|Treatment:variable), data = vecctest)
  s <- summary(mod_test)
  mod_test
  as.data.frame(VarCorr(mod_test))
  ranef(mod_test)
  fixef(mod_test)
  coef(mod_test)
  
  # lme4 model with intercept for reduced data (just to check calculations)
  mod_test_int <- lmer(value ~ Variety:variable + (1|Treatment:variable), data = vecctest)
  s <- summary(mod_test_int)
  mod_test_int
  as.data.frame(VarCorr(mod_test_int))
  ranef(mod_test_int)
  fixef(mod_test_int)
  coef(mod_test_int)
  
  # lme4 model with slope variable with random effect
  #mod_test_dep <- lmer(value ~ 0 + variable + (variable|Treatment), data = vecctest)
  #mod_test_dep
  #as.data.frame(VarCorr(mod_test_dep))
  #ranef(mod_test_dep)
  #fixef(mod_test_dep)
  #coef(mod_test_dep)
  #plot(fitted(mod_test_dep))
  #points(vecctest$value, col=as.numeric(vecctest$Treatment))
  
}

#####
require(nlme)

# reduced dataset formatted as grouped data 
# not sure if this is necessary ?
gvecc <- groupedData(value~Variety:variable | variable/Treatment, data = vecc)
gvecctest <- droplevels(gvecc[as.numeric(gvecc$variable) < 4,])


mod0a <- lme(value ~ 0 + Variety:variable, 
             data = vecc, 
             random=~1|varTreat,
             control = list(msVerbose = TRUE, msMaxIter = 200, opt="optim")) 
s <- summary(mod0a); s
s$sigma
s$modelStruct$reStruct #variance
coef(mod0a$modelStruct$reStruct, unconstrained = T)
s$modelStruct$reStruct$varTreat == s$modelStruct$reStruct[[1]] #StdDev
VarCorr(s)
anova(mod0a)

#mod0 <- lme(value ~ 0 + Variety:variable, data = vecctest, random=~1|variable/Treatment) 
mod0 <- lme(value ~ 0 + Variety:variable, 
             data = vecc, 
             random=~1|Treatment/Variety,
             control = list(msVerbose = TRUE, msMaxIter = 200, opt="optim"))  
mod0
VarCorr(mod0)
intervals(mod0, which="var-cov") # very wide interval for variable -> unreliable
#ranef(mod0)
formula(mod0$modelStruct$reStruct)
mod0$modelStruct$reStruct
mod0$modelStruct$reStruct[1]
mod0$modelStruct$reStruct[2]

anova(mod0)
plot(mod0, resid(.) ~  fitted(.))
plot(mod0, resid(.) ~  fitted(.) | variable) # trait 24, 36, 58 !


# to samo co niżej, ale jedna zmienna dummy dla zagneiżdzenia variable/Treatment
anova(mod0, mod0a)

mod0_CS <- update(mod0, random=list(Treatment=pdCompSymm(~variable-1))) 
mod0_CS
VarCorr(mod0_CS)
anova(mod0_CS)
anova(mod0, mod0_CS)

mod0_ID <- update(mod0, random=list(Treatment=pdBlocked(list(~1, pdIdent(~variable-1))))) 
mod0_ID
VarCorr(mod0_ID)
anova(mod0, mod0_ID)

mod0_ID2 <- update(mod0, random=list(Treatment=pdBlocked(list(~1, pdCompSymm(~variable-1))))) 
mod0_ID2
VarCorr(mod0_ID2)
anova(mod0, mod0_ID2)

mod0_ID3 <- update(mod0, random=list(Treatment=pdIdent(~1), variable=pdIdent(~1))) 
mod0_ID3
VarCorr(mod0_ID3)

mod0_ID4 <- update(mod0, random=list(Treatment=pdDiag(~1), variable=pdIdent(~1))) 
mod0_ID4
VarCorr(mod0_ID4)

mod0_ID5 <- update(mod0, random=list(Replication=pdBlocked(list(
  pdIdent(~Treatment-1), pdIdent(~variable-1)
  )))) 
mod0_ID5
VarCorr(mod0_ID5)

mod0_ID5b <- update(mod0, random=list(Replication=pdBlocked(list(
  pdIdent(~1), pdIdent(~Treatment-1), pdIdent(~variable-1)
  )))) 
mod0_ID5b
VarCorr(mod0_ID5b)

mod0b <- update(mod0, random=~1|Treatment/variable) 
mod0b
VarCorr(mod0b)
intervals(mod0b, which="var-cov") # very wide interval for variable -> unreliable
anova(mod0, mod0b)

mod0_ID_CS <- update(mod0, random=~1|Treatment, corr=corCompSymm(form=~1|Treatment/variable))
mod0_ID_CS
VarCorr(mod0_ID_CS) #waraincje w ramach efektów losowych
v <- as.numeric(VarCorr(mod0_ID_CS)[2,1]) #waraincje dla efektu losowego Treatment
intervals(mod0_ID_CS, which="var-cov") #przedziały dla efektów losowych i struktury błedu
mod0_ID_CS$modelStruct$corStruct #structura błedu
corMatrix(mod0_ID_CS$modelStruct$corStruct)[1] #przykladowa macierz korelacji (miedzy Variety?) 1/416 (Treatment*var)
rho <- coef(mod0_ID_CS$modelStruct$corStruct, unconstrained = F)
mod0_ID_CS$modelStruct$reStruct #znormalizowane StdDev dla losowych
mod0_ID_CS$sigma #
mod0_ID_CS$sigma * sqrt(mod0_ID_CS$modelStruct$reStruct[[1]][1])

coef(mod0_ID_CS$modelStruct$reStruct, unconstrained=F) # == mod0_ID_CS$modelStruct$reStruct[[1]] #wariancja ? co to?
v * rho # powinna wyjść wariancja dla Treatment z mod0b
anova(mod0, mod0_ID_CS)

#wrong, can't have smaller corr than random
mod0_ID_CS2 <- update(mod0, random=~1|variable/Treatment, corr=corCompSymm(form=~1|variable))
mod0_ID_CS2
VarCorr(mod0_ID_CS2)
intervals(mod0_ID_CS2, which="fixed")

#wrong, big intervals for overlappings random&corr
#mod0_ID_CS_no <- update(mod0, random=~1|Treatment/variable, corr=corCompSymm(form=~1|Treatment/variable)) 
#intervals(mod0_ID_CS_no, which="var-cov") #bez sensu

mod0_ID_AR <- update(mod0, random=~1|Treatment, corr=corAR1(form=~1|Treatment/variable))
mod0_ID_AR
VarCorr(mod0_ID_AR)
intervals(mod0_ID_AR, which="var-cov")
corMatrix(mod0_ID_AR$modelStruct$corStruct)[[1]]

mod0_ID_AR2 <- update(mod0, random=~1|variable/Treatment, corr=corAR1())
mod0_ID_AR2
VarCorr(mod0_ID_AR2)
intervals(mod0_ID_AR2, which="var-cov")
corMatrix(mod0_ID_AR2$modelStruct$corStruct)[[1]]

plot(ACF(mod0_ID_CS), alpha=0.01)

str(corMatrix(mod0_ID_CS2$modelStruct$corStruct))
Variogram(mod0)
plot(Variogram(mod0))

ACF(mod0)
plot(ACF(mod0), alpha=0.01)


mod0_IDID <- update(mod0, random=list(variable=pdIdent(~1), Treatment=pdIdent(~1))) 
mod0_IDID
VarCorr(mod0_IDID)
anova(mod0, mod0_IDID)

mod0_IDDiag <- update(mod0, random=list(variable=pdDiag(~Treatment-1))) 
mod0_IDDiag
VarCorr(mod0_IDDiag)
anova(mod0, mod0_IDID)

# too big, don't run
#mod0_IDsym <- update(mod0, random=list(variable=pdSymm(~Treatment-1))) 
#mod0_IDsym
#VarCorr(mod0_IDsym)
#anova(mod0, mod0_IDsym)


mod0_ID_ID <- update(mod0, random=variable, corr = corIdent(~Treatment-1)) 
mod0_IDID
VarCorr(mod0_IDID)
anova(mod0, mod0_IDID)




mod0_ID <- lme(value ~ 0 + Variety:variable, data = vecctest, random=~1|variable/Treatment, 
               correlation = corIdent(form=~1|variable/Treatment)) 
#, correlation = corCompSymm(form = ~ variable|Treatment)) 
mod0_ID$modelStruct$reStruct
VarCorr(mod0_ID)

mod0a_ID <- lme(value ~ 0 + Variety:variable, data = vecctest, random=~1|varTreat, 
                correlation = corIdent(form=~1|varTreat)) 
             #, correlation = corCompSymm(form = ~ variable|Treatment)) 
mod0a_ID$modelStruct$reStruct # or variance: (mod0a_ID$modelStruct$reStruct[[1]])^1/2
VarCorr(mod0a_ID)

modlist <- lme(value ~ 0 + Variety:variable, data = vecctest,
               random=list(variable=~1, Treatment=~1)) # to samo co wyżej, tylko szybciej
modlist$modelStruct$reStruct
modlist2 <- lme(value ~ 0 + Variety:variable, data = vecctest,
                random=list(Treatment=~1, variable=~1)) # odwrotnie zagnieżdzone niż powyżej
modlist2$modelStruct$reStruct
mod0
modlist
modlist2
VarCorr(mod0)
VarCorr(modlist)
VarCorr(modlist2)

mod_IDxAR <- ???
mod_IDxAR
VarCorr(mod_IDxAR)

mod_CS <- lme(value ~ 0 + Variety:variable, random=list(variable=~1, Treatment=~1)
                   , correlation = corCompSymm(form=~1|variable/Treatment)
                   , data = vecctest)
mod_CS
VarCorr(mod_CS)
 mCS <- Initialize(corCompSymm(0, form=~1|variable/Treatment), data = vecctest)
 corMatrix(mCS)
  vapply(corMatrix(mCS), FUN = function(x) {print(x[1,2])}, FUN.VALUE = numeric(1))

mod_CSa <- lme(value ~ 0 + Variety:variable, random=~1|varTreat
                , correlation = corCompSymm(form=~1|varTreat)
                , data = vecctest)
mod_CSa
VarCorr(mod_CSa)
  
 
mod_AR <- lme(value ~ 0 + Variety:variable, random=list(variable=~1, Treatment=~1)
              , correlation = corAR1(form=~1|variable/Treatment)
              , data = gvecctest)
mod_AR
VarCorr(mod_AR)
 mAR <- Initialize(corAR1(form=~1|variable/Treatment), data = vecctest)
 corMatrix(mAR)
 
 
 mID <-pdIdent(0, form=~Treatment-1, data = vecctest)
 corMatrix(mID)
 
mod_ID2 <- lme(value ~ 0 + Variety:variable,
               data = gvecctest,
               random=pdIdent(~variable/Treatment-1)
               )
 mID2 <-pdIdent(0, form=~variable/Treatment-1, data = vecctest)
 corMatrix(mID2)

pdCompSymm(c(0,1), form=~variable/Treatment-1, data = vecctest)
mod_CS <- lme(value ~ 0 + Variety:variable,
               data = gvecctest,
               random=pdCompSymm(form=~variable/Treatment-1)
)
str(mod_CS$modelStruct)$reStruct


mod_IDxDiag <- lme(value ~ 0 + Variety:variable,
            data = vecctest,
            random=pdBlocked(list(
              pdDiag(~variable-1),
              pdIdent(~Treatment-1)
              ))
)
mod_IDxDiag$modelStruct$reStruct
  
mBL <- pdBlocked(list(
    pdCompSymm(c(0,1), form=~variable-1, data = vecctest),
    pdIdent(0, form=~Treatment-1, data = vecctest)
  ))
  corMatrix(mBL)

  #kronecker(as.matrix(pdIdent(0, form=~Treatment-1, data = vecctest)), as.matrix(pdCompSymm(c(1,2), form=~variable-1, data = vecctest)))
  cov <- as.matrix(pdIdent(0, form=~Treatment-1, data = vecctest)) %x% as.matrix(pdCompSymm(c(1,2), form=~variable-1, data = vecctest))
  CM <- cov2cor(cov)
  pdCM <- nearPD(CM)
  
  myCS2 <- corSymm(pdCM$mat[lower.tri(pdCM$mat)])
  myCS2i <- Initialize(myCS2, data = vecctest)
  
  mod_myCS <- lme(value ~ 0 + Treatment,
                data = vecctest,
                random=~1|Variety,
                corr = myCS2) # nie liczy
  mod_ii$modelStruct$reStruct
  
  
  
  pdDiag(value = 0:(length(levels(vecctest$Treatment))-1), form=~Treatment-1, data = vecctest)
  
  

mod_ii <- lme(value ~ 0 + Variety:variable,
              data = gvecctest,
              random=pdBlocked(list(
                pdIdent(~Treatment-1),
                pdIdent(~variable-1)))
) 
str(mod_ii$modelStruct)

mod_ic <- lme(value ~ 0 + Variety:variable,
              data = vecctest,
              random=pdBlocked(list(
                #pdIdent(~1),
                pdIdent(~Treatment-1),
                pdCompSymm(~variable-1)))
)
VarCorr(mod0)
VarCorr(mod_ii)
VarCorr(mod_ic)

# corr for correlation
{
cs1 <- corAR1(0.3)
corMatrix(cs1, covariate = 1:4)
corMatrix(cs1, covariate = 1:4, corr = FALSE)

# Pinheiro and Bates, p. 225
cs1CompSymm <- corCompSymm(value = 0.3, form = ~ 1 | Variety)
cs1CompSymm <- Initialize(cs1CompSymm, data = gvecctest)
corMatrix(cs1CompSymm)
#sum(gvecctest$Variety == "Stratus")  #tyle kowariancji, ile razy Variety wystąpiło w danych


# Pinheiro and Bates, p. 226
cs1Symm <- corSymm(value = c(0.2, 0.1, -0.1, 0, 0.2, 0),
                   form = ~ 1 | Subject)
cs1Symm <- Initialize(cs1Symm, data = Orthodont)
corMatrix(cs1Symm)

cs1Symm <- corSymm(#value = rep(0.1,276),
                   form = ~ 1 | Variety)
cs1Symm <- Initialize(cs1Symm, data = gvecctest)
corMatrix(cs1Symm)


# Pinheiro and Bates, p. 236 
cs1AR1 <- corAR1(0.8, form = ~ 1 | Variety)
cs1AR1 <- Initialize(cs1AR1, data = gvecctest)
corMatrix(cs1AR1)

# Pinheiro and Bates, p. 237 
cs1ARMA <- corARMA(0.4, form = ~ 1 | Variety, q = 1)
cs1ARMA <- Initialize(cs1ARMA, data = gvecctest)
corMatrix(cs1ARMA)

# Pinheiro and Bates, p. 238 
spatDat <- data.frame(x = (0:4)/4, y = (0:4)/4)
cs1Exp <- corExp(1, form = ~ x + y)
cs1Exp <- Initialize(cs1Exp, spatDat)
corMatrix(cs1Exp)
}

#pd for random
{
pdI <- pdIdent(0, form = ~Treatment-1, data = vecctest)
pdDiag(value=0:3, form = ~Treatment-1, data = vecctest)
pdCompSymm(value=c(0,1), form = ~Treatment-1, data = vecctest)
pdBlocked(list(
  pdIdent(0, form = ~variable-1, data = vecctest),
  pdDiag(value=0:3, form = ~Treatment-1, data = vecctest)
  )
)
cs1Symm <- corSymm(value = c(0.2, 0.1, -0.1, 0, 0.2, 0), form = ~ 1| Subject)
cs1Symm <- Initialize(cs1Symm, data = Orthodont)
corMatrix(cs1Symm)

cs1 <- corCAR1(0.2, form = ~ Time | Mare)

fm1Ovar.lme <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
                   data = Ovary, random = pdDiag(~sin(2*pi*Time)))
fm4Ovar.lme <- update(fm1Ovar.lme,
                      correlation = corCAR1(form = ~Time))
}

#CH1
{
  #-*- R -*-
  
  library(nlme)
  pdf(file = 'ch01.pdf')
  options( width = 65, digits = 5 )
  options( contrasts = c(unordered = "contr.helmert", ordered = "contr.poly") )
  
  # Chapter 1    Linear Mixed-Effects Models: Basic Concepts and Examples
  
  # 1.1 A Simple Example of Random Effects
  
  Rail
  fm1Rail.lm <- lm( travel ~ 1, data = Rail )
  fm1Rail.lm
  fm2Rail.lm <- lm( travel ~ Rail - 1, data = Rail )
  fm2Rail.lm
  fm1Rail.lme <- lme(travel ~ 1, data = Rail, random = ~ 1 | Rail)
  summary( fm1Rail.lme )
  fm1Rail.lmeML <- update( fm1Rail.lme, method = "ML" )
  summary( fm1Rail.lmeML )
  plot( fm1Rail.lme )   # produces Figure 1.4
  intervals( fm1Rail.lme )
  anova( fm1Rail.lme )
  
  # 1.2 A Randomized Block Design
  
  plot.design( ergoStool )   # produces Figure 1.6
  contrasts( ergoStool$Type )
  ergoStool1 <- ergoStool[ ergoStool$Subject == "1", ]
  model.matrix( effort ~ Type, ergoStool1 )   # X matrix for Subject 1
  fm1Stool <-
    lme(effort ~ Type, data = ergoStool, random = ~ 1 | Subject)
  summary( fm1Stool )
  anova( fm1Stool )
  options( contrasts = c( factor = "contr.treatment",
                          ordered = "contr.poly" ) )
  contrasts( ergoStool$Type )
  fm2Stool <-
    lme(effort ~ Type, data = ergoStool, random = ~ 1 | Subject)
  summary( fm2Stool )
  anova( fm2Stool )
  model.matrix( effort ~ Type - 1, ergoStool1 )
  fm3Stool <-
    lme(effort ~ Type - 1, data = ergoStool, random = ~ 1 | Subject)
  summary( fm3Stool )
  anova( fm3Stool )
  intervals( fm1Stool )
  plot( fm1Stool,   # produces Figure 1.8
        form = resid(., type = "p") ~ fitted(.) | Subject,
        abline = 0 )
  
  # 1.3  Mixed-effects Models for Replicated, Blocked Designs
  
  plot(Machines)
  with(Machines, interaction.plot( Machine, Worker, score))#, las = 1))   # Figure 1.10
  fm1Machine <-
    lme( score ~ Machine, data = Machines, random = ~ 1 | Worker )
  fm1Machine
  fm2Machine <- update( fm1Machine, random = ~ 1 | Worker/Machine )
  fm2Machine
  anova( fm1Machine, fm2Machine )
  ## delete selected rows from the Machines data
  MachinesUnbal <- Machines[ -c(2,3,6,8,9,12,19,20,27,33), ]
  ## check that the result is indeed unbalanced
  table(MachinesUnbal$Machine, MachinesUnbal$Worker)
  fm1MachinesU <- lme( score ~ Machine, data = MachinesUnbal,
                       random = ~ 1 | Worker/Machine )
  fm1MachinesU
  intervals( fm1MachinesU )
  fm4Stool <- lme( effort ~ Type, ergoStool, ~ 1 | Subject/Type )
  if (interactive()) intervals( fm4Stool )
  (fm1Stool$sigma)^2
  (fm4Stool$sigma)^2 + 0.79621^2
  Machine1 <- Machines[ Machines$Worker == "1", ]
  model.matrix( score ~ Machine, Machine1 )   # fixed-effects X_i
  model.matrix( ~ Machine - 1, Machine1 )   # random-effects Z_i
  fm3Machine <- update( fm1Machine, random = ~Machine - 1 |Worker)
  summary( fm3Machine )
  anova( fm1Machine, fm2Machine, fm3Machine )
  
  # 1.4 An Analysis of Covariance Model
  
  plot(Orthodont)
  names( Orthodont )
  levels( Orthodont$Sex )
  OrthoFem <- Orthodont[ Orthodont$Sex == "Female", ]
  fm1OrthF.lis <- lmList( distance ~ age, data = OrthoFem )
  coef( fm1OrthF.lis )
  intervals( fm1OrthF.lis )
  plot( intervals ( fm1OrthF.lis ) )   # produces Figure 1.12
  fm2OrthF.lis <- update( fm1OrthF.lis, distance ~ I( age - 11 ) )
  plot( intervals( fm2OrthF.lis ) )    # produces Figure 1.13
  fm1OrthF <-
    lme( distance ~ age, data = OrthoFem, random = ~ 1 | Subject )
  summary( fm1OrthF )
  fm1OrthFM <- update( fm1OrthF, method = "ML" )
  summary( fm1OrthFM )
  fm2OrthF <- update( fm1OrthF, random = ~ age | Subject )
  anova( fm1OrthF, fm2OrthF )
  random.effects( fm1OrthF )
  ranef( fm1OrthFM )
  coef( fm1OrthF )
  plot( compareFits(coef(fm1OrthF), coef(fm1OrthFM)))   # Figure 1.15
  plot( augPred(fm2OrthF), aspect = "xy", grid = TRUE )   # Figure 1.16
  
  # 1.5  Models for Nested Classification Factors
  
  fm1Pixel <- lme( pixel ~ day + I(day^2), data = Pixel,
                   random = list( Dog = ~ day, Side = ~ 1 ) )
  plot(Pixel)
  intervals( fm1Pixel )
  plot( augPred( fm1Pixel ) )   # produces Figure 1.18
  VarCorr( fm1Pixel )
  summary( fm1Pixel )
  fm2Pixel <- update( fm1Pixel, random = ~ day | Dog)
  anova( fm1Pixel, fm2Pixel )
  fm3Pixel <- update( fm1Pixel, random = ~ 1 | Dog/Side )
  anova( fm1Pixel, fm3Pixel )
  fm4Pixel <- update( fm1Pixel, pixel ~ day + I(day^2) + Side )
  summary( fm4Pixel )
  
  # 1.6  A Split-Plot Experiment
  
  plot(Oats)
  fm1Oats <- lme( yield ~ ordered(nitro) * Variety, data = Oats,
                  random = ~ 1 | Block/Variety )
  anova( fm1Oats )
  fm2Oats <- update( fm1Oats, yield ~ ordered(nitro) + Variety )
  anova( fm2Oats )
  summary( fm2Oats )
  fm3Oats <- update( fm1Oats, yield ~ ordered( nitro ) )
  summary( fm3Oats )
  fm4Oats <-
    lme( yield ~ nitro, data = Oats, random = ~ 1 | Block/Variety )
  summary( fm4Oats )
  VarCorr( fm4Oats )
  intervals( fm4Oats )
  plot(augPred(fm4Oats), aspect = 2.5, layout = c(6, 3),
       between = list(x = c(0, 0, 0.5, 0, 0))) # produces Figure 1.21
  
  # cleanup
  
  proc.time()
  
}
#CH! ex
{
  #1
  require(SASmixed)
  plot(PBIB)
  plot.design(PBIB)

  mPBIB <- lme(response~Treatment, random = ~1|Block, data=PBIB)
  anova(mPBIB)
  plot(mPBIB)
  plot(mPBIB, form= resid(.)~fitted(.)|Block)
  
  #2
  str(Oxboys)
  plot(Oxboys)
  lmOx <- lm(height~age, data=Oxboys)
  plot(lmOx)
  require(lattice)
  bwplot(Subject ~ resid(lmOx), Oxboys)
  lmOx2 <- lmList(height~age|Subject, Oxboys)
  plot(lmOx2, Subject~resid(.))
  summary(lmOx)
  summary(lmOx2)  
  intervals(lmOx2)
  
  lmOxMix <- lme(height~age, random=~1|Subject, data=Oxboys)
  summary(lmOxMix)
  plot(lmOxMix, Subject~resid(.))
  plot(augPred(lmOxMix))
  
  lmOxMix2 <- lme(height~age, random=~1+age|Subject, data=Oxboys)
  summary(lmOxMix2)
  plot(lmOxMix2, Subject~resid(.))
  plot(augPred(lmOxMix2))
  anova(lmOxMix, lmOxMix2)
  
  plot(lmOxMix)
  plot(lmOxMix2)
  qqnorm(lmOxMix)
  qqnorm(lmOxMix2)
  
  plot(augPred(lmOxMix2))
  plot(lmOxMix2, resid(.) ~ age | Subject)
  
  lmOxSq <- lmList(height~age+I(age^2), Oxboys) #trzeba pamiętać o I(.) !!!
  summary(lmOxSq)
  plot(lmOxSq, resid(.)~age|Subject)
  plot(intervals(lmOxSq))
  intervals(lmOxSq)
  
  lmOxMixSq <- lme(lmOxSq)
  summary(lmOxMixSq)
  plot(lmOxMixSq, resid(.)~age|Subject)
  
  #3
  str(Pixel)
  pxLm <- lmList(pixel~day+I(day^2), data=Pixel)
  fitted(pxLm) #cos nei dziala, index out of bound exception
  summary(pxLm)
  
  pxLm2 <- lmList(pixel~day+I(day^2) | Dog/Side, level = 2, data=Pixel)
  summary(pxLm2)  #as above
  pxLm2
  
  pxLmMix <- lme(pixel~day+I(day^2), random= ~1+day | Dog/Side, data=Pixel, control = lmeControl(maxIter = 50, opt="optim"))
  pxLmMix
  anova(pxLmMix, fm1Pixel)
  # diff in random effects: fm1Pixel <- lme( pixel ~ day + I(day^2), data = Pixel, random = list( Dog = ~ day, Side = ~ 1 ) )

  
  #4
  str(Alfalfa)
  plot(Alfalfa)
  lmAlf <- lme(Yield~Date*Variety, random=~1|Block/Variety, data=Alfalfa)
  summary(lmAlf)
  anova(lmAlf)
  
  aovAlf <- aov(Yield ~ Date * Variety + Error(Block/Variety), Alfalfa)
  summary(aovAlf)
  
  lmAlf2 <- update(lmAlf, Yield ~ Date)
  summary(lmAlf2)
  plot(lmAlf2)
  plot(lmAlf2, Date~resid(.)|Block:Variety)
  qqnorm(lmAlf2)
  
  
  }

#CH2
{
  #-*- R -*-
  
  library( nlme )
  options( width = 65, digits = 5 )
  options( contrasts = c(unordered = "contr.helmert",
                         ordered = "contr.poly") )
  pdf( file = 'ch02.pdf' )
  
  # Chapter 2    Theory and Computational Methods for Linear Mixed-Effects Models
  
  # 2.2   Likelihood Estimation for LME Models
  
  Xmat <- matrix( c(1, 1, 1, 1, 8, 10, 12, 14), ncol = 2 )
  Xmat
  Xqr <- qr( Xmat )               # creates a QR structure
  qr.R( Xqr )                     # returns R
  qr.Q( Xqr )                     # returns Q-truncated
  qr.Q( Xqr, complete = TRUE )    # returns the full Q
  
  fm1Rail.lme <- lme( travel ~ 1, data = Rail, random = ~ 1 | Rail,
                      control = list( msVerbose = TRUE ) )
  fm1Rail.lme <- lme( travel ~ 1, data = Rail, random = ~ 1 | Rail,
                      control = list( msVerbose = TRUE, niterEM = 0 ))
  
  fm1Machine <-
    lme( score ~ Machine, data = Machines, random = ~ 1 | Worker )
  fm2Machine <- update( fm1Machine, random = ~ 1 | Worker/Machine )
  anova( fm1Machine, fm2Machine )
  
  OrthoFem <- Orthodont[ Orthodont$Sex == "Female", ]
  fm1OrthF <- lme( distance ~ age, data = OrthoFem,
                   random = ~ 1 | Subject )
  fm2OrthF <- update( fm1OrthF, random = ~ age | Subject )
  orthLRTsim <- simulate.lme( fm1OrthF, m2 = fm2OrthF, nsim = 1000 )
  plot( orthLRTsim, df = c(1, 2) )    # produces Figure 2.3
  
  machineLRTsim <- simulate.lme(fm1Machine, m2 = fm2Machine, nsim= 1000)
  plot( machineLRTsim, df = c(0, 1),      # produces Figure 2.4
        layout = c(4,1), between = list(x = c(0, 0.5, 0)) )
  
  stoolLRTsim <-
    simulate.lme( list(fixed = effort ~ 1, data = ergoStool,
                       random = ~ 1 | Subject),
                  m2 = list(fixed = effort ~ Type),
                  method = "ML", nsim = 1000 )
  plot( stoolLRTsim, df = c(3, 4) )    # Figure 2.5
  data( PBIB, package = 'SASmixed' )
  pbibLRTsim <-
    simulate.lme(list( fixed = response ~ 1, data = PBIB,
                       random = ~ 1 | Block ),
                 m2 = list(fixed = response ~ Treatment, data = PBIB,
                           random = ~ 1 | Block),
                 method = "ML", nsim = 1000 )
  plot( pbibLRTsim, df = c(14,16,18), weights = FALSE )    # Figure 2.6
  
  summary( fm2Machine )
  
  fm1PBIB <- lme(response ~ Treatment, data = PBIB, random = ~ 1 | Block)
  anova( fm1PBIB )
  fm2PBIB <- update( fm1PBIB, method = "ML" )
  fm3PBIB <- update( fm2PBIB, response ~ 1 )
  anova( fm2PBIB, fm3PBIB )
  anova( fm2Machine )
  
  anova(fm2Oats)
  # cleanup
  
  proc.time() 
  
  fm4Oats
  fm4Oats2 <- update(fm4Oats, random=~Block, control = lmeControl(maxIter = 50, opt="optim"))
  oatsSim <-
    simulate.lme(fm4Oats2, m2 = fm4Oats, method = "ML", nsim = 1000 )
  plot( oatsSim, df = c(0,1))    # Figure 2.6
  
}

#CH3
{
  # Chapter 3    Describing the Structure of Grouped Data
  
  # 3.1 The Display Formula and Its Components
  
  formula( Rail )
  formula( ergoStool )
  formula( Machines )
  formula( Orthodont )
  formula( Pixel )
  formula( Oats )
  table( Oxboys$Subject )
  getGroups(Oxboys)
  table( getGroups( Oxboys ) )
  unique( table( getGroups( Oxboys ) ) )  # a more concise result
  unique( table( getCovariate( Oxboys ), getGroups( Oxboys ) ) )
  isBalanced(Oxboys)
  length( unique( getCovariate( Oxboys ) ) )
  unique( getGroups(Pixel, level = 1) )
  unique( getGroups(Pixel, level = 2) )
  Pixel.groups <- getGroups( Pixel, level = 1:2 )
  class( Pixel.groups )
  names( Pixel.groups )
  unique( Pixel.groups[["Side"]] )
  formula( PBG )
  PBG.log <- update( PBG, formula = deltaBP ~ log(dose) | Rabbit )
  formula(PBG.log)
  unique( getCovariate(PBG.log) )
  unique( getCovariate(PBG) )
  
  # 3.2 Constructing groupedData Objects
  
  # The next line is not from the book.
  # It is added to ensure that the file is available
  
  write.table( Oxboys, "oxboys.dat" )
  
  Oxboys.frm <- read.table( "oxboys.dat", header = TRUE )
  class( Oxboys.frm )        # check the class of the result
  dim( Oxboys.frm )          # check the dimensions
  Oxboys <- groupedData( height ~ age | Subject,
                         data = read.table("oxboys.dat", header = TRUE),
                         labels = list(x = "Centered age", y = "Height"),
                         units = list(y = "(cm)") )
  Oxboys                     # display the object
  unique( getGroups( Oxboys ) )
  plot( BodyWeight)
  plot( BodyWeight, outer = ~ Diet, aspect = 3 )  # Figure 3.3
  plot( BodyWeight, outer = TRUE, aspect = 3 )
  plot(Soybean)
  plot( Soybean, outer = ~ Year * Variety )       # Figure 6.10
  plot( Soybean, outer = ~ Variety * Year )
  gsummary( BodyWeight, invar = TRUE )
  plot(PBG)
  plot( PBG, inner = ~ Treatment, scales = list(x = list(log = 2)))
  ergoStool.mat <- asTable( ergoStool )
  ergoStool.mat
  ergoStool.new <- balancedGrouped( effort ~ Type | Subject,
                                    data = ergoStool.mat )
  ergoStool.new
  
  # 3.3 Controlling Trellis Graphics Presentations of Grouped Data
  
  plot(CO2)
  plot(CO2, layout=c(6,2), aspect=2, between=list(x=c(0,0,0.5,0,0)))
  plot(Spruce)
  plot( Spruce, layout = c(7, 4, 3),
        skip = c(rep(FALSE, 27), TRUE, rep(FALSE, 27), TRUE,
                 rep(FALSE, 12), rep(TRUE, 2), rep(FALSE,13)) )
  plot( Spruce, layout = c(9, 3, 3),
        skip = c(rep(FALSE, 66), TRUE, TRUE, rep(FALSE, 13)) )
  unique( getCovariate(DNase) )
  log( unique(getCovariate(DNase)), 2 )
  plot( DNase, layout=c(6,2), scales = list(x=list(log=2)) )
  plot(Pixel, layout = c(4,5),
       between = list(x = c(0, 0.5, 0), y = 0.5))
  plot( Pixel, displayLevel = 1 )
  plot( Wafer, display = 1, collapse = 1 )
  plot( Wafer, display = 1, collapse = 1,
        FUN = function(x) sqrt(var(x)), layout = c(10,1) )
  
  # 3.4 Summaries
  
  sapply( ergoStool, data.class )
  gsummary( Theoph, inv = TRUE )
  gsummary( Theoph, omit = TRUE, inv = TRUE )
  is.null(gsummary(Theoph, inv = TRUE, omit = TRUE)) # invariants present
  is.null(gsummary(Oxboys, inv = TRUE, omit = TRUE)) # no invariants
  gsummary( Theoph )
  gsummary( Theoph, FUN = max, omit = T)
  Quin.sum <- gsummary( Quinidine, omit = TRUE, FUN = mean )
  dim( Quin.sum )
  Quin.sum[1:10, ]
  Quinidine[Quinidine[["Subject"]] == 3, 1:8]
  Quin.sum1 <- gsummary( Quinidine, omit = TRUE )
  Quin.sum1[1:10, 1:7]
  summary( Quin.sum1 )
  summary( Quinidine )
  sum( ifelse(is.na(Quinidine[["conc"]]), 0, 1) )
  sum( !is.na(Quinidine[["conc"]]) )
  sum( !is.na(Quinidine[["dose"]]) )
  gapply( Quinidine, "conc", function(x) sum(!is.na(x)) )
  table( gapply(Quinidine, "conc", function(x) sum(!is.na(x))) )
  changeRecords <- gapply( Quinidine, FUN = function(frm)
    any(is.na(frm[["conc"]]) & is.na(frm[["dose"]])) )
  changeRecords
  table(changeRecords)
  sort( as.numeric( names(changeRecords)[changeRecords] ) )
  Quinidine[29:31,]
  Quinidine[Quinidine[["Subject"]] == 4, ]
  
  # cleanup
  
  proc.time()
}

#CH4
{
  #-*- R -*-
  
  # initialization
  
  library(nlme)
  library(lattice)
  options(width = 65, digits = 5)
  options(contrasts = c(unordered = "contr.helmert", ordered = "contr.poly"))
  pdf(file = 'ch04.pdf')
  
  # Chapter 4    Fitting Linear Mixed-Effects Models
  
  # 4.1 Fitting Linear Models in S with lm and lmList
  
  fm1Orth.lm <- lm(distance ~ age, Orthodont)
  fm1Orth.lm
  par(mfrow=c(2,2))
  plot(fm1Orth.lm)                               # Figure 4.1
  fm2Orth.lm <- update(fm1Orth.lm, formula = distance ~ Sex*age)
  summary(fm2Orth.lm)
  fm3Orth.lm <- update(fm2Orth.lm, formula = . ~ . - Sex)
  summary(fm3Orth.lm)
  bwplot(getGroups(Orthodont)~resid(fm2Orth.lm)) # Figure 4.2
  fm1Orth.lis <- lmList(distance ~ age | Subject, Orthodont)
  getGroupsFormula(Orthodont)
  fm1Orth.lis <- lmList(distance ~ age, Orthodont)
  formula(Orthodont)
  fm1Orth.lis <- lmList(Orthodont)
  fm1Orth.lis
  summary(fm1Orth.lis)
  pairs(fm1Orth.lis, id = 0.01, adj = -0.5)      # Figure 4.3
  fm2Orth.lis <- update(fm1Orth.lis, distance ~ I(age-11))
  pairs(fm2Orth.lis, id = 0.01, adj = -0.5)      # Figure 4.3
  intervals(fm2Orth.lis)
  plot(intervals(fm2Orth.lis))                   # Figure 4.5
  IGF
  plot(IGF)                                      # Figure 4.6
  fm1IGF.lis <- lmList(IGF)
  coef(fm1IGF.lis)
  plot(intervals(fm1IGF.lis))                    # Figure 4.7
  fm1IGF.lm <- lm(conc ~ age, data = IGF)
  summary(fm1IGF.lm)
  
  # 4.2 Fitting Linear Mixed-Effects Models with lme
  
  fm1Orth.lme <- lme(distance ~ I(age-11), data = Orthodont,
                     random = ~ I(age-11) | Subject)
  fm1Orth.lme <- lme(distance ~ I(age-11), data = Orthodont)
  fm1Orth.lme <- lme(fm2Orth.lis)
  fm1Orth.lme
  fm2Orth.lme <- update(fm1Orth.lme, distance~Sex*I(age-11))
  summary(fm2Orth.lme)
  fitted(fm2Orth.lme, level = 0:1)
  resid(fm2Orth.lme, level = 1)
  resid(fm2Orth.lme, level = 1, type = "pearson")
  newOrth <- data.frame(Subject = rep(c("M11","F03"), c(3, 3)),
                        Sex = rep(c("Male", "Female"), c(3, 3)),
                        age = rep(16:18, 2))
  predict(fm2Orth.lme, newdata = newOrth)
  predict(fm2Orth.lme, newdata = newOrth, level = 0:1)
  fm2Orth.lmeM <- update(fm2Orth.lme, method = "ML")
  summary(fm2Orth.lmeM)
  compOrth <-
    compareFits(coef(fm2Orth.lis), coef(fm1Orth.lme))
  compOrth
  
  plot(compOrth, mark = fixef(fm1Orth.lme)) # Figure 4.8
  ## Figure 4.9
  plot(comparePred(fm2Orth.lis, fm1Orth.lme, length.out = 2),
       layout = c(8,4), between = list(y = c(0, 0.5, 0)))
  plot(compareFits(ranef(fm2Orth.lme), ranef(fm2Orth.lmeM)),
       mark = c(0, 0))
  fm4Orth.lm <- lm(distance ~ Sex * I(age-11), Orthodont)
  summary(fm4Orth.lm)
  anova(fm2Orth.lme, fm4Orth.lm)
  
  
  fm1IGF.lme <- lme(fm1IGF.lis, control = lmeControl(opt="optim"))
  fm1IGF.lme
  intervals(fm1IGF.lme)
  summary(fm1IGF.lme)
  plot(compareFits(coef(fm1IGF.lis), coef(fm1IGF.lme)))
  formula(fm1IGF.lme)
  
  pd1 <- pdDiag(~ age)
  pd1
  formula(pd1)
  #fm2IGF.lme <- update(fm1IGF.lme, random = pdDiag(~age))
  (fm2IGF.lme <- lme(conc ~ age, IGF,
                     random = pdDiag(~age)))
  anova(fm1IGF.lme, fm2IGF.lme)
  anova(fm2IGF.lme)
  update(fm1IGF.lme, random = list(Lot = pdDiag(~ age))) # NAMED LIST - to say random struct explicitely!
  pd2 <- pdDiag(value = diag(2), form = ~ age)
  pd2
  formula(pd2)
  lme(conc ~ age, IGF, pdDiag(diag(2), ~age))
  
  fm4OatsB <- lme(yield ~ nitro, data = Oats,
                  random =list(Block = pdCompSymm(~ Variety - 1)))
  summary(fm4OatsB)
  summary(fm4Oats)
  corMatrix(fm4OatsB$modelStruct$reStruct$Block)
  fm4OatsC <- lme(yield ~ nitro, data = Oats,
                  random=list(Block=pdBlocked(list(pdIdent(~ 1),
                                                   pdIdent(~ Variety-1)))))
  summary(fm4OatsC)
  ## establishing the desired parameterization for contrasts
  options(contrasts = c("contr.treatment", "contr.poly"))
  
  Assay
  plot(Assay)
  str(Assay)
  summary(Assay)
  plot.design(Assay)
  fm1Assay <- lme(logDens ~ sample * dilut, Assay,
                  random = pdBlocked(list(pdIdent(~ 1), 
                                          pdIdent(~ sample - 1),
                                          pdIdent(~ dilut - 1))))
  fm1Assay
  anova(fm1Assay)
  
  formula(Oxide)
  plot(Oxide)
  fm1Oxide <- lme(Thickness ~ 1, Oxide) # already includes: "random = ~ 1 | Lot/Wafer" or "random = list( Lot = ~ 1, Wafer = ~ 1 )"
  fm1Oxide
  intervals(fm1Oxide, which = "var-cov")
  fm2Oxide <- update(fm1Oxide, random = ~ 1 | Lot)
  fm2Oxide
  anova(fm1Oxide, fm2Oxide)
  coef(fm1Oxide, level = 1)
  coef(fm1Oxide, level = 2)
  ranef(fm1Oxide, level = 1:2)
  
  fm1Wafer <- lme(current ~ voltage + I(voltage^2), data = Wafer,
                  random = list(Wafer = pdDiag(~voltage + I(voltage^2)), 
                                Site = pdDiag(~voltage + I(voltage^2)))) # == "random = pdDiag( ~ voltage + voltage^2" bo ~1|Wafer/Site zadeklarowane w GroupedData
  summary(fm1Wafer)
  fitted(fm1Wafer, level = 0)
  resid(fm1Wafer, level = 1:2)
  newWafer <-
    data.frame(Wafer = rep(1, 4), voltage = c(1, 1.5, 3, 3.5))
  predict(fm1Wafer, newWafer, level = 0:1)
  newWafer2 <- data.frame(Wafer = rep(1, 4), Site = rep(3, 4),
                          voltage = c(1, 1.5, 3, 3.5))
  predict(fm1Wafer, newWafer2, level = 0:2)
  
  # 4.3 Examining a Fitted Model
  
  plot(fm2Orth.lme, Subject~resid(.), abline = 0) # fig. 4.16
  plot(fm2Orth.lme, resid(., type = "p") ~ fitted(.) | Sex,
       id = 0.05, adj = -0.3)
  fm3Orth.lme <-
    update(fm2Orth.lme, weights = varIdent(form = ~ 1 | Sex))
  fm3Orth.lme
  fm2Orth.lme
  plot(fm3Orth.lme, distance ~ fitted(.),
       id = 0.05, adj = -0.3)
  anova(fm2Orth.lme, fm3Orth.lme)
  qqnorm(fm3Orth.lme, ~resid(.) | Sex)
  
  plot(fm2IGF.lme, resid(., type = "p") ~ fitted(.) | Lot,
       layout = c(5,2))
  qqnorm(fm2IGF.lme, ~ resid(.), id = 0.05, adj = -0.75)
  
  plot(fm1Oxide)
  qqnorm(fm1Oxide)
  
  qqnorm(fm1Wafer)
  plot(fm1Wafer, resid(.) ~ voltage | Wafer)
  plot(fm1Wafer, resid(.) ~ voltage | Wafer,
       panel = function(x, y, ...) {
         panel.grid()
         panel.xyplot(x, y)
         panel.loess(x, y, lty = 2)
         panel.abline(0, 0)
       })
  with(Wafer,
       coef(lm(resid(fm1Wafer) ~ cos(4.19*voltage)+sin(4.19*voltage)-1)))
  nls(resid(fm1Wafer) ~ b3*cos(w*voltage) + b4*sin(w*voltage), Wafer,
      start = list(b3 = -0.0519, b4 = 0.1304, w = 4.19))
  fm2Wafer <- update(fm1Wafer,
                     . ~ . + cos(4.5679*voltage) + sin(4.5679*voltage),
                     random = list(Wafer=pdDiag(~voltage+I(voltage^2)),
                                   Site=pdDiag(~voltage+I(voltage^2))))
  summary(fm2Wafer)
  intervals(fm2Wafer)
  qqnorm(fm2Wafer)
  plot(fm2Wafer, resid(.) ~ voltage | Wafer)
  
  # 4.3.2 Assessing Assumptions on the Random Effects
  
  qqnorm(fm2Orth.lme, ~ranef(.), id = 0.10, cex = 0.7)
  pairs(fm2Orth.lme, ~ranef(.) | Sex,
        id = ~ Subject == "M13", adj = -0.3) # fig. 4.30
  qqnorm(fm3Orth.lme, ~ranef(.), id = 0.10, cex = 0.7)
  pairs(fm3Orth.lme, ~ranef(.) | Sex,
        id = ~ Subject == "M13", adj = -0.3) # fig. 4.30
  
  fm2IGF.lme
  c(0.00031074, 0.0053722)/abs(fixef(fm2IGF.lme))
  fm3IGF.lme <- update(fm2IGF.lme, random = ~ age - 1)
  anova(fm2IGF.lme, fm3IGF.lme)
  
  qqnorm(fm1Oxide, ~ranef(., level = 1), id=0.10) #fig. 4.33
  qqnorm(fm1Oxide, ~ranef(., level = 2), id=0.10)
  
  fm2Wafer
  pairs(fm2Wafer, ~ranef(., level=1), adj = -0.3) # fig. 4.35
  fm3Wafer <- update(fm2Wafer,
                random = list(Wafer = ~voltage+I(voltage^2),
                              Site = pdDiag(~voltage+I(voltage^2))),
                     control = list(msVerbose = TRUE, msMaxIter = 200, opt="optim")
                     )
  fm3Wafer
  anova(fm2Wafer, fm3Wafer)
  pairs(fm3Wafer, ~ranef(., level=2), adj = -0.3) # fig. 4.35
  fm4Wafer <- update(fm2Wafer,
                     random = list(Wafer = ~ voltage + I(voltage^2),
                     Site = pdBlocked(list(~1,
                     ~voltage+I(voltage^2) - 1))),
                     control = list(msVerbose = TRUE, msMaxIter = 200, opt="optim"))
  fm4Wafer
  anova(fm3Wafer, fm4Wafer)
  qqnorm(fm4Wafer, ~ranef(., level = 2), id = 0.05, # fig. 4.37
          cex = 0.7, layout = c(3, 1))
  pairs(fm4Wafer, ~ranef(., level=2), adj = -0.3) 
  
  # The next line is not in the book but is needed to get fm1Machine
  
  # ex1
  fm1Machine <-
    lme(score ~ Machine, data = Machines, random = ~ 1 | Worker)
  
  fm3Machine <- update(fm1Machine, random = ~Machine-1|Worker)
  fm3Machine
  fm4Machine <- update(fm3Machine, random = pdCompSymm(~Machine-1))
  fm4Machine
  
  qqnorm(fm3Machine, ~ranef(.), id = 0.05, cex = 0.7)
  qqnorm(fm4Machine, ~ranef(.), id = 0.05, cex = 0.7)
  pairs(fm3Machine, ~ranef(.), adj = -0.3) 
  pairs(fm4Machine, ~ranef(.), adj = -0.3) 
  plot(fm3Machine)
  plot(fm4Machine)
  anova(fm3Machine, fm4Machine)
  anova(fm2Machine, fm4Machine)
  
  # ex2
  str(Orthodont)
  #TODO
  
  # ex3
  plot(Assay)
  str(Assay)
  fm1Assay
  formula(fm1Assay)
  anova(fm1Assay)
  
  fm2Assay <- update(fm1Assay, fixed = logDens ~ sample + dilut)
  fm2Assay
  anova(fm1Assay, fm2Assay)
  fm3Assay <- update(fm2Assay, random = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1))))
  fm3Assay
  fm3bAssay <- update(fm2Assay, random = pdCompSymm(~ sample - 1))
  fm3bAssay
  fm3cAssay <- update(fm2Assay, random = ~ 1|Block/sample)
  fm3cAssay
  anova(fm2Assay, fm3Assay)
  anova(fm2Assay, fm3bAssay)
  anova(fm2Assay, fm3cAssay)
  
  fm4Assay <- update(fm2Assay, random = pdIdent(~1))
  fm4Assay
  VarCorr(fm4Assay)
  ranef(fm4Assay)
  anova(fm3Assay, fm4Assay)
  
  fm5Assay <- lm(fm4Assay, data = Assay)
  summary(fm5Assay)
  anova(fm4Assay, fm5Assay)
    
}

#CH5
{
  #-*- R -*-
  
  # initialization
  
  library(nlme)
  options(width = 65, digits = 5)
  options(contrasts = c(unordered = "contr.helmert", ordered = "contr.poly"))
  pdf(file = "ch05.pdf")
  
  # Chapter 5    Extending the Basic Linear Mixed-Effects Models
  
  # 5.1 General Formulation of the Extended Model
  
  vf1Fixed <- varFixed(~ age)
  vf1Fixed <- Initialize(vf1Fixed, data = Orthodont)
  varWeights(vf1Fixed)
  vf1Ident <- varIdent(c(Female = 0.5), ~ 1 | Sex)
  vf1Ident <- Initialize(vf1Ident, Orthodont)
  varWeights(vf1Ident)
  vf2Ident <- varIdent(form =  ~ 1 | Sex, fixed = c(Female = 0.5))
  vf2Ident <- Initialize(vf2Ident, Orthodont)
  varWeights(vf2Ident)
  vf3Ident <- varIdent(form = ~ 1 | Sex * age)
  vf3Ident <- Initialize(vf3Ident, Orthodont)
  varWeights(vf3Ident)
  vf1Power <- varPower(1)
  formula(vf1Power)
  vf2Power <- varPower(fixed = 0.5)
  vf3Power <- varPower(form = ~ fitted(.) | Sex,
                       fixed = list(Male = 0.5, Female = 0))
  vf1Exp <- varExp(form = ~ age | Sex, fixed = c(Female = 0))
  vf1ConstPower <- varConstPower(power = 0.5,
                                 fixed = list(const = 1))
  vf1Comb <- varComb(varIdent(c(Female = 0.5), ~ 1 | Sex),
                     varExp(1, ~ age))
  vf1Comb <- Initialize(vf1Comb, Orthodont)
  varWeights(vf1Comb)
  
  fm1Dial.lme <-
    lme(rate ~(pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB,
        Dialyzer, ~ pressure + I(pressure^2))
  fm1Dial.lme
  plot(fm1Dial.lme, resid(.) ~ pressure, abline = 0)
  fm2Dial.lme <- update(fm1Dial.lme,
                        weights = varPower(form = ~ pressure))
  fm2Dial.lme
  anova(fm1Dial.lme, fm2Dial.lme)
  plot(fm2Dial.lme, resid(., type = "p") ~ pressure,
       abline = 0)
  intervals(fm2Dial.lme)
  plot(fm2Dial.lme, resid(.) ~ pressure|QB, abline = 0)
  fm3Dial.lme <- update(fm2Dial.lme,
                        weights=varPower(form = ~ pressure | QB))
  fm3Dial.lme
  anova(fm2Dial.lme, fm3Dial.lme)
  fm4Dial.lme <- update(fm2Dial.lme,
                        weights = varConstPower(form = ~ pressure))
  anova(fm2Dial.lme, fm4Dial.lme)
  plot(red(fm2Dial.lme), grid = TRUE)
  anova(fm2Dial.lme)
  anova(fm2Dial.lme, Terms = 8:10)
  
  options(contrasts = c("contr.treatment", "contr.poly"))
  fm1BW.lme <- lme(weight ~ Time * Diet, BodyWeight,
                   random = ~ Time)
  fm1BW.lme
  fm2BW.lme <- update(fm1BW.lme, weights = varPower())
  fm2BW.lme
  anova(fm1BW.lme, fm2BW.lme)
  summary(fm2BW.lme)
  anova(fm2BW.lme, L = c("Time:Diet2" = 1, "Time:Diet3" = -1))
  
  cs1CompSymm <- corCompSymm(value = 0.3, form = ~ 1 | Subject)
  cs2CompSymm <- corCompSymm(value = 0.3, form = ~ age | Subject)
  cs1CompSymm <- Initialize(cs1CompSymm, data = Orthodont)
  corMatrix(cs1CompSymm)
  cs1Symm <- corSymm(value = c(0.2, 0.1, -0.1, 0, 0.2, 0),
                     form = ~ 1 | Subject)
  cs1Symm <- Initialize(cs1Symm, data = Orthodont)
  corMatrix(cs1Symm)
  cs1AR1 <- corAR1(0.8, form = ~ 1 | Subject)
  cs1AR1 <- Initialize(cs1AR1, data = Orthodont)
  corMatrix(cs1AR1)
  cs1ARMA <- corARMA(0.4, form = ~ 1 | Subject, q = 1)
  cs1ARMA <- Initialize(cs1ARMA, data = Orthodont)
  corMatrix(cs1ARMA)
  cs2ARMA <- corARMA(c(0.8, 0.4), form = ~ 1 | Subject, p=1, q=1)
  cs2ARMA <- Initialize(cs2ARMA, data = Orthodont)
  corMatrix(cs2ARMA)
  spatDat <- data.frame(x = (0:4)/4, y = (0:4)/4)
  cs1Exp <- corExp(1, form = ~ x + y)
  cs1Exp <- Initialize(cs1Exp, spatDat)
  corMatrix(cs1Exp)
  cs2Exp <- corExp(1, form = ~ x + y, metric = "man")
  cs2Exp <- Initialize(cs2Exp, spatDat)
  corMatrix(cs2Exp)
  cs3Exp <- corExp(c(1, 0.2), form = ~ x + y, nugget = TRUE)
  cs3Exp <- Initialize(cs3Exp, spatDat)
  corMatrix(cs3Exp)
  fm1Ovar.lme <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
                     data = Ovary, random = pdDiag(~sin(2*pi*Time)))
  fm1Ovar.lme
  ACF(fm1Ovar.lme)
  plot(ACF(fm1Ovar.lme,  maxLag = 10), alpha = 0.01)
  fm2Ovar.lme <- update(fm1Ovar.lme, correlation = corAR1())
  anova(fm1Ovar.lme, fm2Ovar.lme)
  if (interactive()) intervals(fm2Ovar.lme)
  fm3Ovar.lme <- update(fm1Ovar.lme, correlation = corARMA(q = 2))
  fm3Ovar.lme
  anova(fm2Ovar.lme, fm3Ovar.lme, test = F)
  fm4Ovar.lme <- update(fm1Ovar.lme,
                        correlation = corCAR1(form = ~Time))
  anova(fm2Ovar.lme, fm4Ovar.lme, test = F)
  (fm5Ovar.lme <- update(fm1Ovar.lme,
                         corr = corARMA(p = 1, q = 1)))
  anova(fm2Ovar.lme, fm5Ovar.lme)
  plot(ACF(fm5Ovar.lme,  maxLag = 10, resType = "n"), alpha = 0.01)
  
  v <- Variogram(fm2BW.lme, form = ~ Time)
  plot(v$n.pairs ~ v$dist)
  plot(Variogram(fm2BW.lme, form = ~ Time, maxDist = 42))
  fm3BW.lme <- update(fm2BW.lme,
                      correlation = corExp(form = ~ Time))
  fm3BW.lme
  intervals(fm3BW.lme)
  anova(fm2BW.lme, fm3BW.lme)
  fm4BW.lme <-
    update(fm3BW.lme, correlation = corExp(form =  ~ Time,
                                           nugget = TRUE))
  fm4BW.lme
  anova(fm3BW.lme, fm4BW.lme)
  plot(Variogram(fm3BW.lme, form = ~ Time, maxDist = 42))
  plot(Variogram(fm3BW.lme, form = ~ Time, maxDist = 42,
                 resType = "n", robust = TRUE))
  fm5BW.lme <- update(fm3BW.lme, correlation = corRatio(form = ~ Time))
  fm6BW.lme <- update(fm3BW.lme, correlation = corSpher(form = ~ Time))
  fm7BW.lme <- update(fm3BW.lme, correlation = corLin(form = ~ Time))
  fm8BW.lme <- update(fm3BW.lme, correlation = corGaus(form = ~ Time))
  anova(fm3BW.lme, fm5BW.lme, fm6BW.lme, fm7BW.lme, fm8BW.lme)
  
  fm1Orth.gls <- gls(distance ~ Sex * I(age - 11), Orthodont,
                     correlation = corSymm(form = ~ 1 | Subject),
                     weights = varIdent(form = ~ 1 | age))
  fm1Orth.gls
  intervals(fm1Orth.gls)
  fm2Orth.gls <-
    update(fm1Orth.gls, corr = corCompSymm(form = ~ 1 | Subject))
  anova(fm1Orth.gls, fm2Orth.gls)
  intervals(fm2Orth.gls)
  fm3Orth.gls <- update(fm2Orth.gls, weights = NULL)
  anova(fm2Orth.gls, fm3Orth.gls)
  plot(fm3Orth.gls, resid(., type = "n") ~ age | Sex)
  fm4Orth.gls <- update(fm3Orth.gls,
                        weights = varIdent(form = ~ 1 | Sex))
  anova(fm3Orth.gls, fm4Orth.gls)
  qqnorm(fm4Orth.gls, ~resid(., type = "n"))
  # not in book but needed for the following command
  fm3Orth.lme <-
    lme(distance~Sex*I(age-11), data = Orthodont,
        random = ~ I(age-11) | Subject,
        weights = varIdent(form = ~ 1 | Sex))
  anova(fm3Orth.lme, fm4Orth.gls, test = FALSE)
  
  plot(Dialyzer)
  str(Dialyzer)
  fm1Dial.gls <-
    gls(rate ~(pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB,
        Dialyzer)
  plot(fm1Dial.gls, resid(.) ~ pressure, abline = 0)
  fm2Dial.gls <- update(fm1Dial.gls,
                        weights = varPower(form = ~ pressure))
  plot(fm2Dial.gls, resid(.) ~ pressure, abline = 0)
  anova(fm1Dial.gls, fm2Dial.gls)
  ACF(fm2Dial.gls, form = ~ 1 | Subject)
  plot(ACF(fm2Dial.gls, form = ~ 1 | Subject), alpha = 0.01)
  (fm3Dial.gls <- update(fm2Dial.gls,
                         corr = corAR1(0.771, form = ~ 1 | Subject)))
  intervals(fm3Dial.gls)
  plot(fm3Dial.gls, resid(.) ~ pressure, abline = 0)
  ACF(fm3Dial.gls)
  plot(ACF(fm3Dial.gls, form = ~ 1 | Subject), alpha = 0.01)
  anova(fm2Dial.gls, fm3Dial.gls)
  
  anova(fm3Dial.gls, fm2Dial.lme, test = FALSE)
  
  str(Wheat2)
  plot(Wheat2)
  summary(Wheat2)
  fm1Wheat2 <- gls(yield ~ variety - 1, Wheat2)
  fm1Wheat2
  Variogram(fm1Wheat2, form = ~ latitude + longitude)
  plot(Variogram(fm1Wheat2, form = ~ latitude + longitude,
                 maxDist = 32), xlim = c(0,32))
  fm2Wheat2 <- update(fm1Wheat2, corr = corSpher(c(28, 0.2),
                                                 form = ~ latitude + longitude,
                                                 nugget = TRUE))
  fm2Wheat2
  fm3Wheat2 <- update(fm1Wheat2,
                      corr = corRatio(c(12.5, 0.2),
                                      form = ~ latitude + longitude, nugget = TRUE))
  fm3Wheat2
  anova(fm2Wheat2, fm3Wheat2)
  anova(fm1Wheat2, fm3Wheat2)
  plot(Variogram(fm3Wheat2, resType = "n"))
  plot(fm3Wheat2, resid(., type = "n") ~ fitted(.), abline = 0)
  qqnorm(fm3Wheat2, ~ resid(., type = "n"))
  fm4Wheat2 <- update(fm3Wheat2, model = yield ~ variety)
  anova(fm4Wheat2)
  anova(fm3Wheat2, L = c(-1, 0, 1))
  
  # cleanup
  
  proc.time()
  
  # ex.1
  fm2BW.lme
  plot(fm2BW.lme, resid(.) ~ as.integer(Diet), abline = 0)
  fm3BW.lme <- update(fm2BW.lme, weights = varIdent(form = ~1|Diet))
  fm3BW.lme 
  plot(fm3BW.lme, resid(.) ~ as.integer(Diet), abline = 0)
  intervals(fm3BW.lme, which="var-cov")
  anova(fm1BW.lme, fm3BW.lme)
  anova(fm2BW.lme, fm3BW.lme)
  fm1BW.gls <- gls(weight ~ Time * Diet, BodyWeight, 
                   weights = varPower(), 
                   correlation = corCAR1(form = ~1|Time))
  fm3BW.lme
  anova(fm3BW.lme, fm1BW.gls)
  
  
  # ex.3 Oats
  str(Oats)
  fm4Oats; VarCorr(fm4Oats)
  fm4OatsB; VarCorr(fm4OatsB)
  fm4OatsC; VarCorr(fm4OatsC)
  fm5Oats <- update(fm4Oats, random = ~1|Block, corr = corSymm(form = ~1|Block/Variety))
  fm5Oats; VarCorr(fm5Oats)
  ranef(fm5Oats)
  intervals(fm5Oats, which = "var-cov")
  fm6Oats <- lme(yield~nitro, data=Oats, random = ~1|Block, 
                 correlation = corCompSymm(form = ~1|Block/Variety))
  fm6Oats; VarCorr(fm6Oats)
  intervals(fm6Oats)
  anova(fm6Oats, fm5Oats)
  anova(fm6Oats, fm4Oats)
  anova(fm6Oats, fm4OatsB)
  fm4Oats
  fm6Oats
  
  VarCorr(fm6Oats)
  corMatrix(fm6Oats$modelStruct$corStruct)
  coef(fm6Oats$modelStruct$corStruct, unconstrained = F)
  VarCorr(fm4Oats)
  VarCorr(fm4OatsB)
  VarCorr(fm4OatsC)
  corMatrix(fm4Oats$modelStruct$corStruct)
  
  #fm4::sigma2^2 = fm6::cor * fm6::var
  coef(fm6Oats$modelStruct$corStruct, unconstrained = F) #cor
  VarCorr(fm6Oats) #var
  as.numeric(VarCorr(fm6Oats)[2,1])*coef(fm6Oats$modelStruct$corStruct, unconstrained = F) #var zagniezdzonego
  

                           
  
  
  # 5.3 Correlation - bylo też wyżej
  {
  str(Ovary)
  plot(Ovary)
  fm1Ovar.lme <- lme( follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
                      data = Ovary, 
                      random = pdDiag(~sin(2*pi*Time)) )
  fm1Ovar.lme
  VarCorr(fm1Ovar.lme)
  plot(fm1Ovar.lme)
  plot(fm1Ovar.lme, resid(.)~ follicles)
  ACF(fm1Ovar.lme)
  plot(ACF(fm1Ovar.lme, maxLag = 10), alpha = 0.01)
  
  fm2Ovar.lme <- update( fm1Ovar.lme, correlation = corAR1() )
  fm2Ovar.lme
  plot(fm2Ovar.lme)
  plot(fm2Ovar.lme, resid(.)~ follicles)
  ACF(fm2Ovar.lme)
  plot(ACF(fm2Ovar.lme, maxLag = 10), alpha = 0.01)
  
  anova( fm1Ovar.lme, fm2Ovar.lme )
  intervals( fm2Ovar.lme )
  
  fm3Ovar.lme <- update(fm1Ovar.lme, correlation = corARMA(q = 2))
  fm3Ovar.lme
  anova( fm2Ovar.lme, fm3Ovar.lme, test=F ) # not nested, cannot be compared through a likelihood ratio test. They can, however, be compared via their information criterion statistics.
  ACF(fm3Ovar.lme)
  plot(ACF(fm3Ovar.lme, maxLag = 10), alpha = 0.01)
  
  fm4Ovar.lme <- update( fm1Ovar.lme,
                         correlation = corCAR1(form = ~Time) )
  fm4Ovar.lme
  anova( fm2Ovar.lme, fm4Ovar.lme, test = F ) # fm2 lepszy
  
  fm5Ovar.lme <- update(fm1Ovar.lme, corr = corARMA(p = 1, q = 1))
  fm5Ovar.lme
  anova( fm2Ovar.lme, fm5Ovar.lme ) #AR(1) model is nested within the ARMA(1, 1) model (corresponding to θ1 = 0)
  plot( ACF(fm5Ovar.lme, maxLag = 10, resType = "n"), alpha = 0.01 ) # Figure 5.12
  
  # Body Weight Growth in Rats
  
  Variogram( fm2BW.lme, form = ~ Time )
  }
  
}
