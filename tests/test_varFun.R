#tests
{
  set.seed(100)
value = rnorm(100)
time = factor(rep(1:5, each = 20))
id = rep(letters[1:20],5)
block = rep(letters[21:25],4)
data = data.frame(value, time, id, block)

require(nlme)
Fit1 <- nlme::lme(value ~ time, random=~ 1 | id
                  , correlation=corCompSymm(form=~1|id)
                  ,data=data)
summary(Fit1)

Fit1a <- nlme::lme(value ~ time, random=~ 1 | block/id
                  , correlation=corCompSymm(form=~1|block/id)
                  ,data=data)
summary(Fit1a)
VarCorr(Fit1a)


Fit1b <- nlme::lme(value ~ time, random=list(~ 1 | block, ~1|id)
                   , correlation = list(corCompSymm(form=~1|block), corIdent(form=~1:id))
                   , data = data)
summary(Fit1b)
VarCorr(Fit1a)


Fit2 <- nlme::lme(value ~ time, random=~ 1 | id
                  ,correlation=NULL
                  ,data=data)
summary(Fit2)
VarCorr(Fit2)

Fit3 <- nlme::lme(value ~ time, random=~ 1 | id
                  ,correlation=corSymm(form=~1|id)
                  ,data=data)
summary(Fit3)
VarCorr(Fit3)

Fit4 <- nlme::lme(value ~ time, random=~ 1 | id
                  ,correlation=corAR1(form=~1|id)
                  ,data=data)
summary(Fit4)
VarCorr(Fit4)
ranef(Fit4)

Fit5 <- nlme::lme(value ~ time, random=~ 1 | id
                  ,correlation=corIdent(form=~1|id)
                  ,data=data)
summary(Fit5)
VarCorr(Fit5)
ranef(Fit5)

Fit6 <- nlme::lme(value ~ time, random=~ 1 | id
                  ,correlation=corARMA(form=~1|id, p=1, q=2),
                  ,data=data)
summary(Fit6)

Fit6 <- nlme::lme(value ~ time, random=~ 1 | id
                  ,correlation=corLin(form=~1|id),
                  ,data=data)
summary(Fit6)





ex <- dget('ex1')
str(ex)

require(nlme)

lmex <- lme(level~methods, random=~1|day/methods, data=ex)
summary(lmex)
lmex2 <- lme(level~methods, random=~methods|day, data=ex)
summary(lmex2)
lmex3 <- lme(level~methods, random=~methods|day, data=ex, corr=NULL) # same as above
summary(lmex3)

require(lme4)
lmex4 <- lmer(level~methods|day, data=ex)
summary(lmex4)
lmex5 <- lmer(level~methods+ 1|day, data=ex) # same as above
summary(lmex5)

lmex6 <- lmer(level~0+methods|day, data=ex)
summary(lmex6)
lmex7 <- lmer(level~0+methods+ 1|day, data=ex) 
summary(lmex7)


##### test varFun

exg <- groupedData(data = ex, formula = level ~ 1 | methods/day)
tmp <- lme(level~1, data=exg, weights=varIdent(form = ~1|methods))
tmp
plot(resid(tmp))

tmp2 <- lme(level~1, data=exg, weights=varIdent(form = ~1|day))
tmp2
plot(resid(tmp2))

tmp3 <- lme(level~1, data=exg, weights=varComb(varIdent(form = ~1|methods), varIdent(form=~1|day)))
tmp3

tmp4 <- lme(level~-1+methods, data=exg, weights=varIdent(form = ~1|day))
plot(tmp4)

tmp5 <- lme(level~-1+methods, data=exg)
tmp5
plot(tmp5)

plot(cbind(resid(tmp4),resid(tmp5)), col=rainbow(length(resid(tmp4))))
abline(0,1)

plot(cbind(1:10,1:10), col=rainbow(length(resid(tmp4))))

tmp$modelStruct$reStruct
tmp$modelStruct$varStruct
resid(tmp)
fitted(tmp)
coef(tmp)
plot(resid(tmp))
}

#polapgen

setwd(dir = "Code/R_oom/")
dat <- read.table("dane_wielowymiarowe_polapgen/przyklad_polapgen_metabo_2.txt", header = T, sep="\t", dec = ",")
head(dat)
#str(dat), dim(dat)

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
