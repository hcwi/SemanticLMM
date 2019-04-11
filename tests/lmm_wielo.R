setwd(dir = "Code/R_oom/")
dat <- read.table("dane_wielo.txt", header = T)
dat[,5] <- as.factor(dat[,5])
dat[,6] <- as.factor(dat[,6])
str(dat)

library(reshape2)
vecc <- melt(dat)

#####
library(lme4)
mod <- lmer(value ~ 0+Fact:variable + (1|Block:variable), data = vecc)
summary(mod)

anova(mod)
confint(mod)
fixef(mod) # Fixed effects estimators (BLUE)

require(emmeans)
ref_grid(mod)
emmeans(mod, ~Fact*variable)


ranef(mod) # Random effects predictors (BLUP)
vc <- VarCorr(mod)
vcFrame <- as.data.frame(vc)
vcFrame

#####

require(nlme)

gvecc <- groupedData(value ~ Fact:variable | Block, data = vecc)
mod <- lme(value ~ 0 + Fact:variable,
           random = ~ 1|variable/Block, 
           data = gvecc,
           correlation = corCompSymm(form = ~ 1|variable/Block)
           )
mod
mod <- lme(value ~ 0 + Fact:variable,
           random = ~ 1|variable/Block, 
           data = gvecc,
           correlation = corAR1(form = ~ 1|variable/Block)
)
mod

mod0 <- lme(value ~ 0 + Fact:variable,
           data = gvecc,
           random=pdBlocked(list(
             #pdIdent(~1),
             pdIdent(~Block-1),
             pdDiag(~variable-1)))
)
mod0

mod1 <- lme(value ~ 0 + Fact:variable,
            data = gvecc,
            random=pdBlocked(list(
              #pdIdent(~1),
              pdIdent(~Block-1),
              pdIdent(~variable-1)))
)
mod1

mod2 <- lme(value ~ 0 + Fact:variable,
            data = gvecc,
            random=pdBlocked(list(
              #pdIdent(~1),
              pdDiag(~Block:variable-1),
              pdDiag(~variable-1)))
)
mod2

anova(mod0, mod1)
anova(mod1, mod2)

#gls model
mg <- gls(value~variable + Fact:variable,
          data=gvecc,
          correlation=corCompSymm(form = ~variable|Block)#,
          #weight=varIdent(form=~1|variable)
          )
summary(mg)

ml <- lme(fixed=value~variable+Fact:variable, 
          random=pdCompSymm(~Block-1),
          data=gvecc
)
ranef(ml)
ml
summary(ml)
anova(mg,ml)

#adding diff variances per group
mlvar <- lme(fixed=value~variable+Fact:variable, 
          random=pdCompSymm(~Block-1),
          data=gvecc,
          weights=varIdent(form=~1|variable)
)
mlvar

##### Examples with RAIL data

data(Rail)
str(Rail)
#lset(col.whitebg())
plot(Rail)
m <- lme(travel~1,data=Rail,random=~1|Rail)
anova(m)
lme(travel 1,data=Rail) 
lme(Rail)
r1 <- lme(travel~1,data=Rail)
plot(r1)
plot(r1,resid(.)~fitted(.)|Rail)

lme(travel~1,data=Rail,method="ML")
lme(travel~1,data=Rail,method="REML") #bigger Rail effect than above

one <- rep(1,nrow(Rail))
lme(travel~1,
    random=list(one=pdIdent(~Rail-1)),
    data=Rail)


m0 <- lme(travel~1,data=Rail)
m1 <- lme(travel~1,data=Rail,weights=varIdent(form=~1|Rail))
m2 <- lme(travel~1,data=Rail,random = pdDiag(Rail~1))


V.d <- getVarCov(mod, type="marginal",individual=1)   # V_i
R.d <- getVarCov(mod, type="conditional",individual=1)   # R_i


mod2 <- update(mod, corr = corIdent())
mod2 <- update(mod, corr = corCompSymm(form = ~ 1 | Sex))
ranef(mod)
fixef(mod)

summary(mod)





###### Examples with ORTHODONT data

data(Orthodont)
OrthoFem <- subset(Orthodont, Sex=="Female")
plot(OrthoFem)
plot(intervals(lmList(distance ~ age, data=OrthoFem)))

of1 <- lme(distance ~ age, data=OrthoFem, random=~1|Subject)
of2 <- lme(distance ~ age, data=OrthoFem, random=~age|Subject)
anova(of1,of2)

plot(augPred(of2), grid=TRUE, aspect="xy")


data(Pixel)
px1 <- lme(pixel~day+I(day^2), data=Pixel, random=list(Dog=~day,Side=~1))
plot(augPred(px1))
px2 <- update(px1,random=~day|Dog) # == Dog=~day
anova(px1,px2)
plot(augPred(px2))
px3 <- update(px1,random=~1|Dog/Side)
anova(px1,px3)
plot(augPred(px3))
#comparing fixed terms
px4 <- update(px1,fixed=~.+Side)
summary(px4)
anova(px4)
px1ml <- update(px1,method="ML")
px4ml <- update(px4,method="ML")
anova(px1ml,px4ml)


#coding crossed effects
require(nlme)
data(Assay)
as0 <- lme(logDens~sample*dilut, 
           data=Assay, 
           random=pdBlocked(
             list(
               pdIdent(~1), 
               pdIdent(~sample-1), 
               pdDiag(~dilut-1)
               )
             )
           )
as0
ranef(as0)

as2 <- lme(logDens~sample*dilut, 
           data=Assay,
           random=list(
             Block=pdBlocked(
               list(
                 pdIdent(~1),
                 pdIdent(~sample-1)
                 )
               ),
             dilut=~1)
           )
as2
ranef(as2)

as3 <- lme(logDens~sample*dilut, 
           data=Assay,
           random=list(
             Block=~1,
             Block=pdIdent(~sample-1),
             dilut=~1
             )
           )
as3
ranef(as3)
