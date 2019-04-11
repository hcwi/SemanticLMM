getDataFusarium <- function() {
  dataFus = read.csv(file = "/Users/hania/Code/model/data_fusarium.txt", sep="\t")
  dataFus$Characteristics.Block. <- factor(dataFus$Characteristics.Block.)
  levels(dataFus$Characteristics.Block.) <- c("block1", "block2", "block3")
  levels(dataFus$Characteristics.Infraspecific.name.) <- 
    gsub(levels(dataFus$Characteristics.Infraspecific.name.), rep="", pattern = "[/_]", perl = T)
  #colnames(dataFus) <- c("Name", "Infraname", "Location", "Block", "Infection", "Dlugosc", 
  #                       "Pieterka", "Ziarenka", "Masa", "DON", "Plon", "Kloszenie",
  #                       "Wysokosc", "IFK", "Pokroj", "Zbitosc")
  colnames(dataFus) <- c("Name", "Variety", "Location", "Block", "Infection", "SpikeLength", 
                         "Spikelets", "Grains", "Mass", "DON", "Yield", "DaysToHeading",
                         "Height", "IFK", "GrowthType", "SpikeDensity")
  levels(dataFus$Infection) <- c("Infected", "None")
  dataFus
}

getDataYates <- function() {
  library(MASS)
  data("oats")
  oats
}

require(lme4)
require(lmerTest)
require(nlme)

dataFus <- getDataFusarium()

getExamples <- function() {
  
  rights <- c(" ~ Infection + Variety + (1|Block)",
              " ~ Variety * Infection + (1|Block)",
              " ~ 0 + Variety * Infection + (1|Block)",
              " ~ Infection + (1|Variety) + (1|Block)",
              " ~ Variety:Infection + (1|Block)",
              " ~ Mass + Infection + (1|Block)",
              " ~ 0 + Mass*Infection + (1|Block)"
  )
  
  models <- list()
  for (varName in names(dataFus)[c(6:8,11:16)]) {
    for (r in rights) {
      f <- as.formula(paste0(varName, r))
      print(f)
      mod <- lmer(f, data = dataFus)
      models <- append(models, mod)
    }
  }
  models
}

getExamplesAdv <- function() {
  
  adv <- lmer(Yield ~ Mass + Variety*Infection + (1|Block), data = dataFus)
  adv
  
  adv_nested <- lmer(Yield ~ Mass + Infection + (1|Block/Variety), data = dataFus)
  ranef(adv_nested)
  
  adv_sep <- lmer(Yield ~ Mass + Infection + (1|Block) + (1|Variety), data = dataFus)
  adv_sep
  
  adv2 <- lme(Yield ~ Mass + Variety*Infection, random=~1|Block, data = dataFus)
  adv2
  
  adv3 <- lme(Yield ~ Mass + Variety*Infection, random=list(Block=pdCompSymm(~Block-1)), data = dataFus)
  adv3
  
  adv3b <- lme(Yield ~ Mass + Variety*Infection, random=list(Block=pdDiag(~Block-1)), data = dataFus)
  adv3b
  
  adv4_nested <- lme(Yield ~ Mass + Variety*Infection, random=~1|Block/Variety, data = dataFus)
  adv4_nested$modelStruct$reStruct
  
  adv5_nested <- lme(Yield ~ Mass + Variety*Infection, random=list(Block=pdCompSymm(~Block-1), Variety=pdIdent(~1)), data = dataFus)
  formula(adv5_nested$modelStruct$reStruct["Block"])
  
  adv6_nested <- lme(Yield ~ Mass + Variety*Infection, random=list(Block=pdCompSymm(~Block-1), Variety=pdIdent(~1), Infection=pdIdent(~1)), data = dataFus)
  adv6_nested
  ranef(adv6_nested)
  
  adv7_nested <- lme(Yield ~ Mass + Variety*Infection, random=list(Block=pdCompSymm(~Block-1), Variety=pdCompSymm(~Variety-1)), data = dataFus)
  formula(adv7_nested$modelStruct$reStruct["Block"])
  adv7_nested$modelStruct$reStruct
  VarCorr(adv7_nested)
}

getTestCS <- function() {
  
  test_mixed <- lme(Yield ~ Variety*Infection, 
                    random=list(Block=pdCompSymm(~Variety-1)), 
                    data = dataFus)
  test_mixed$modelStruct$reStruct$Block
  test_mixed$coefficients$random
  VarCorr(test_mixed)
  
  
  test_nested <- lme(Yield ~ Variety*Infection, 
                     random=list(Block=pdCompSymm(~Block-1), 
                                 Variety=pdIdent(~1)), 
                     data = dataFus)
  test_nested$modelStruct$reStruct$Block
  test_nested$modelStruct$reStruct$Variety
  test_nested$coefficients$random
  
  
  test_nested2 <- lme(Yield ~ Variety*Infection, 
                      random=list(Block=pdCompSymm(~Block-1), 
                                  Variety=pdCompSymm(~Variety-1)), 
                      data = dataFus)
  test_nested2$modelStruct$reStruct$Block
  test_nested2$modelStruct$reStruct$Variety
  test_nested2$coefficients$random
  
  
  test_blocked <- lme(Yield ~ Variety*Infection, 
                      random=list(Block=pdBlocked(list(pdCompSymm(~Block-1), 
                                                       pdDiag(~Variety-1)))), 
                      data = dataFus)
  test_blocked$modelStruct$reStruct$Block
  test_blocked$coefficients$random
}

# m0 <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
#                  data = dataFus, REML = TRUE)
# 
# #options(contrasts=c("contr.sum", "contr.poly"))
# #options(contrasts=c("contr.treat", "contr.poly"))
# m0_contrSum <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
#                     data = dataFus, REML = TRUE, contrasts = list(Infraname="contr.sum", Infection="contr.sum"))
# 
# m0_contrPoly <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
#                     data = dataFus, REML = TRUE, contrasts = list(Infraname=contr.poly(5)))
# 
# m0_contrNo <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
#                    data = dataFus, REML = TRUE, contrasts = list(Infraname=contr.treatment(5, contrast=F)))
# model.matrix(m0_contrNo)
# summary(m0_contrNo)$coef # intercept: Soldo infected, missing: Soldo none, dropped: Soldo, Soldo_none
# 
# m0_contrNoAll <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
#                    data = dataFus, REML = TRUE, contrasts = list(Infraname=contr.treatment(5, contrast=F), Infection=contr.treatment(2, contrast=F)))
# model.matrix(m0_contrNoAll)
# summary(m0_contrNoAll)$coef # intercept: Soldo infected, missing: Soldo none, dropped: Soldo, Soldo_none
# 
# 
# m0_noREML <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
#                  data = dataFus, REML = FALSE)
# 
# m0_noint <- lmer(Dlugosc ~ 0 + Infraname * Infection + (1|Block), 
#                  data = dataFus, REML = TRUE)
# 
# m2 <- lmer(Dlugosc ~ Infection + (1|Infraname), 
#                  data = dataFus, REML = TRUE)
# 
# m2_2Rand <- lmer(Dlugosc ~ Infection + (1|Infraname) + (1|Block), 
#                  data = dataFus, REML = TRUE)
# 
# m2_hierRand <- lmer(Dlugosc ~ Infection + (1|Infraname:Block), 
#                  data = dataFus, REML = TRUE)
# 
# m2_slope <- lmer(Dlugosc ~ Infection + (Infection|Infraname) + (1|Block), 
#                  data = dataFus, REML = TRUE)
# 
# m2_slopeAlt <- lmer(Dlugosc ~ Infection + (1|Infraname) + (Infection|Block), 
#                  data = dataFus, REML = TRUE)
# 
# m3_hierFixed <- lmer(Dlugosc ~ Infraname:Infection + (1|Block), 
#                  data = dataFus, REML = TRUE)
# 
# m3_hierFixedAlt <- lmer(Dlugosc ~ Infection:Infraname + (1|Block), 
#                  data = dataFus, REML = TRUE)
# 
# m4_addFixed <- lmer(Dlugosc ~ Infection + Infraname + (1|Block), 
#                  data = dataFus, REML = TRUE)
# 
# #m5_3Fixed <- lmer(Dlugosc ~ Infection*Infraname*Block + (1|Block), 
# #                 data = dataFus, REML = TRUE)
# 
# m_cov <- lmer(Dlugosc ~ Masa + Infection + (1|Block), 
#               data = dataFus, REML = TRUE)
# 
# m_cov2 <- lmer(Dlugosc ~ Infection + Masa + (1|Block), 
#               data = dataFus, REML = TRUE)
# 
# m_cov_noint <- lmer(Dlugosc ~ 0 + Masa + Infection + (1|Block), 
#                     data = dataFus, REML = TRUE)
# 
# m_cov_noint2 <- lmer(Dlugosc ~ 0 + Infection + Masa + (1|Block), 
#                     data = dataFus, REML = TRUE)
# 
# m_cov_mult <- lmer(Dlugosc ~ Masa*Infection + (1|Block), 
#                    data = dataFus, REML = TRUE)
# 
# m_cov_mult2 <- lmer(Dlugosc ~ 0+Masa*Infection + (1|Block), 
#                    data = dataFus, REML = TRUE)
# 
# m_cov_mult3 <- lmer(Dlugosc ~ Masa:Infection + (1|Block), 
#                    data = dataFus, REML = TRUE)
# 
# m_cov_mult4 <- lmer(Dlugosc ~ 0+Masa:Infection + (1|Block), 
#                     data = dataFus, REML = TRUE)
# 
# summary(m_cov_noint)$coef
# attr(model.matrix(m_cov_noint), "assign")
# 
# 
# oats <- getDataYates()
# 
# m_oats <- lmer(Y ~ N*V + (1|B), data = oats, REML = TRUE)
# 
# 
# m_all <- list(m0, m0_noREML, m0_noint, 
#               m2, m2_2Rand, m2_hierRand, m2_slope, m2_slopeAlt,
#               m3_hierFixed, m3_hierFixedAlt,
#               m4_addFixed,
#               #m5_3Fixed,
#               m_oats)

runPreview <- function(mod) {
  summary(mod)
  anova(mod)
  confint(mod)
  fixef(mod) # Fixed effects estimators (BLUE)
  ranef(mod) # Random effects predictors (BLUP)
  
  require(emmeans)
  ref_grid(mod)
  emmeans(mod, "Infraname")
  emmeans(mod, "Infection")
  emmeans(mod, ~Infraname*Infection)
}


analyseYatesDataset <- function() {
  
  oats <- getDataYates()
  
  boxplot(yield ~ nitro*Variety, data=oats)
  
  oats$Nf <- ordered(oats$N, levels = sort(levels(oats$N)))
  oats.aov <- aov(Y ~ Nf*V + Error(B/V), data = oats, qr = TRUE)
  summary(oats.aov)
  summary(oats.aov, split = list(Nf=list(L=1, Dev=2:3)))
  par(mfrow = c(1,2), pty = "s")
  plot(fitted(oats.aov[[4]]), studres(oats.aov[[4]]))
  abline(h = 0, lty = 2)
  oats.pr <- proj(oats.aov)
  qqnorm(oats.pr[[4]][,"Residuals"], ylab = "Stratum 4 residuals")
  qqline(oats.pr[[4]][,"Residuals"])
  
  par(mfrow = c(1,1), pty = "m")
  oats.aov2 <- aov(Y ~ N + V + Error(B/V), data = oats, qr = TRUE)
  model.tables(oats.aov2, type = "means", se = TRUE)
  model.tables(oats.aov2, type = "effects", se = TRUE)
  
  m_oats_splitplot <- lmer(Y ~ N*V + (1|B/V), data = oats, REML = TRUE)
  summary(m_oats_splitplot)
  
  m_oats_noIntercept <- lmer(Y ~ 0 + N*V + (1|B), data = oats, REML = TRUE)
  summary(m_oats_noIntercept)
  
  fixef(m_oats)
  fixef(m_oats_noIntercept)
  
}


