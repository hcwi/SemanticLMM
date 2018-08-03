getDataFusarium <- function() {
  dataFus = read.csv(file = "/Users/hania/Code/model/data_fusarium.txt", sep="\t")
  dataFus$Characteristics.Block. <- factor(dataFus$Characteristics.Block.)
  levels(dataFus$Characteristics.Block.) <- c("block1", "block2", "block3")
  levels(dataFus$Characteristics.Infraspecific.name.) <- 
    gsub(levels(dataFus$Characteristics.Infraspecific.name.), rep="", pattern = "[/_]", perl = T)
  colnames(dataFus) <- c("Name", "Infraname", "Location", "Block", "Infection", "Dlugosc", 
                         "Pieterka", "Ziarenka", "Masa", "DON", "Plon", "Kloszenie",
                         "Wysokosc", "IFK", "Pokroj", "Zbitosc")
  dataFus
}

getDataYates <- function() {
  library(MASS)
  data("oats")
  oats
}

require(lme4)
require(lmerTest)

dataFus <- getDataFusarium()

m0 <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
                 data = dataFus, REML = TRUE)

#options(contrasts=c("contr.sum", "contr.poly"))
#options(contrasts=c("contr.treat", "contr.poly"))
m0_contrSum <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
                    data = dataFus, REML = TRUE, contrasts = list(Infraname="contr.sum", Infection="contr.sum"))

m0_contrPoly <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
                    data = dataFus, REML = TRUE, contrasts = list(Infraname=contr.poly(5)))

m0_contrNo <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
                   data = dataFus, REML = TRUE, contrasts = list(Infraname=contr.treatment(5, contrast=F)))
model.matrix(m0_contrNo)
summary(m0_contrNo)$coef # intercept: Soldo infected, missing: Soldo none, dropped: Soldo, Soldo_none

m0_contrNoAll <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
                   data = dataFus, REML = TRUE, contrasts = list(Infraname=contr.treatment(5, contrast=F), Infection=contr.treatment(2, contrast=F)))
model.matrix(m0_contrNoAll)
summary(m0_contrNoAll)$coef # intercept: Soldo infected, missing: Soldo none, dropped: Soldo, Soldo_none


m0_noREML <- lmer(Dlugosc ~ Infraname * Infection + (1|Block), 
                 data = dataFus, REML = FALSE)

m0_noint <- lmer(Dlugosc ~ 0 + Infraname * Infection + (1|Block), 
                 data = dataFus, REML = TRUE)

m2 <- lmer(Dlugosc ~ Infection + (1|Infraname), 
                 data = dataFus, REML = TRUE)

m2_2Rand <- lmer(Dlugosc ~ Infection + (1|Infraname) + (1|Block), 
                 data = dataFus, REML = TRUE)

m2_hierRand <- lmer(Dlugosc ~ Infection + (1|Infraname:Block), 
                 data = dataFus, REML = TRUE)

m2_slope <- lmer(Dlugosc ~ Infection + (Infection|Infraname) + (1|Block), 
                 data = dataFus, REML = TRUE)

m2_slopeAlt <- lmer(Dlugosc ~ Infection + (1|Infraname) + (Infection|Block), 
                 data = dataFus, REML = TRUE)

m3_hierFixed <- lmer(Dlugosc ~ Infraname:Infection + (1|Block), 
                 data = dataFus, REML = TRUE)

m3_hierFixedAlt <- lmer(Dlugosc ~ Infection:Infraname + (1|Block), 
                 data = dataFus, REML = TRUE)

m4_addFixed <- lmer(Dlugosc ~ Infection + Infraname + (1|Block), 
                 data = dataFus, REML = TRUE)

#m5_3Fixed <- lmer(Dlugosc ~ Infection*Infraname*Block + (1|Block), 
#                 data = dataFus, REML = TRUE)

oats <- getDataYates()

m_oats <- lmer(Y ~ N*V + (1|B), data = oats, REML = TRUE)


m_all <- list(m0, m0_noREML, m0_noint, 
              m2, m2_2Rand, m2_hierRand, m2_slope, m2_slopeAlt,
              m3_hierFixed, m3_hierFixedAlt,
              m4_addFixed,
              #m5_3Fixed,
              m_oats)

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

