getDataFusarium <- function() {
  dataFus = read.csv(file = "/Users/hania/Code/model/data_fusarium.txt", sep="\t")
  dataFus$Characteristics.Block. <- factor(dataFus$Characteristics.Block.)
  levels(dataFus$Characteristics.Block.) <- c("block1", "block2", "block3")
  levels(dataFus$Characteristics.Infraspecific.name.) <- 
    gsub(levels(dataFus$Characteristics.Infraspecific.name.), rep="", pattern = "[/_]", perl = T)
  colnames(dataFus) <- c("aName", "cInfraname", "fLocation", "cBlock", "fInfection", "tDlugosc", 
                         "tPieterka", "tZiarenka", "tMasa", "tDON", "tPlon", "tKloszenie",
                         "tWysokosc", "tIFK", "tPokroj", "tZbitosc")
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

m0_basic <- lmer(tDlugosc ~ 0 + cInfraname * fInfection + (1|cBlock), 
                 data = dataFus, REML = TRUE)

m0_noREML <- lmer(tDlugosc ~ 0 + cInfraname * fInfection + (1|cBlock), 
                 data = dataFus, REML = FALSE)

m1_basic <- lmer(tDlugosc ~ cInfraname * fInfection + (1|cBlock), 
                 data = dataFus, REML = TRUE)

m2_basic <- lmer(tDlugosc ~ fInfection + (1|cInfraname), 
                 data = dataFus, REML = TRUE)

m3_basic <- lmer(tDlugosc ~ fInfection + (1|cInfraname) + (1|cBlock), 
                 data = dataFus, REML = TRUE)

m4_basic <- lmer(tDlugosc ~ fInfection + (1|cInfraname:cBlock), 
                 data = dataFus, REML = TRUE)

m5_basic <- lmer(tDlugosc ~ fInfection + (fInfection|cInfraname) + (1|cBlock), 
                 data = dataFus, REML = TRUE)

m6_basic <- lmer(tDlugosc ~ fInfection + (1|cInfraname) + (fInfection|cBlock), 
                 data = dataFus, REML = TRUE)

oats <- getDataYates()

m_oats <- lmer(Y ~ N*V + (1|B), data = oats, REML = TRUE)


m_all <- list(m0_basic, m0_noREML, m1_basic, m2_basic, m3_basic, m4_basic, m5_basic, m6_basic, m_oats)

runPreview <- function(m1_basic) {
  summary(m1_basic)
  anova(m1_basic)
  confint(m1_basic)
  fixef(m1_basic) # Fixed effects estimators (BLUE)
  ranef(m1_basic) # Random effects predictors (BLUP)
  
  require(emmeans)
  ref_grid(m1_basic)
  emmeans(m1_basic, "cInfraname")
  emmeans(m1_basic, "fInfection")
  emmeans(m1_basic, ~cInfraname*fInfection)
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
  
  m_oats <- lmer(Y ~ N*V + (1|B), data = oats, REML = TRUE)
  summary(m_oats)
  
  m_oats_noIntercept <- lmer(Y ~ 0 + N*V + (1|B), data = oats, REML = TRUE)
  summary(m_oats_noIntercept)
  
  fixef(m_oats)
  fixef(m_oats_noIntercept)
  
}

