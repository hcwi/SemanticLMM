dataFus = read.csv(file = "/Users/hania/Code/model/data_fusarium.txt", sep="\t")
dataFus$Characteristics.Block. <- factor(dataFus$Characteristics.Block.)
levels(dataFus$Characteristics.Block.) <- c("block1", "block2", "block3")
colnames(dataFus) <- c("aName", "cInfraname", "fLocation", "cBlock", "fInfection", "tDlugosc", 
                       "tPieterka", "tZiarenka", "tMasa", "tDON", "tPlon", "tKloszenie",
                       "tWysokosc", "tIFK", "tPokroj", "tZbitosc")

require(lme4)
require(lmerTest)

m0_basic <- lmer(tDlugosc ~ 0 + cInfraname * fInfection + (1|cBlock), 
                 data = dataFus, REML = TRUE)

m1_basic <- lmer(tDlugosc ~ cInfraname * fInfection + (1|cBlock), 
                 data = dataFus, REML = TRUE)

m2_basic <- lmer(tDlugosc ~ fInfection + (1|cInfraname), 
                 data = dataFus, REML = TRUE)

m3_basic <- lmer(tDlugosc ~ fInfection + (1|cInfraname) + (1|cBlock), 
                 data = dataFus, REML = TRUE)

m4_basic <- lmer(tDlugosc ~ fInfection + (1|cInfraname:cBlock), 
                 data = dataFus, REML = TRUE)


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
