# semantic model creation

# manully from Genstat

source("example2/example2_manualGenstatR.R")
example2_genstat()


# automatically from R nlme

source("lmer_create.R")

source("example1/example1.R")
example1_lmer()

source("examples_R.R")
examplesOats_lmer()
examplesNPK_lmer()


# automatically from R lme 

source("lme_create.R")
source("example1/example1.R")
example1_lme()

source("example3/example3_Polapgen.R")
example3_lme()
examplePolapgen_lme()
examplePolapgenAllTraits_lme()

source("examples_R.R")
examplesOats_lme()
