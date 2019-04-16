##### aux #####

package_aux <- function() {
  usethis::use_package("lme4")
  usethis::use_package("nlme")
  usethis::use_package("lmerTest")
  usethis::use_package("assertthat")
  usethis::use_package("emmeans")
  usethis::use_package("log4r")

  usethis::use_data(example1)
  usethis::use_data(example3)
}

# preparing example1
{
y <- as.numeric(c(6, 4, 5, 6, 9, 3))
Treatment <- as.factor(c("T1","T1","T2","T2","T3","T3"))
Block <- as.factor(c("B1","B2","B1","B2","B1","B2"))
example1 <- data.frame(y,Treatment, Block)
usethis::use_data(example1)
}

# preparing example3
{
path <- "/Users/hania/Code/R_oom/example3/dataset_field_IPGPAS_Polapgen/"
s1 <- read.table(paste0(path, "s_study1.txt"), sep="\t", head=T, dec = ",")
a1 <- read.table(paste0(path, "a_study1_phenotyping2012.txt"), sep="\t", head=T, dec = ",")
d1 <- read.table(paste0(path, "d_polapgen_field2012.txt"), sep="\t", head=T, dec = ",")
sad1 <- merge(merge(s1, a1, by="Sample.Name"), d1, by="Assay.Name")
s2 <- read.table(paste0(path, "s_study2.txt"), sep="\t", head=T, dec = ",")
a2 <- read.table(paste0(path, "a_study1_phenotyping2013.txt"), sep="\t", head=T, dec = ",")
d2 <- read.table(paste0(path, "d_polapgen_field2013.txt"), sep="\t", head=T, dec = ",")
sad2 <- merge(merge(s2, a2, by="Sample.Name"), d2, by="Assay.Name")
sad <- rbind(sad1, sad2)
names(sad)
summary(sad)
cols <- c("Characteristics.Infraspecific.name.", "Characteristics.Study.start.", "Factor.Value.Replication.", names(sad)[55:64])
dat <- sad[cols]
names(dat) <- c("InfraspecificName", "StudyStart", "Replication", names(sad)[55:64])
for (i in 1:3) {
  dat[,i] <- as.factor(dat[,i])
}
dat
example3 <- dat
usethis::use_data(example3)
}


##### test package
library(semLMM)

#example1 lmer
require(lmerTest)
mod <- lmer(y ~ 0 + Treatment + (1|Block), data = example1)
modelFitting <- exportModelToRDF(mod, example1)
modelFitting$saveTriples()

#example1 nlme
require(nlme)
mod <- lme(y ~ 0 + Treatment, random = ~1|Block, data = example1)
modelFitting <- exportModelToRDF_2(mod, example1)
modelFitting$saveTriples()


#example3
ds <- example3
subset <- list()
subset$data <- ds[as.numeric(ds$InfraspecificName) < 5, ]
subset$label <- "Dataset_example3_Polapgen-subset"
subset$url <- paste("http://cropnet.pl/plantphenodb/index.php?id=250")
subset$dataset <- Dataset(label = ifelse(!is.null(subset[["label"]]), subset[["label"]], "Dataset"),
                      url = ifelse(!is.null(subset[["url"]]), subset[["url"]], "url unavailable"),
                      comments = {if (!is.null(subset[["comments"]])) subset[["comments"]] else list()})

mod <- lme(GW_m2 ~ InfraspecificName*StudyStart + HD, random = ~1|Replication, data = subset$data)
modelFitting <- exportModelToRDF_2(mod, ds = subset)
modelFitting$saveTriples()

#"Verbosity threshold (5=DEBUG, 4=INFO 3=WARN, 2=ERROR, 1=FATAL)"))
# logger <- create.logger(logfile = "", level = verbosity(5))
#
# fatal(logger, "Fatal message")
# error(logger, "Error message")
# warn(logger, "Warning message")
# info(logger, "Informational message")
# debug(logger, "Debugging message")

