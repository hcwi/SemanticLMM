getExample1 <- function() {

  df <- read.table("example1.txt", header = T, sep = "\t")

  dataset <- list()
  dataset$data <- df
  dataset$label <- "Dataset_example1"
  dataset$comments <- list(paste0("rdf:value \"", paste(capture.output(dataset$data),collapse=";"),  "\""))

  dataset
}

example1_lmer<- function() {

  ex1 <- getExample1()

  require(lmerTest)
  mod <- lmer(y ~ 0 + Treatment + (1|Block), data = ex1$data)
  print(formula(mod))

  require(semLMM)
  modelFitting <- exportModelToRDF(mod, ex1)
  modelFitting$saveTriples(graphName = "example1")

  #modelFitting
}


example1_lme <- function() {

  ex1 <- getExample1()

  require(nlme)
  mod <- lme(y ~ 0 + Treatment, random = ~1|Block, data = ex1$data)
  print(formula(mod))

  require(semLMM)
  modelFitting <- exportModelToRDF(mod, ex1)
  modelFitting$saveTriples(graphName = "example1")

  #modelFitting
}


run <- function() {
  example1_lmer()
  #example1_lme()
}

run()
