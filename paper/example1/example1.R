getExample1 <- function() {


  #y <- as.numeric(c(1.5, 1.7, 2.1, 2.1, 1.9, 2.2))
  y <- as.numeric(c(6, 4, 5, 6, 9, 3))
  Treatment <- as.factor(c("T1","T1","T2","T2","T3","T3"))
  Block <- as.factor(c("B1","B2","B1","B2","B1","B2"))
  df <- data.frame(y,Treatment, Block)

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
  modelFitting <- exportModelToRDF(mod, ex1)
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id) #, graphName = "Example1")

}



example1_lme <- function() {

  ex1 <- getExample1()

  require(nlme)
  mod <- lme(y ~ 0 + Treatment, random = ~1|Block, data = ex1$data)

  print(formula(mod))
  modelFitting <- exportModelToRDF_2(mod, ex1)
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id) #, graphName = "Example1")

}


