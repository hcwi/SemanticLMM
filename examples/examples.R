getExampleOats <- function() {

  require(nlme)

  data("Oats")
  df <- Oats
  levels(df$Variety) <- sub(" ", "", levels(df$Variety))

  dataset <- list()
  dataset$data <- df
  dataset$label <- "Dataset_Oats"
  dataset$comments <- list(paste0("rdf:value \"", "
Data from an Oats Field Trial
Description: The yield of oats from a split-plot field trial using three varieties and four levels of manurial treatment. The experiment was laid out in 6 blocks of 3 main plots, each split into 4 sub-plots. The varieties were applied to the main plots and the manurial treatments to the sub-plots.
Source: Yates, F. (1935) Complex experiments, Journal of the Royal Statistical Society Suppl. 2, 181–247.
References: Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
                                  ",  "\""))
  dataset
}

examplesOats_lmer<- function() {

  ex <- getExampleOats()

  require(lme4)
  mod <- lmer(yield ~ nitro*Variety + (1|Block), data = ex$data)
  print(formula(mod))

  require(semLMM)
  modelFitting <- exportModelToRDF(mod, ex)
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
}



examplesOats_lme <- function() {

  ex <- getExampleOats()

  require(nlme)
  mod <- lme(yield ~ nitro + Variety, random = ~1|Block, data = ex$data)
  print(formula(mod))

  require(semLMM)
  modelFitting <- exportModelToRDF(mod, ex)
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

  mod <- lme(yield ~ 0 + nitro * Variety, random = list(Block=pdIdent(~1)), data = ex$data)
  print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ex) # !!!
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

  #mod <- lme(yield ~ nitro + Variety, random = list(Block=pdDiag(~Block-1)), data = ex$data) # pdDiag not implemented yet
  mod <- lme(yield ~ nitro, random = ~1|Variety/Block, data = ex$data)
  print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ex)
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

}



getExampleNPK <- function() {

  require(nlme)
  data("npk")
  df <- npk

  dataset <- list()
  dataset$data <- df
  dataset$label <- "Dataset_NPK"
  dataset$comments <- list(paste0("rdf:value \"", "
Data from a classical NPK factorial experiment.
Description: A classical N, P, K (nitrogen, phosphate, potassium) factorial experiment on the growth of peas conducted on 6 blocks. Each half of a fractional factorial design confounding the NPK interaction was used on 3 of the plots.
The npk data has 24 rows and 5 columns: block (labelled 1 to 6), N, P, K (indicators 0/1 for the application of nitrogen, phosphate, and potassium) and yield (yield of peas, in pounds/plot from the plots of (1/70) acre).
Source: Imperial College, London, M.Sc. exercise sheet.
References: Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
                                  ",  "\""))
  dataset
}


examplesNPK_lmer<- function() {

  ex <- getExampleNPK()

  require(lme4)
  mod <- lmer(yield ~ N*P*K + (1|block), data = ex$data)
  print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ex)
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

  mod <- lmer(yield ~ 0 + N*K + (1|block), data = ex$data)
  print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ex)
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

}


examplesNPK_lmer<- function() {

  ex <- getExampleNPK()

  require(lme4)
  mod <- lmer(yield ~ N*P*K + (1|block), data = ex$data)
  print(formula(mod))

  modelFitting <- exportModelToRDF(mod, ex) #!!!
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

  mod <- lmer(yield ~ 0 + N*K + (1|block), data = ex$data)
  print(formula(mod))

  modelFitting <- exportModelToRDF(mod, ex)
  modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

}

run <- function() {
  examplesOats_lmer()
  #examplesOats_lme()
  examplesNPK_lmer()
}

#run()
