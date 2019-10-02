# read original data from ISA-Tab format
getPolapgen <- function() {
  path <- "example3/"
  s1 <- read.table(paste0(path, "s_study1.txt"), sep="\t", header=T, dec = ",")
  a1 <- read.table(paste0(path, "a_study1_phenotyping2012.txt"), sep="\t", header=T, dec = ",")
  d1 <- read.table(paste0(path, "d_polapgen_field2012.txt"), sep="\t", header=T, dec = ",")
  sad1 <- merge(merge(s1, a1, by="Sample.Name"), d1, by="Assay.Name")
  s2 <- read.table(paste0(path, "s_study2.txt"), sep="\t", header=T, dec = ",")
  a2 <- read.table(paste0(path, "a_study1_phenotyping2013.txt"), sep="\t", header=T, dec = ",")
  d2 <- read.table(paste0(path, "d_polapgen_field2013.txt"), sep="\t", header=T, dec = ",")
  sad2 <- merge(merge(s2, a2, by="Sample.Name"), d2, by="Assay.Name")
  sad <- rbind(sad1, sad2)
  names(sad)
  summary(sad)
  cols <- c("Characteristics.Infraspecific.name.", "Characteristics.Study.start.", "Factor.Value.Replication.", names(sad)[55:64])
  dat <- sad[cols]
  names(dat) <- c("InfraspecificName", "StudyStart", "Replication", names(sad)[55:64])
  dat
  }

getExample3 <- function() {

  #dat <- getPolapgen()
  dat <- read.table("example3.txt", sep="\t", header = T)
  for (i in 1:3) {
    dat[,i] <- as.factor(dat[,i])
  }
  dat

  ds <- list()
  ds$data <- dat
  ds$label <- "Dataset_Polapgen"
  ds$url <- paste("http://cropnet.pl/plantphenodb/index.php?id=250")
  ds$dataset <- Dataset(label = ifelse(!is.null(ds[["label"]]), ds[["label"]], "Dataset"),
                             url = ifelse(!is.null(ds[["url"]]), ds[["url"]], "url unavailable"),
                             comments = {if (!is.null(ds[["comments"]])) ds[["comments"]] else list()})
  ds
}


example3_lme <- function() {
  ex3 <- getExample3()
  str(ex3$data)
  mod <- lme(GW_m2 ~ InfraspecificName*StudyStart + HD, random = ~1|Replication, data = ex3$data)
  print(formula(mod))
  require(semLMM)
  modelFitting <- exportModelToRDF(mod, ds = ex3)
  modelFitting$saveTriples(graphName = "example3")
}

example3_subset_lme <- function() {
  ex3 <- getExample3()
  subset <- list()
  subset$data <- ex3$data[as.numeric(ex3$data$InfraspecificName) < 5, ]
  subset$label <- "Dataset_example3_Polapgen-subset"
  subset$url <- paste("http://cropnet.pl/plantphenodb/index.php?id=250")

  mod <- lme(GW_m2 ~ InfraspecificName*StudyStart + HD, random = ~1|Replication, data = subset$data)
  print(formula(mod))

  require(semLMM)
  modelFitting <- exportModelToRDF(mod, ds = subset)
  modelFitting$saveTriples(graphName = "example3_subset")
}


example3_batchAllTraits_lme <-function() {

  ex3 <- getExample3()
  str(ex3$data)

  mod <- lme(TGW ~ InfraspecificName*StudyStart + HD, random = ~1|Replication, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
  mod <- lme(TGW ~ InfraspecificName+StudyStart + HD, random = ~1|Replication, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
  mod <- lme(TGW ~ InfraspecificName + HD, random = ~1|StudyStart/Replication, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
  mod <- lme(TGW ~ HD, random = ~1|InfraspecificName, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
  mod <- lme(TGW ~ InfraspecificName + StudyStart*HD, random = ~1|Replication, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

  mod <- lme(GW_MS ~ InfraspecificName*StudyStart + HD, random = ~1|Replication, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
  mod <- lme(GW_MS ~ StudyStart+HD, random = ~1|InfraspecificName, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

  mod <- lme(main_spike ~ InfraspecificName*StudyStart + HD, random = ~1|Replication, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
  mod <- lme(main_spike ~ StudyStart+HD, random = ~1|InfraspecificName, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

  mod <- lme(main_stem ~ InfraspecificName*StudyStart + HD, random = ~1|Replication, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
  mod <- lme(main_stem ~ StudyStart+HD, random = ~1|InfraspecificName, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

  mod <- lme(G_MS ~ InfraspecificName*StudyStart + HD, random = ~1|Replication, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)
  mod <- lme(G_MS ~ StudyStart+HD, random = ~1|InfraspecificName, data = ex3$data); print(formula(mod))
  modelFitting <- exportModelToRDF(mod, ds = ex3); modelFitting$saveTriples(modelFitting$hasInput[[1]]$id)

}


run <- function() {
  example3_lme()
  example3_subset_lme()
  #example3_batchAllTraits_lme()
}

run()
