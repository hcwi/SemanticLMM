getModelName <- function (x, ...) {
  UseMethod("getModelName", x)
}
getModelName.lmerMod <- function(mod) {
  print(class(mod))
  print(mod@call)
  }
getModelName.lme <- function(mod) {
  print(class(mod))
  print(mod$call)
  }



source("example1/example1.R")
ex1 <- getExample1()

require(lme4)
mod_x <- lmer(y ~ 0 + Treatment + (1|Block), data = ex1$data)

require(nlme)
mod_y <- lme(y ~ 0 + Treatment, random = ~1|Block, data = ex1$data)

getModelName(mod_x)
getModelName(mod_y)
