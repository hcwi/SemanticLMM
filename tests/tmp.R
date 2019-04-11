for (mod in m_all) {
  
  print(formula(mod))
  print(ref_grid(mod))
  
  #get fixed "effects?"
  fixcoefs <- summary(mod)$coefficients # get estimated effects
  feLabs <- row.names(fixcoefs)
  fixeffs <- model.matrix(mod)
  fixeffsLabs <- colnames(fixeffs) # == feLabs
  
  termAssign <- attr(model.matrix(mod), "assign")
  ref <- min(termAssign) # ass == ref -> EMM else Contrast 
  
  varLabs <- unique(unlist(strsplit(attr(terms(mod), "term.labels"),":"))) # get variable names
  for (varLab in varLabs) {
    feLabs <- sub(feLabs, pattern = varLab, rep="") # remove variable names from effect labels
  }
  feLabs
  
  print(cbind(feLabs, termAssign))
  
}


printCoefmat(dataFus)
BIC(m0)
AIC(m0)
REMLcrit(m0) # equivalent of deviance() for REML

emmeans(mod, ~Infection)
emmeans(mod, ~Infraname)
emmeans(mod, ~Infection*Infraname)
