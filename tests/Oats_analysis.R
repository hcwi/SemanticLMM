Oats

fm1Oats <- lme( yield ~ ordered(nitro) * Variety, data = Oats, random = ~ 1 | Block/Variety )
fm2Oats <- update( fm1Oats, yield ~ ordered(nitro) + Variety )
fm3Oats <- update( fm1Oats, yield ~ ordered( nitro ) )
fm4Oats <- lme( yield ~ nitro, data = Oats, random = ~ 1 | Block/Variety )
fm4OatsB <- lme(yield ~ nitro, data = Oats, random =list(Block = pdCompSymm(~ Variety - 1)))
fm4OatsC <- lme(yield ~ nitro, data = Oats,
                random=list(Block=pdBlocked(list(pdIdent(~ 1), pdIdent(~ Variety-1)))))

fm5Oats <- update(fm4Oats, random = ~1|Block, corr = corSymm(form = ~1|Block/Variety))
fm6Oats <- update(fm4Oats, random = ~1|Block, corr = corCompSymm(form = ~1|Block/Variety))
compareFits(coef(fm4OatsB), coef(fm4OatsC))
comparePred(fm4Oats, fm6Oats)

corMatrix(fm4Oats$modelStruct$reStruct$Block)
corMatrix(fm4OatsB$modelStruct$reStruct$Block)
corMatrix(fm4OatsC$modelStruct$reStruct$Block)
corMatrix(fm5Oats$modelStruct$reStruct$Block)
corMatrix(fm6Oats$modelStruct$reStruct$Block)
VarCorr(fm4Oats)
