#### logistic regression ####
dataForFocus = merge(ourNotes[c('PMID', 'STATUS', 'FOCUS', 'notRelevant')],
                     data.frame(PMID=rownames(titleFocusMatrix), titleFocusMatrix)
)
dataForFocus$isEconomics = (dataForFocus$FOCUS=='Economics')
dataForFocus = dataForFocus[dataForFocus$notRelevant == FALSE
                            & !is.na(dataForFocus$STATUS) 
                            & dataForFocus$STATUS!='in progress...', ]

dim(dataForFocus)

### check it!  
dataForFocus$PMID[dataForFocus$isEconomics]
titles[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics]]

#### Modeling ####
allFeatures = paste(names(dataForFocus[-(1:4)]), collapse=' + ')
allFeatures = gsub(' + isEconomics', '', allFeatures, fixed=T)
econoFormula = as.formula(paste('isEconomics ~ ', allFeatures))

fullModelForEconomics = glm(data=dataForFocus,
                            econoFormula, 
                            family=binomial)
summary(fullModelForEconomics)
stepModelForEconomics = step(fullModelForEconomics, scope = ~ 1)

predictedLogits = predict(stepModelForEconomics, newdata=titleFocusMatrix)
inverseLogit = function(z)exp(z)/(1+exp(z))
predictedProbs = inverseLogit(predictedLogits)
imputationMerge =   merge(all = TRUE,
                          data.frame(PMID=names(predictedLogits), predictedLogits, predictedProbs),
                          dataForFocus[c('PMID', 'isEconomics')]
)
table(imputationMerge$isEconomics, exclude=NULL)
nrow(imputationMerge)

#### Begin imputation ####
NReps = 10
glmResults = 
  lapply(1:NReps, 
         function(ignoreMe,
                  ourFormula=imputationMerge$imputedValuesForIsEconomics
                  ~ as.numeric(years) # + oncotype_in_TiAbKw + mammaprint_in_TiAbKw,
         ) {
           imputationMerge$imputedValuesForIsEconomics = 
             rbinom(length(predictedProbs), 1, predictedProbs)
           ### replace imputed values by values from ourNotes when available. ####
           
           # print(  table(imputationMerge$imputedValuesForIsEconomics,
           #               imputationMerge$isEconomics,
           #               exclude=NULL) 
           # )
           imputationMerge$imputedValuesForIsEconomics[!is.na(imputationMerge$isEconomics)] = 
             imputationMerge$isEconomics[!is.na(imputationMerge$isEconomics)]
           # print(  table(imputationMerge$imputedValuesForIsEconomics,
           #               imputationMerge$isEconomics,
           #               exclude=NULL) 
           # )
           glm.out = glm(ourFormula, family=binomial)   #### test interesting questions here.
           print(summary(glm.out))
         }
  )
sapply(glmResults, function(result)
  coef(result)[2, 'Estimate']
)
