#### logistic regression ####
dataForFocus = merge(ourNotes[c('PMID', 'STATUS', 'FOCUS', 'notRelevant')],
                     data.frame(PMID=rownames(titleFocusMatrix), titleFocusMatrix)
)
dataForFocus$isEconomics = (dataForFocus$FOCUS=='Economics')
dataForFocus = dataForFocus[dataForFocus$notRelevant == FALSE
                            & !is.na(dataForFocus$STATUS) 
                            & dataForFocus$STATUS!='in progress...', ]

dim(dataForFocus)

### check matching with the titles ####  
dataForFocus$PMID[dataForFocus$isEconomics]
titles[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics] ] 
###  OK, it appears that we are selecting the titles correctly 
### But the order is wrong:
dataForFocus$PMID[dataForFocus$isEconomics] [1]
pmid[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics] ] [1]
sort(pmid[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics] ]) [1]
####  Using imputationMerge defined below:
imputationMerge$titles[sapply(imputationMerge$isEconomics, isTRUE)]
sort(imputationMerge$titles[sapply(imputationMerge$isEconomics, isTRUE)]) ==
  sort(titles[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics] ] )
####  OK we're all set now.  Remember to use isTRUE for booleans:  isTRUE(NA) is FALSE! ####

table(years[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics]])  ### First one in 2005
table(oncotype_in_TiAbKw[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics]],
      mammaprint_in_TiAbKw[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics]],
      dnn = c('ODX', 'MP')
)  

#### so why is this wrong?  ####
table(years[pmid %in% dataForFocus$PMID], dataForFocus$FOCUS)
head(cbind(pmid[pmid %in% dataForFocus$PMID], dataForFocus$PMID) )  ### yup, wrong! 



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
IR.df$PMID = IR.df$pmid
imputationMerge = merge(imputationMerge, IR.df)

#### Begin imputation ####
doImputation = function(NReps = 10, printMe = FALSE,
                        ourFormula=imputedValuesForIsEconomics
                        ~ as.numeric(years) + oncotype_in_TiAbKw + mammaprint_in_TiAbKw) {
  glmResults = lapply(1:NReps, 
         function(ignoreMe     
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
           glm.out = glm(data=imputationMerge, formula = ourFormula, family=binomial)   #### test interesting questions here.
           if(printMe) print(summary(glm.out))
           glm.out
         }
  )
  glmResults
}
glmResults = doImputation()
dimnames(summary(glmResults[[1]])$coefficients)
sapply(glmResults, function(result)
  coef(summary(result))['as.numeric(years)', 'z value']
)
