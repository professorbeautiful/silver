#### FOCUS ####
dataForFocus = merge(by = 'PMID', 
#  merge(
    ourNotes,
#    data.frame(PMID=rownames(abstractFocusMatrix), abstractFocusMatrix)
#  ),
  data.frame(PMID=rownames(titleFocusMatrix), titleFocusMatrix)
)
#names(dataForFocus) = gsub('(.*[ #?/\n].*)', '`\\1`', names(dataForFocus))
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

#### Modeling ####

allFeatures = paste(
  paste0('`', names(dataForFocus)[-(1:4)], '`')
        , collapse=' + '
)
allFeatures = gsub(' + `isEconomics`', '', allFeatures, fixed=T)
econoFormula = as.formula(paste('isEconomics ~ ', allFeatures))

#fullModelForEconomics = glm(data=dataForFocus,
                            # econoFormula, 
                            # family=binomial)
#summary(fullModelForEconomics)
stepModelForEconomics = step(fullModelForEconomics, scope = ~ 1)

predictedLogits = predict(stepModelForEconomics, newdata=titleFocusMatrix)
inverseLogit = function(z)exp(z)/(1+exp(z))
predictedProbs = inverseLogit(predictedLogits)
imputationMerge =   merge(all = TRUE,
                          data.frame(PMID=names(predictedLogits), 
                                     predictedLogits, 
                                     predictedProbs,
                                     titles, years, 
                                     oncotype_in_TiAbKw,
                                     mammaprint_in_TiAbKw),
                          dataForFocus[c('PMID', 'isEconomics')]
)
imputationMerge$titles[sapply(imputationMerge$isEconomics, isTRUE)]
sort(imputationMerge$titles[sapply(imputationMerge$isEconomics, isTRUE)]) ==
  sort(titles[pmid %in% dataForFocus$PMID[dataForFocus$isEconomics] ] )
####  OK we're all set now.  Remember to use isTRUE for booleans:  isTRUE(NA) is FALSE! ####

table(imputationMerge$isEconomics, exclude=NULL)
nrow(imputationMerge)
IR.df$PMID = IR.df$pmid
imputationMerge = merge(imputationMerge, IR.df)

#### Begin imputation ####
doImputation = function(NReps = 10, printMe = FALSE, saveImputations=TRUE,
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
           if(saveImputations) {
             if(ignoreMe==1) imputations <<- list()
             imputations[[ignoreMe]] <<- imputationMerge
           }
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

multipleImputationMeanVariance = function(results=glmResults){
  nReps = length(results)
  summaries = lapply(results, summary)
  estimates = sapply(summaries, function(summary) (summary$coef) [, 1, drop=F])
  stdErrors = sapply(summaries, function(summary) (summary$coef) [, 2, drop=F])
  theMeans = apply(estimates, 1, mean)
  within = apply(stdErrors^2, 1, mean)
  between = apply(estimates, 1, var)
  return( data.frame(theMeans, within = within, between = between,
            mImpVariance = within + (nReps+1)/nReps*between,
            row.names = row.names(summaries[[1]]$coefficients))
  )
}
multipleImputationMeanVariance()

multipleImputationMeanVariance(results = doImputation(ourFormula=imputedValuesForIsEconomics
                               ~ as.numeric(years)) )

table(bigMerge$oncotype_in_TiAbKw, bigMerge$mammaprint_in_TiAbKw, bigMerge$`Oncotype DX?
Mammaprint?`)  

table(imputations[[1]]$`Oncotype DX?\nMammaprint?`, 
      imputations[[1]]$imputedValuesForIsEconomics)
chisq.test(
  table(imputations[[1]]$`Oncotype DX?\nMammaprint?`, 
        imputations[[1]]$imputedValuesForIsEconomics)
  [-1, ])$resid