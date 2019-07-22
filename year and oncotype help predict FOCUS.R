#####  Does year and oncotype help predict FOCUS? (No) ####

onlyOneTest = (bigMerge$oncotype_in_TiAbKw + bigMerge$mammaprint_in_TiAbKw)==1

library(MASS)  ### for loglm
loglm(   ~ FOCUS + oncotype_in_TiAbKw, 
         xtabs( ~ FOCUS + oncotype_in_TiAbKw,
                data=bigMerge[onlyOneTest & !is.na(bigMerge$FOCUS),  ])
)
#### not helpful

#### Try to analyze each FOCUS separately ####
focusCategories = unique(bigMerge$FOCUS)[-2]   ## Removing NA.
bigMerge$years = as.numeric(bigMerge$years)
glmAfocus = function(focus, predictor = 'oncotype_in_TiAbKw * years') {
  print(focus)
  try((glm( 
  formula=(FOCUS %in%  focus)
            ~  #oncotype_in_TiAbKw * 
    years ,
            data=bigMerge,
            subset= onlyOneTest & !is.na(bigMerge$FOCUS) & years < 2019, 
            family=poisson
  ))
  )
}
allFocusModels = lapply(focusCategories, glmAfocus)
names(allFocusModels) = focusCategories
names(allFocusModels[[1]])
sapply(allFocusModels, '[', i='deviance')
sapply(allFocusModels, '[', i='null.deviance')
allFocusAnovas = lapply(allFocusModels, anova)
allFocusAnovas
names(allFocusAnovas) = focusCategories
allFocusSummaries = lapply(allFocusModels, summary)
names(allFocusSummaries) = focusCategories
allFocusSummaries
Pvalues = sapply(allFocusSummaries, function(summ) summ$coefficients[-1, 4])
min(Pvalues)
####  No relationships anywhere with interaction model####


summary(glmAfocus(patientBehaviorFocus))
summary(glmAfocus(patientOutcomeFocus))

## Modeling ONLY with years, Patient prognosis gives P=0.013, est= -0.127
allFocusModels[1]
sort(sapply(allFocusModels, function(model) model$coefficients[2]))

table(focusCategories)
