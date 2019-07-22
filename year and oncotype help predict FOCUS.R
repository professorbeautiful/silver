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
allFocusModels = lapply(focusCategories, function(focus) {
  print(focus)
  try((glm( (FOCUS ==  focus)
            ~  oncotype_in_TiAbKw * years,
            data=bigMerge,
            subset= onlyOneTest & !is.na(bigMerge$FOCUS), 
            family=poisson
  ))
  )
})
names(allFocusModels[[1]])
sapply(allFocusModels, '[', i='deviance')
sapply(allFocusModels, '[', i='null.deviance')
allFocusAnovas = lapply(allFocusModels, anova)
allFocusAnovas
allFocusSummaries = lapply(allFocusModels, summary)
names(allFocusSummaries) = focusCategories

####  No relationships anywhere ####