tableFV = table(ourNotes$FOCUS, ourNotes$`Validation/ 
discovery`)

chisq.test(tableFV)

tableOMV = table(ourNotes$`Oncotype DX?\nMammaprint?`, 
                 ourNotes$`Validation/ \ndiscovery`)

chisq.test(tableOMV)
round(digits=1,chisq.test(tableOMV)$stdres)


### computation study sub-type ####
computationSubTypes = fieldChoices$`Study sub-type`[2:6]
patientBehaviorFocus = fieldChoices$`FOCUS (primary endpoint)`[c(12,13,14,15)]
table(ourNotes$FOCUS %in% patientBehaviorFocus, 
      ourNotes$`Study\nsub-type` %in% computationSubTypes, 
      dnn = c('patientBehaviorFocus', 'computation'))
patientOutcomeFocus = fieldChoices$`FOCUS (primary endpoint)`[c(5,9,10,11)]
table(ourNotes$FOCUS %in% patientOutcomeFocus, 
      ourNotes$`Study\nsub-type` %in% computationSubTypes, 
      dnn = c('patientOutcomeFocus', 'computation'))
doctorBehaviorFocus = fieldChoices$`FOCUS (primary endpoint)`[c(2,3,4,17)]
table(ourNotes$FOCUS %in% doctorBehaviorFocus, 
      ourNotes$`Study\nsub-type` %in% computationSubTypes, 
      dnn = c('doctorBehaviorFocus', 'computation'))

#### proportion doctorBehaviorFocus by year
length(pmidSample)
pmidSampleRelevant = ourNotes$PMID
ourNotes = merge(ourNotes, data.frame(PMID=pmid, year=years))

plotProportions = function(subset, feature=doctorBehaviorFocus, 
                           summaryFunction=mean,
                     .ylab='proportion', 
                     legendLocation = 'topleft',
                     ...) {
   featureName = deparse(substitute(feature))
   print(featureName)
   if(missing(subset)) subset = pmid
   inSubset = pmid %in% subset
   ### we need to omit 2019. not complete.
   featureByYear = split(ourNotes$FOCUS %in% feature, 
                                     ourNotes$year)
   
   featureProportion = sapply(featureByYear, mean)
   plot(allyears, featureProportion, 
        type='h', col='lightgrey' ,
        xlab='year', ylab=.ylab,
        ylim = c(0,1), 
        ...
   )
   title(featureName)
   lines(allyears,
         inverseLogit(
            predict(glm(featureProportion ~ as.numeric(names(featureProportion)),
                        family=binomial))
         ),
         col='lightgrey'
   )
   featureByYearAndTest = split(ourNotes$FOCUS %in% feature, 
                         list(ourNotes$year, 
                              ourNotes$`Oncotype DX?\nMammaprint?`)
   )
   
   featureProportionByYearAndTest = sapply(featureByYearAndTest, mean)
   
   lwd = 2
   offset=0.2
   for(year in ouryears) {
      for(test in unique(ourNotes$`Oncotype DX?\nMammaprint?`)) {
         if(is.na(test)) break
         thisData = featureProportionByYearAndTest[[paste(sep='.',
                                   year,test)]]
         color = switch(test, 
                        "Mammaprint only (70 genes)" = 'red',
                        "Oncotype only (21 genes)"  = 'green',
                        "Both tests" = 'blue')
         thisOffset = offset * 
            switch(test, 
                   "Mammaprint only (70 genes)" = +1,
                   "Oncotype only (21 genes)"  = -1,
                   "Both tests" = 0)
         points(type='h',
                thisOffset+year, 
                mean(thisData),
                col=color, lwd=lwd)
      }
      lines(c(year-offset,year+offset), c(0,0))
   }
   legendColors = c('green', 'red', 'blue', 'grey')
   legend(legendLocation,
          legend=c('ODX', 'MP', 'both', 'total') , 
          lwd=lwd,
          text.col=legendColors,
          col=legendColors
   )
}

plotProportions()
plotProportions(feature = patientBehaviorFocus)
plotProportions(feature = patientOutcomeFocus, legendLocation='topright')
