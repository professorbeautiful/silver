allyears = as.numeric(sort(unique(years)))

table(years)

table(mammaprint_in_TiAbKw, oncotype_in_TiAbKw)

table(match(years, allyears))

ouryears = allyears[allyears<2019]
plotYears = function(subset, summaryFunction=sum,
                     .ylab='article count', ...) {
  if(missing(subset)) subset = pmid
  inSubset = pmid %in% subset
  ### we need to omit 2019. not complete.
  yeartable = sapply(ouryears, function(y)
    summaryFunction(years[inSubset] == y))
  ## as.vector(table(years[inSubset]))
  plot(ouryears, yeartable,
       type='h', col='lightgrey' ,
       xlab='year', ylab=.ylab,
       ...)
  offset=0.2
  lwd = 2
  
  for(year in ouryears) {
    points(type='h',
           offset+year, 
           summaryFunction(years[mammaprint_in_TiAbKw&!oncotype_in_TiAbKw&inSubset]==year),
           col='red', lwd=lwd)
    points(type='h',
           -offset+year, sum(years[oncotype_in_TiAbKw&!mammaprint_in_TiAbKw&inSubset]==year),
           col='green', lwd=lwd)
    points(type='h',
           year, summaryFunction(years[mammaprint_in_TiAbKw&oncotype_in_TiAbKw&inSubset]==year),
           col='blue', lwd=lwd)
    lines(c(year-offset,year+offset), c(0,0))
  }
  legendColors = c('green', 'red', 'blue', 'grey')
  legend('topleft',
         legend=c('ODX', 'MP', 'both', 'total') , 
         lwd=lwd,
         text.col=legendColors,
         col=legendColors
  )
}
plotYears()
plot(ecdf(pubMedPubDate))


#### why 2002 mammaprint and why 2020?
titles[years<=2002]
abstracts[years==2002]  ###  Yes, 
pmid[years==2002]  ###  Yes, 
launchPMID = function(thispmid='12490681') 
  browseURL(pmid_url[match(thispmid, pmid)])
#launchPMID()  ### OK. really is MP.

titles[years<=2004] ### must remove "metalloproteinase-21 gene" hits!

selectionTable = table( mammaprint_in_TiAbKw, 
                        oncotype_in_TiAbKw,
                        years)
tapply(pmid, INDEX=list(mammaprint_in_TiAbKw, 
                        oncotype_in_TiAbKw,
                        years), 
       FUN= head, n=1)
pmidSelector = function(X)
  sample(X, min(MaxNumPMID, length(X)), 
         replace = FALSE)
MaxNumPMID = 8  #### 10->243 articles, 9->231, 8->218
#savedSeed = .Random.seed
#dump('savedSeed', file = 'savedSeed.rda')
source(file = 'savedSeed.rda', local=TRUE)
.Random.seed = savedSeed 
pmidSampleArray = tapply(pmid, INDEX=list(mammaprint_in_TiAbKw, 
                        oncotype_in_TiAbKw,
                        years), 
       FUN= pmidSelector)
pmidSample = unlist(pmidSampleArray)
length(pmidSample)
head(pmidSample)[1:2]
pmidSampleSaved = pmidSample
inSample = pmid %in% pmidSample
### To get the row in the spreadsheet, add 1.
sampleIndex = function(indexInFullSet)
  sum(inSample[1:indexInFullSet])
pbcopy(makeHyperlinks(pmidSampleSaved) )
plotYears(pmidSample)
abline(h=8, lty=2)
title(paste('up to ', MaxNumPMID, ' per PMID group'))


mammaPrintByYear = selectionTable[2, 1, ]
oncotypePrintByYear = selectionTable[1, 2, ]
cbind(oncotypePrintByYear)
yearMerge = merge(
  data.frame(mp = mammaPrintByYear, year = as.numeric(names(mammaPrintByYear))),
  data.frame(odx = oncotypePrintByYear, year = as.numeric(names(oncotypePrintByYear)))
)
dataForYearStudy = data.frame(count= unlist(yearMerge[-1]))
dataForYearStudy$year = c(yearMerge$year,yearMerge$year)
dataForYearStudy$test = rep(c('mp', 'odx'), each=17)
dataForYearStudy = dataForYearStudy[dataForYearStudy$year<2019,]
# lines(dataForYearStudy$year[dataForYearStudy$test=='mp'], 
#       dataForYearStudy$count[dataForYearStudy$test=='mp'], col='red')
# lines(dataForYearStudy$year[dataForYearStudy$test=='odx'], 
#       dataForYearStudy$count[dataForYearStudy$test=='odx'], col='green')
glm.out.dataForYearStudy = glm(
  data=dataForYearStudy,
  subset= (year < 2019),
  count ~ poly(degree = 2, year * test), family=poisson)
summary(glm.out.dataForYearStudy)
predictions = exp(predict(glm.out.dataForYearStudy))
plotYears()
lines(dataForYearStudy$year[dataForYearStudy$test=='mp'], 
      predictions[dataForYearStudy$test=='mp'], col='red', lwd=2)
lines(dataForYearStudy$year[dataForYearStudy$test=='odx'], 
      predictions[dataForYearStudy$test=='odx'], col='green', lwd=2)
title('Poisson regression')


for(focus in focusCategories) {
  subset = bigMerge$PMID[sapply(bigMerge$FOCUS==focus, isTRUE)]
  cat(focus, ' ', length(subset), '\n')
  if(length(subset) > 10) {
    plotYears(subset)
    title(paste0(focus, ' (N=', length(subset), ')') )
  }
}


