allyears = as.numeric(sort(unique(years)))

table(years)

table(mammaprint_in_TiAbKw, oncotype_in_TiAbKw)

table(match(years, allyears))

plotYears = function(subset) {
  if(missing(subset)) subset = pmid
  inSubset = pmid %in% subset
  plot(allyears, as.vector(table(years[inSubset])),
       type='h', col='lightgrey' ,
       xlab='year', ylab='article count')
  offset=0.2
  lwd = 2
  
  for(year in allyears) {
    points(type='h',
           offset+year, 
           sum(years[mammaprint_in_TiAbKw&!oncotype_in_TiAbKw&inSubset]==year),
           col='red', lwd=lwd)
    points(type='h',
           -offset+year, sum(years[oncotype_in_TiAbKw&!mammaprint_in_TiAbKw&inSubset]==year),
           col='green', lwd=lwd)
    points(type='h',
           year, sum(years[mammaprint_in_TiAbKw&oncotype_in_TiAbKw&inSubset]==year),
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
launchPMID()  ### OK. really is MP.

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
savedSeed = .Random.seed
dump('savedSeed', file = 'savedSeed.rda')
pmidSampleArray = tapply(pmid, INDEX=list(mammaprint_in_TiAbKw, 
                        oncotype_in_TiAbKw,
                        years), 
       FUN= pmidSelector)
pmidSample = unlist(pmidSampleArray)
length(pmidSample)
head(pmidSample)[1:2]
pmidSampleSaved = pmidSample
pbcopy(makeHyperlinks(pmidSampleSaved) )
plotYears(pmidSample)
abline(h=8, lty=2)
title(paste('up to ', MaxNumPMID, ' per PMID group'))
