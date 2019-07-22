
####  Using bigMerge defined below, it will be ok.
allArticles = data.frame(PMID=pmid,
                         titles, abstracts, years,
                         oncotype_in_TiAbKw,
                         mammaprint_in_TiAbKw)

ourNotesFeatures = dataForFocus
ourNotesFeatures$isMoney = isMoney   #### 178.
ourNotesFeatures$isEconomics 

bigMerge =   merge(all = TRUE,
                   allArticles, ourNotesFeatures
)

