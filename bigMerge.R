
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
table(bigMerge$oncotype_in_TiAbKw, bigMerge$mammaprint_in_TiAbKw, bigMerge$`Oncotype DX?
Mammaprint?`, dnn=list('odx', 'mp', 'test'), exclude=NULL)
### a few discrepancies:  7 out of 172.  (and 6 have some NA ?)
### We won't worry about that now.

bigMerge$`Oncotype DX?\nMammaprint?`[
  is.na(bigMerge$`Oncotype DX?\nMammaprint?`) 
  & bigMerge$mammaprint_in_TiAbKw 
  & !bigMerge$oncotype_in_TiAbKw] = 
  'Mammaprint only (70 genes)'

bigMerge$`Oncotype DX?\nMammaprint?`[
  is.na(bigMerge$`Oncotype DX?\nMammaprint?`) 
  & !bigMerge$mammaprint_in_TiAbKw 
  & bigMerge$oncotype_in_TiAbKw] = 
  'Oncotype only (21 genes)'

bigMerge$`Oncotype DX?\nMammaprint?`[
  is.na(bigMerge$`Oncotype DX?\nMammaprint?`) 
  & bigMerge$mammaprint_in_TiAbKw 
  & bigMerge$oncotype_in_TiAbKw] = 
  'Both tests'
  
