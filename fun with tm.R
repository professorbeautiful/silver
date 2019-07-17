# https://cran.r-project.org/web/views/MachineLearning.html
# https://cran.r-project.org/web/views/NaturalLanguageProcessing.html

#### fun with tm ####

library(tm)
a_corp = VCorpus(VectorSource(abstracts))
a_corp = tm_map(a_corp, removeWords, stopwords("english"))
dtm = DocumentTermMatrix(a_corp)
findFreqTerms(dtm, 15)
findAssocs(dtm, 'economics', 0.5)
dtm = removeSparseTerms(dtm, 0.8)
inspect(weightTf(removeSparseTerms(dtm, 0.8)))
str(dtm)
table(dtm$v)
dtm[10,21:23]
dimnames(dtm)[[2]][421:423]

library(deepnet)
library(googlesheets)
### See also our file "Abstract Isabel Rivera Plata.Rmd"
sheetURL =   'https://docs.google.com/spreadsheets/d/141nlDIfnPdSvGSHgLPen9_QpVJlA0vwrvKc4y9fh6dc'
ourSheet = googlesheets::gs_url(sheetURL)
ourNotes = googlesheets::gs_read(ourSheet, ws=1) 
View(ourNotes)
ourNotes = ourNotes[-1, ]   #first row is mistaken.
fieldChoices = googlesheets::gs_read(ourSheet, ws=2)
fieldChoices = fieldChoices[-1, ]
focusWords = fieldChoices$`FOCUS (primary endpoint)`
focusWords = unique(unlist(strsplit(focusWords, split='[ :]')))
focusWords = focusWords[!is.na(focusWords)]
focusWords = focusWords[nchar(focusWords)>2]
length(focusWords)  ## 40 for FOCUS.

#### incidence counts for titles
titleFocusMatrix = as.data.frame(sapply(
  focusWords,
  function(word)
    as.vector(sapply(titles, function(thisTitle) 
      sum(gregexpr(text=thisTitle, pattern=word, ignore.case = TRUE)[[1]]
          > 0) ) )
)  )
rownames(titleFocusMatrix) = pmid
View(titleFocusMatrix)
sapply(titleFocusMatrix, table)

names(ourNotes)
table(ourNotes$FOCUS)

#### logistic regression ####
dataForEconomics = data.frame(isEconomics=ourNotes$FOCUS=='Economics', 
                titleFocusMatrix[pmidSample, ])
### check it!  
dataForEconomics
fullModelForEconomics = glm(,
                            isEconomics ~ ., family=binomial)
                            step()
