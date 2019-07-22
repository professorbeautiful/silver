# https://cran.r-project.org/web/views/MachineLearning.html
# https://cran.r-project.org/web/views/NaturalLanguageProcessing.html

#### fun with tm ####

# library(tm)
# a_corp = VCorpus(VectorSource(abstracts))
# a_corp = tm_map(a_corp, removeWords, stopwords("english"))
# dtm = DocumentTermMatrix(a_corp)
# findFreqTerms(dtm, 15)
# findAssocs(dtm, 'economics', 0.5)
# dtm = removeSparseTerms(dtm, 0.8)
# inspect(weightTf(removeSparseTerms(dtm, 0.8)))
# str(dtm)
# table(dtm$v)
# dtm[10,21:23]
# dimnames(dtm)[[2]][421:423]
# 
# library(deepnet)
