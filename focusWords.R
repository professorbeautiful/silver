
### focusWords ####
focusWords = fieldChoices$`FOCUS (primary endpoint)`
focusWords = unique(unlist(strsplit(focusWords, split='[ :]')))
focusWords = focusWords[!is.na(focusWords)]
focusWords = focusWords[nchar(focusWords)>2]
length(focusWords)  ## 40 for FOCUS.
letsChopTheS = c(13, 27)
focusWords[letsChopTheS] = sub('s$', '', focusWords[letsChopTheS] )
focusWords = c(focusWords, 'cost', 'outcome')

#### incidence counts for titles ####
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

#### incidence counts for abstracts ####
abstractFocusMatrix = as.data.frame(sapply(
  focusWords,
  function(word)
    as.vector(sapply(abstracts, function(thisAbstract) 
      sum(gregexpr(text=thisAbstract, pattern=word, ignore.case = TRUE)[[1]]
          > 0) ) )
)  )
rownames(abstractFocusMatrix) = pmid
View(abstractFocusMatrix)
sapply(abstractFocusMatrix, table)

dim(titleFocusMatrix)
dim(abstractFocusMatrix)
