library(googlesheets)
### See also our file "Abstract Isabel Rivera Plata.Rmd"
sheetURL =   'https://docs.google.com/spreadsheets/d/141nlDIfnPdSvGSHgLPen9_QpVJlA0vwrvKc4y9fh6dc'
ourSheet = googlesheets::gs_url(sheetURL)
ourNotes = googlesheets::gs_read(ourSheet, ws=1) 
View(ourNotes)
ourNotes = ourNotes[-1, ]   #first row is mistaken.


names(ourNotes)
ourNotes$notRelevant = (ourNotes$STATUS=='DONE; NOT RELEVANT' | ourNotes$FOCUS=='NOT RELEVANT')
ourNotes$Done = !is.na(ourNotes$STATUS) & (ourNotes$STATUS!='in progress...')
table(ourNotes$notRelevant, exclude=NULL)
table(ourNotes$Done, exclude=NULL)
ourNotes[is.na(ourNotes$notRelevant & !is.na(ourNotes$Done)), 'PMID']
ourNotes = ourNotes[!ourNotes$notRelevant, ]

fieldChoices = googlesheets::gs_read(ourSheet, ws=2)
fieldChoices = fieldChoices[-1, ]
