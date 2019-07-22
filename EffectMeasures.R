#### EffectMeasures.R ####

effectMeasureNames = fieldChoices$`Effect measures`

hasEffectMeasure = function(EM)
  sapply((ourNotes$`Effect measure\n#1`==EM) |
           (ourNotes$`Effect measure\n#2`==EM) |
           (ourNotes$`Effect measure\n#3`==EM),
         isTRUE)
isMoney = hasEffectMeasure('Money')
table(isMoney)
t(table(isMoney, ourNotes$FOCUS))


