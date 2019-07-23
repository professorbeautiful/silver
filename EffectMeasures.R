#### EffectMeasures.R ####

effectMeasureNames = fieldChoices$`Effect measures`

hasEffectMeasure = function(EM)
  sapply((ourNotes$`Effect measure\n#1`  %in%  EM) |
           (ourNotes$`Effect measure\n#2`   %in%  EM) |
           (ourNotes$`Effect measure\n#3`   %in%  EM),
         isTRUE)

isMoney = hasEffectMeasure('Money')
table(isMoney)
t(table(isMoney, ourNotes$FOCUS))

table(
  hasEffectMeasure(c(
    "Positive predictive value",
    "Negative predictive value",
    "Number needed to treat (NNT)",
    "Net reclassification improvement",
    "Recurrence free survival",
    "Disease free survival")
  )  
)


