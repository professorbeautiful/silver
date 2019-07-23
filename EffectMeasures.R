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
    "Net reclassification improvement"
    # "Recurrence free survival",
    # "Disease free survival"
    )
  )  
)

table(
  hasEffectMeasure(c(
    "Positive predictive value",
    "Negative predictive value",
    "Number needed to treat (NNT)",
    "Net reclassification improvement"
    # "Recurrence free survival",
    # "Disease free survival"
  )
  )  ,
  hasEffectMeasure(c(
    # "Positive predictive value",
    # "Negative predictive value",
    # "Number needed to treat (NNT)",
    # "Net reclassification improvement"
    "Recurrence free survival",
    "Disease free survival"
  )
  )  
)

EMusage = sort(decreasing = TRUE,
  apply(
    sapply( unique(fieldChoices$`Effect measures`), hasEffectMeasure),
    2,
    sum)
)


####  plot of effect measure usage ####
plot(EMusage, axes=F, xlab="")
axis(2)
numText = 9
offsetText = 5
yFactor = 2
yText = max(EMusage) - (-1 + 1:numText)* yFactor
text(y = yText, x = (1:numText) + offsetText, names(EMusage) [1:numText],
     pos=4, 
     col=c('black', 'red') 
     [1 + (names(EMusage)=='Net reclassification improvement')]
     )
for(i in 1:numText) lines(x=c(i, i+offsetText), y=c(EMusage[i], yText[i]),
                          col=c('black', 'red') 
                          [1 + (names(EMusage)[i]=='Net reclassification improvement')]
)

patientOrientedEM = c(    "Positive predictive value",
      "Negative predictive value",
      "Number needed to treat (NNT)",
      "Net reclassification improvement"
)
SQUARE = 15
OPENSQUARE = 0
points(which(names(EMusage) %in% patientOrientedEM), 
       EMusage[which(names(EMusage) %in% patientOrientedEM)], 
       col='red', pch=SQUARE, cex=1.6)
# points(which(names(EMusage) %in% patientOrientedEM), 
#        EMusage[which(names(EMusage) %in% patientOrientedEM)], 
#        col='black', pch=OPENSQUARE, cex=1.6)
abline(h=0)
text(x=1, y=2, 'predictive values', col='red', pos=4)
lines(x=c(18, which(names(EMusage)=='Negative predictive value')), 
          y=c(2, 1),
      col='red' 
)
text(x=1, y=0.5, 'number needed to treat', col='red', pos=4)
lines(x=c(24, which(names(EMusage)=='Number needed to treat (NNT)')), 
      y=c(0.5, 0),
      col='red' 
)
title("Effect measure usage\nPatient-oriented in red")
