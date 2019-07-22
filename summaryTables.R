tableFV = table(ourNotes$FOCUS, ourNotes$`Validation/ 
discovery`)

chisq.test(tableFV)

tableOMV = table(ourNotes$`Oncotype DX?
Mammaprint?`, ourNotes$`Validation/ 
discovery`)

chisq.test(tableOMV)
round(digits=1,chisq.test(tableOMV)$stdres)


### computation study sub-type ####
computationSubTypes = fieldChoices$`Study sub-type`[2:6]
patientBehaviorFocus = fieldChoices$`FOCUS (primary endpoint)`[c(12,13,14,15)]
table(ourNotes$FOCUS %in% patientBehaviorFocus, 
      ourNotes$`Study\nsub-type` %in% computationSubTypes, 
      dnn = c('patientBehaviorFocus', 'computation'))
patientOutcomeFocus = fieldChoices$`FOCUS (primary endpoint)`[c(5,9,10,11)]
table(ourNotes$FOCUS %in% patientOutcomeFocus, 
      ourNotes$`Study\nsub-type` %in% computationSubTypes, 
      dnn = c('patientOutcomeFocus', 'computation'))
doctorBehaviorFocus = fieldChoices$`FOCUS (primary endpoint)`[c(2,3,4,17)]
table(ourNotes$FOCUS %in% doctorBehaviorFocus, 
      ourNotes$`Study\nsub-type` %in% computationSubTypes, 
      dnn = c('doctorBehaviorFocus', 'computation'))
