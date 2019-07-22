#### Augmenting test ####

augmentingTable = table(ourNotesFeatures$FOCUS %in%
        c('Augmenting test with other features',
          'Feasibility of doing test'),
      ourNotes$`Oncotype DX?
Mammaprint?`
)

augmentingTable[ , -1]
chisq.test(augmentingTable[ , -1])$expected

## Mammaprint has more 'plasticity', more likely to encourage small tweaks. 
## Appears false.
