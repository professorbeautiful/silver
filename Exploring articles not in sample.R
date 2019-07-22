##  Exploring articles not in sample ####
# Nothing exciting in here.

titles[oncotype_in_TiAbKw & !mammaprint_in_TiAbKw & !(inSample)]

abstracts[oncotype_in_TiAbKw & !mammaprint_in_TiAbKw & !(inSample)][120]   
###   NCT00310180  TAILORx low risk,   NNT = 1000/13 = 77?   NNT = 1000/ (13/2) = 154

loglin(table(bigMerge$oncotype_in_TiAbKw, bigMerge$mammaprint_in_TiAbKw  
      , bigMerge$FOCUS
      ),
      margin=3
)
