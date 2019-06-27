library('xml2')

IR = read_xml('importing-to-R.xml')
str(IR)

IR[[1]]
xml_contents( xml_contents(xml_children(IR))  [1] )
length(xml_contents(xml_children(IR)) )  ### 668

xml_contents( xml_contents(xml_children(IR))  [1] )[13]

titles = xml_contents(xml_find_all(IR, ".//title") )
titles[grep ('mammaprint', xml_contents(xml_find_all(IR, ".//label") ) ) ]

tags = xml_contents(xml_find_all(IR, ".//label") )
length(tags)  ### 545 initially
tags_split = strsplit(split=';', gsub('&amp;', '&', tags)  )
length(grep("Isabel&amp;Roger 2019", tags))  ### 500
table( sapply(tags_split, length) )

hits_for_Oncotype_DX= sapply(tags_split, grep, pattern='Oncotype DX') 
hits_for_Oncotype_DX[0==sapply(hits_for_Oncotype_DX, length)] = NA
table( exclude = NULL,
  unlist(hits_for_Oncotype_DX  )
)

grep("Isabel&amp;Roger 2019", tags_split)


length(xml_find_all(IR, ".//record") ) 

###  Lengths of the 668 nodesets.

table(
  sapply(FUN = length, xml_children(IR)[[1]])
)

#### How many articles have abstracts?
abstract = xml_contents(xml_find_all(IR, ".//abstract") )
length(abstract)   #### 627

abstract = (xml_find_all(IR, ".//abstract") )
length(abstract)

abstracts_list = sapply(xml_find_all(IR, ".//record"),
              xml_find_all, 
              xpath=".//abstract") 
abstracts = sapply(abstracts_list, as.character)
length(abstracts)
### replace "abstracts" with no abstract entry at all  with empty strings.
abstracts[[641]]
have_no_abstract = which(  sapply(abstracts, length) == 0) 
abstracts[have_no_abstract] = ""
abstracts[[641]]
abstracts = unlist(abstracts)
grep('oncotype', (abstracts), ignore.case = TRUE)
### this works!

####   Which are review articles?
####  Which are clinical articles (patient data focus)?
####   ....

#### TODO:  separate into two piles silver and brass.
####  Select only clinical papers.
####  rank order, essential ones first (early; randomized), others. 



