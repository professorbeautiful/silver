###  We need to re-read this file and reconstruct IR each time?
library('xml2')
IR = read_xml('importing-to-R.xml')
str(IR)
IR[[1]]
write_xml(IR, 'exporting-from-R.xml', format_whitespace=TRUE)

record_nodes = xml_find_all(IR, ".//record")
length(record_nodes) 
###  Lengths of the 668 record nodesets.
table(
  sapply(FUN = length, sapply(record_nodes, xml_children) )
)
# 9  10  11  12  13  14  15  16  17  18 
# 1   4   1  11  16  49 211 370   4   1 

title_nodes = xml_contents(xml_find_all(IR, ".//title") )
length(title_nodes)   #### They all have a title node.
title_nodes[grep ('mammaprint', xml_contents(xml_find_all(IR, ".//label") ) ) ]
titles = as.character(title_nodes)
titles[1]

### Careful here.
tags = xml_contents(xml_find_all(IR, ".//label") )
length(tags)  ### NO!  545.   We must not skip the ones without tags.
### Better:
tag_nodes = sapply(record_nodes, xml_find_all, ".//label") 
tags = sapply(tag_nodes, xml_contents )
length(tags)   ### OK,  668.
tags = sapply(tags, as.character)

tags_split = strsplit(split=';', gsub('&amp;', '&', tags)  )
length(grep("Isabel&amp;Roger 2019", tags))  ### Only 500
### OK for tags!!!
table( sapply(tags_split, length) )
# 1   2   3   4   5   6   7 
# 126 238 253   4  44   2   1 
## So 126 have only one tag.
hits_for_Oncotype_DX= sapply(tags_split, grep, pattern='Oncotype DX') 
hits_for_Oncotype_DX[0==sapply(hits_for_Oncotype_DX, length)] = NA
table( exclude = NULL,
  unlist(hits_for_Oncotype_DX  )
)

grep("Isabel&amp;Roger 2019", tags_split)


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

#### TODO:  separate into two piles silver (Isabel) and brass (Roger).
####  Select only clinical papers.
####  rank order, essential ones first (early; randomized), others. 

assignments = sample(rep(c('_Silver', '_Brass'), each=length(abstracts)/2),
                      replace=FALSE)
table(assignments)  ### 334 each.

length(tags)
table(nchar(as.character(tags)))
which(nchar(as.character(tags)) == min(nchar(as.character(tags))))  ## 5
tags[90]
### record 90 is  "pmc99" only.

#### Now, let's add the assignments into the tags.
sapply(1:length(record_nodes),
       function(node_number) {
         cat(node_number, ' ')
         newlabel = read_xml(
           paste0('<label> ', assignments[node_number], ';', tags[node_number], '</label>')
         )
         xml_replace(.x=xml_find_all(IR, ".//label")[[node_number]], 
                     .value=newlabel, .copy = TRUE)
       }
)
#Finally, rewrite the xml doc.
