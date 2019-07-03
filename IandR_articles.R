###  We need to re-read this file and reconstruct IR each time?
library('xml2')
IR = read_xml('pubmed_result oncotype or mammaprint.xml')
str(IR)
IR[[1]]
write_xml(IR, 'exporting-from-R.xml', format_whitespace=TRUE)

record_nodes = xml_find_all(IR, ".//PubmedArticle")
length(record_nodes) 
###  Lengths of the 662 record nodesets.
table(
  sapply(FUN = length, sapply(record_nodes, 
                              function(node)xml_children(xml_children(node))) )
)
# 9  10  11  12  13  14  15  16  17  18 
# 1   4   1  11  16  49 211 370   4   1 


### Careful here.
get_nodes <- function(path=".//label") {
  sapply(record_nodes, xml_find_all, path) 
}

get_node_contents <- function(path=".//label", do.unlist=TRUE,
                              selectFirst = FALSE) {
  the_nodes = get_nodes(path)
  if(selectFirst)
    contents = sapply(the_nodes, 
                      function(node)
                        xml_contents(node)[1] )
  else 
    contents = sapply(the_nodes, 
                      function(node)
                          paste(collapse='', as.vector(xml_contents(node) )) )
  empty = sapply(contents, length)==0
  contents[empty] = ''
  cat(length(contents) ,'\n')  ### OK,  668.
  result = sapply(contents, as.character)
  if(do.unlist) result = unlist(result)
  result
}

##### TITLES ####
title_nodes = get_nodes(".//ArticleTitle")
table(sapply(get_node_contents(".//ArticleTitle", do.unlist = F) , length) )
title_list = get_node_contents(".//ArticleTitle", do.unlist = F)
title_list_lengths = sapply( title_list, length) 
table(title_list_lengths)
titles = sapply(title_list, paste0, collapse = '')
str(titles)

#### ABSTRACTS ####
abstracts = get_node_contents(".//abstract")
length(abstracts)

oncotype_in_Ti= regexpr(pattern='oncotype|21 gene|21-gene', text = titles, 
                        ignore.case=TRUE) > 0
oncotype_in_Ab= regexpr(pattern='oncotype|21 gene|21-gene', text = abstracts, 
                        ignore.case=TRUE) > 0
oncotype_in_TiAb = oncotype_in_Ti | oncotype_in_Ab
mammaprint_in_Ti= regexpr(pattern='mammaprint|70 gene|70-gene', text = titles, 
                        ignore.case=TRUE) > 0
mammaprint_in_Ab= regexpr(pattern='mammaprint|70 gene|70-gene', text = abstracts, 
                        ignore.case=TRUE) > 0
mammaprint_in_TiAb = mammaprint_in_Ti | mammaprint_in_Ab

#### YEARS ####

year_nodes = get_nodes(".//PubMedPubDate//Year")
years = get_node_contents(".//PubMedPubDate//Year", selectFirst=TRUE)
sum(table(years))
table(years, exclude=NULL)  
### 24 article have no pub year but do have PubMedPubDate year.
months = get_node_contents(".//PubMedPubDate//Month")
table(months)
sum(months=='')
days = get_node_contents(".//PubMedPubDate//Day")
sum(days=='')
library(lubridate)
pubMedPubDate = ymd(
  apply(
    cbind(years,months,days),
    MARGIN = 1, FUN = paste, collapse='-'))


#### PMID ####

pmid = get_node_contents('.//PMID')
pmid_url = paste0('https://www.ncbi.nlm.nih.gov/pubmed/?term=',
                  pmid, '%5Bpmid%5D')
table(nchar(pmid))
table(table(pmid))


####   Which are review articles?
####  Which are clinical articles (patient data focus)?
####   ....

#### TODO:  separate into two piles silver (Isabel) and brass (Roger).
####  Select only clinical papers.
####  rank order, essential ones first (early; randomized), others. 

assignments = sample(rep(c('_Silver', '_Brass'), each=length(abstracts)/2),
                      replace=FALSE)
table(assignments)  ### 334 each.


#Finally, rewrite the xml doc.
write_xml(IR, 'exporting-from-R.xml', format_whitespace=TRUE)
### The written document looks good.
### But Mendeley is not doing the re-import correctly yet.
write_xml(IR, 'exporting-from-R.xml')


##### EXPORTING #####
library(openxlsx)
IR.df = data.frame(pmid_url, titles, abstracts, pmid, years,
                   mammaprint_in_TiAb, oncotype_in_TiAb)
write.csv(IR.df, file = 'IR.df.csv')
write.table(IR.df, file = 'IR.df.tabsv.xls', sep='\t', row.names = F)
#  
system('cat IR.df.tabsv.xls | pbcopy')  ### Works in MacBook.
#system('open IR.df.tabsv.xls')
