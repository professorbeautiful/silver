###  We need to re-read this file and reconstruct IR each time?

library(pubmed.mineR)
library(easyPubMed)
library(rentrez)
library(xml2)
library(XML)
#IR = read_xml('pubmed_result oncotype or mammaprint.xml')
#str(IR)
#IR[[1]]

exclusions21 = c('ORF','IL', 'Interleukin', 'FGF', 'kinase',
                 'kappa', 'mir', 'microrna', 'ighv3', 'factor', 'trisomy',
                 'chromosome')
exclusion21clause = paste0('"', exclusions21, ' 21"[title/abstract]', collapse=' NOT ')

queryOncotype  = paste('
"loattrfree full text"[sb] 
 NOT review[filter]
 AND English[lang]
 AND breast[title/abstract] 
 AND (cancer[title/abstract] or carcinoma[title/abstract])
 AND (mammaprint [title/abstract] 
      OR 70-gene[title/abstract] 
      OR oncotype[title/abstract] 
      OR ("21-gene"[title/abstract] NOT ',
       exclusion21clause,
       ' NOT 2.7.1.21',
  ') )'
)

query = gsub('\n', ' ', queryOncotype)
otc_on_pubmed <- easyPubMed::get_pubmed_ids(query)
IR <- read_xml(fetch_pubmed_data(otc_on_pubmed))
write_xml(IR, 'exporting-from-R.xml', format_whitespace=TRUE)

record_nodes = xml_find_all(IR, ".//PubmedArticle")
length(record_nodes) 
###  Lengths of the 407 (NOT 662) record nodesets.
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
                              selectFirst = FALSE, collapse='') {
  the_nodes = get_nodes(path)
  if(selectFirst)
    contents = sapply(the_nodes, 
                      function(node)
                        xml_contents(node)[1] )
  else 
    contents = sapply(the_nodes, 
                      function(node)
                          paste(collapse=collapse, as.vector(xml_contents(node) )) )
  empty = sapply(contents, length)==0
  contents[empty] = ''
  cat(length(contents) ,'\n')  ### OK,  668.
  result = sapply(contents, as.character)
  if(do.unlist) result = as.vector(unlist(result))
  result
}

##### TITLES ####
title_nodes = get_nodes(".//ArticleTitle")
table(sapply(get_node_contents(".//ArticleTitle", do.unlist = F) , length) )
title_list = get_node_contents(".//ArticleTitle", do.unlist = F)
title_list_lengths = sapply( title_list, length) 
table(title_list_lengths)
titles = sapply(title_list, paste0, collapse = '')
titles = gsub('<i>|</i>', '', titles)
titles = as.vector(titles)
str(titles)

#### ABSTRACTS ####
abstract_nodes = get_nodes(".//AbstractText")
table(sapply(get_node_contents(".//AbstractText", do.unlist = F) , length) )
abstract_list = get_node_contents(".//AbstractText", do.unlist = F)
abstract_list_lengths = sapply( abstract_list, length) 
table(abstract_list_lengths)

abstracts = sapply(abstract_list, paste0, collapse = '')
abstracts = as.vector(get_node_contents(".//AbstractText"))
abstracts = gsub('<i>|</i>', '', abstracts)
abstracts = as.vector(abstracts)
str(abstracts)

#### KEYWORDS ####
keywords = get_node_contents('.//Keyword', collapse=';')

#### Concatenating titles, abstracts, keywords ####
ti_ab_kw = apply(cbind(titles, abstracts, keywords), 1,
                 paste, sep='|||')
## This shows we really do need our eyeballs!
titles[grep('sensitivity', ignore.case = T, titles)]
###  But this kind of query could be a good check on our eyeballs.
titles[grep('specificity', ignore.case = T, abstracts)]


#### PMID ####

pmidFull = get_node_contents('.//PMID')
pmid = pmidFirst = get_node_contents('.//PMID', selectFirst = TRUE)
pmid_url = paste0('https://www.ncbi.nlm.nih.gov/pubmed/?term=',
                  pmid, '%5Bpmid%5D')
table(nchar(pmid))
table(table(pmid))
makeHyperlinks = function(pmidlist) paste0(
  '=HYPERLINK("https://www.ncbi.nlm.nih.gov/pubmed/?term=',
  pmidlist, '%5Bpmid%5D", "', pmidlist, '")')
hyperlinks = makeHyperlinks(pmidlist = pmid)
grep('31174485', hyperlinks, v=T)
print('=HYPERLINK("https://www.ncbi.nlm.nih.gov/pubmed/?term=31174485%5Bpmid%5D","31174485")')


#### WHICH TESTS ####
oncotype_in_Ti= regexpr(pattern='oncotype|21 gene|21-gene', text = titles, 
                        ignore.case=TRUE) > 0
oncotype_in_Ab= regexpr(pattern='oncotype|21 gene|21-gene', text = abstracts, 
                        ignore.case=TRUE) > 0
oncotype_in_Kw= regexpr(pattern='oncotype|21 gene|21-gene', text = keywords, 
                        ignore.case=TRUE) > 0
oncotype_in_TiAbKw = oncotype_in_Ti | oncotype_in_Ab | oncotype_in_Kw
mammaprint_in_Ti= regexpr(pattern='mammaprint|70 gene|70-gene', text = titles, 
                        ignore.case=TRUE) > 0
mammaprint_in_Ab= regexpr(pattern='mammaprint|70 gene|70-gene', text = abstracts, 
                        ignore.case=TRUE) > 0
mammaprint_in_Kw= regexpr(pattern='mammaprint|70 gene|70-gene', text = keywords, 
                          ignore.case=TRUE) > 0
mammaprint_in_TiAbKw = mammaprint_in_Ti | mammaprint_in_Ab | mammaprint_in_Kw

#### Exploring problems with the TiAb classification.
table(mammaprint_in_TiAbKw,oncotype_in_TiAbKw)

titles[(!mammaprint_in_TiAbKw) & (!oncotype_in_TiAbKw)] [1] 
abstracts[(!mammaprint_in_TiAbKw) & (!oncotype_in_TiAbKw)] [1] 
keywords[(!mammaprint_in_TiAbKw) & (!oncotype_in_TiAbKw)] [1] 
pmid[(!mammaprint_in_TiAbKw) & (!oncotype_in_TiAbKw)] [1] 
### This one is selected by "21 gene" in the abstract, but it is a DIFFERENT set of genes!
#DONE!
#pmid_to_remove = pmid[(!mammaprint_in_TiAbKw) & (!oncotype_in_TiAbKw)] [1] 

#### YEARS ####

year_nodes = get_nodes(".//PubMedPubDate//Year")
years = get_node_contents(".//PubMedPubDate//Year", selectFirst=TRUE)
sum(table(years))
table(years, exclude=NULL)  
### 24 article have no pub year but do have PubMedPubDate year.
months = get_node_contents(".//PubMedPubDate//Month", selectFirst=TRUE)
table(months)
sum(months=='')
days = get_node_contents(".//PubMedPubDate//Day", selectFirst=TRUE)
sum(days=='')
library(lubridate)
pubMedPubDate = ymd(
  apply(
    cbind(years,months,days),
    MARGIN = 1, FUN = paste, collapse='-'))


#### PMID ####

pmidFull = get_node_contents('.//PMID')
pmid = pmidFirst = get_node_contents('.//PMID', selectFirst = TRUE)
pmid_url = paste0('https://www.ncbi.nlm.nih.gov/pubmed/?term=',
                  pmid, '%5Bpmid%5D')
table(nchar(pmid))
table(table(pmid))

makeHyperlinks = function(pmidlist) paste0(
  '=HYPERLINK("https://www.ncbi.nlm.nih.gov/pubmed/?term=',
  pmidlist, '%5Bpmid%5D", "', pmidlist, '")')
hyperlinks = makeHyperlinks(pmidlist = pmid)
grep('31174485', hyperlinks, v=T)
print('=HYPERLINK("https://www.ncbi.nlm.nih.gov/pubmed/?term=31174485%5Bpmid%5D","31174485")')


#### Investigate multiple PMIDS ####
# browseURL(paste0('https://www.ncbi.nlm.nih.gov/pubmed/?term=',
#                  "30003141", '%5Bpmid%5D'))
# browseURL(paste0('https://www.ncbi.nlm.nih.gov/pubmed/?term=',
#                  "28721379", '%5Bpmid%5D'))
# browseURL(paste0('https://www.ncbi.nlm.nih.gov/pubmed/?term=',
#                  "12490681", '%5Bpmid%5D'))
# browseURL(paste0('https://www.ncbi.nlm.nih.gov/pubmed/?term=',
#                  "12712998", '%5Bpmid%5D'))
strsplit(pmid[407], split='........', perl=T)

####   Which are review articles?
####  Which are clinical articles (patient data focus)?
####   ....

#### TODO:  separate into two piles silver (Isabel) and brass (Roger).
####  Select only clinical papers.
####  rank order, essential ones first (early; randomized), others. 

assignments = sample(rep(c('_Silver', '_Brass'), each=length(abstracts)/2),
                      replace=FALSE)
assignments = rep(c('Silver', 'Brass'), times=length(abstracts)/2 + 1)
if(length(titles) %% 2 == 1)  ### odd;  slice off the first assignment
  assignments = assignments[-1]
length(assignments)
head(assignments)
table(assignments)
#pbcopy(assignments)

#####  REMOVING pmid_to_remove ####
removeArticle = function(pmids_to_remove) {
  which_to_remove = match(table = pmid, x = pmids_to_remove)
  xml_remove(record_nodes[which_to_remove])
  titles = titles[-which_to_remove]
  abstracts = abstracts[-which_to_remove]
  years = years[-which_to_remove]
  assignments = assignments[-which_to_remove]
  pmid = pmid[-which_to_remove]
  pmid_url = pmid_url[-which_to_remove]
  keywords = keywords[-which_to_remove]
  mammaprint_in_Ti = mammaprint_in_Ti[-which_to_remove]
  mammaprint_in_Ab = mammaprint_in_Ab[-which_to_remove]
  mammaprint_in_Kw = mammaprint_in_Kw[-which_to_remove]
  mammaprint_in_TiAbKw = mammaprint_in_TiAbKw[-which_to_remove]
  oncotype_in_Ti = oncotype_in_Ti[-which_to_remove]
  oncotype_in_Ab = oncotype_in_Ab[-which_to_remove]
  oncotype_in_Kw = oncotype_in_Kw[-which_to_remove]
  oncotype_in_TiAbKw = oncotype_in_TiAbKw[-which_to_remove]
}

#Finally, rewrite the xml doc.
write_xml(IR, 'exporting-from-R.xml', format_whitespace=TRUE)
### The written document looks good.
### But Mendeley is not doing the re-import correctly yet.

##### EXPORTING #####
library(openxlsx)
IR.df = data.frame(pmid_url, titles, abstracts, pmid, years,
                   mammaprint_in_TiAbKw, oncotype_in_TiAbKw)
write.csv(IR.df, file = 'IR.df.csv')
write.table(IR.df, file = 'IR.df.tabsv.xls', sep='\t', row.names = F)
#  
system('cat IR.df.tabsv.xls | pbcopy')  ### Works in MacBook.
#system('open IR.df.tabsv.xls')
