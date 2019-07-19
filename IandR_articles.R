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
IR <- (easyPubMed::fetch_pubmed_data(otc_on_pubmed))
IR <- xml2::read_xml(IR)

#write_xml(IR, 'exporting-from-R.xml', format_whitespace=TRUE)

#### Read IR from disk (no re-loading from web is needed) ####
IR = xml2::read_xml('exporting-from-R.xml')

allNodeNames = sort(unique(xml2::xml_name(xml_find_all(x = IR, xpath = './/*'))) )

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


## Obtaining FULL TEXT addresses ####
###   PMC DOI and PII strings.
EIdType_nodes = xml2::xml_find_all(IR, xpath='//@EIdType')
# grep 'IdType="pmc"' exporting-from-R.xml |wc  #  314 
# grep 'IdType="doi"' exporting-from-R.xml |wc  #  739!  
# grep 'IdType="pii"' exporting-from-R.xml |wc  #  326!  
IdType_nodes = xml2::xml_find_all(IR, xpath='//@IdType')
sort(unique(as.character(IdType_nodes)))
### PMC addresses ####
PMCnodes = 
  IdType_nodes[as.character(IdType_nodes)==" IdType=\"pmc\""]
length(PMCnodes)   ### 314
PMCstrings = sapply(PMCnodes, function(node) xml_text(xml_parent(node)))
PMCurls = rep("", length(pmid))
names(PMCurls) = pmid
pmidsForPMCs = sapply(PMCnodes, function(node)
  xml_text(xml_find_first(xml_parents(node)[[4]], './/PMID')))
PMCurls[pmidsForPMCs] = paste(sep='/', 
                              'https://www.ncbi.nlm.nih.gov/pmc/articles',
                              PMCstrings)
sum(PMCurls!='')   ### 314, as expected.
titles[250]
browseURL(PMCurls[250])
#### DOI addresses Digital Object Identifier ####
DOInodes = 
  EIdType_nodes[as.character(EIdType_nodes)
                    ==" EIdType=\"doi\""]
length(DOInodes)   ### 351
## articles corresponding to DOInodes: ####
# First, the pmid corresponding to a DOI node:
pmidsForDOIs = sapply(DOInodes, function(node)
  xml_text(xml_find_first(xml_parents(node)[[4]], './/PMID')))
DOIurls = rep("", length(pmid))
names(DOIurls) = pmid
DOIstrings = sapply(DOInodes, function(node) xml_text(xml_parent(node)))
DOIurls[pmidsForDOIs] = paste(sep='/', 'https://doi.org', DOIstrings)
sum(DOIurls!='')   ### 351, as expected.
## Spot check
titles[300]
browseURL(DOIurls[300])  ### OK for 100,200,300  
table(DOIurls!='')
### For the record, the following things don't work.
  # xml2::xml_find_all(IR, xpath='//@IdType="pmc"')
  # xml2::xml_find_all(IR, xpath="//@IdType='pmc'")
  # xml2::xml_find_all(IR, xpath="//@IdType=pmc")
  #  XML::getNodeSet(IR, path='@IdType="pmc"')
  #  DOIparent = xmlAncestors(DOInodes[[1]])
### CAUTION: FOR DOI'S generally have to follow redirects. ####
### browseURL will work, but:  
weNeedDOI = PMCurls=='' & DOIurls!=''    ### 64 of them.
doiRetrievals = sapply(DOIurls[weNeedDOI], 
                      function(DOIurl) RCurl::getURL(DOIurl)  )  ### Redirections
### We only need them for the ones PMC doesn't have.
### so, we will have to scrape out the correct URL from the initial fetch.
DOIrealURLs = sapply(doiRetrievals,
                     function(doi1)
                       gsub("Handle Redirect", '', xml_text(read_xml(doi1))) )

### check out the pii nodes too.  ####
#### DOI addresses ####
DOInodes = 
  EIdType_nodes[as.character(EIdType_nodes)
                ==" EIdType=\"doi\""]
length(DOInodes)   ### 351
## articles corresponding to DOInodes: ####
# First, the pmid corresponding to a DOI node:
pmidsForDOIs = sapply(DOInodes, function(node)
  xml_text(xml_find_first(xml_parents(node)[[4]], './/PMID')))
DOIurls = rep("", length(pmid))
names(DOIurls) = pmid
DOIstrings = sapply(DOInodes, function(node) xml_text(xml_parent(node)))
DOIurls[pmidsForDOIs] = paste(sep='/', 'https://doi.org', DOIstrings)
sum(DOIurls!='')   ### 351, as expected.

#### PII addresses ####
### https://en.wikipedia.org/wiki/Publisher_Item_Identifier
PIInodes = 
  EIdType_nodes[as.character(EIdType_nodes)
                ==" EIdType=\"pii\""]
length(PIInodes)   ### 36
## articles corresponding to PIInodes: ####
# First, the pmid corresponding to a PII node:
pmidsForPIIs = sapply(PIInodes, function(node)
  xml_text(xml_find_first(xml_parents(node)[[4]], './/PMID')))
PIIurls = rep("", length(pmid))
names(PIIurls) = pmid
PIIstrings = sapply(PIInodes, function(node) xml_text(xml_parent(node)))
PIIurls[pmidsForPIIs] = paste(sep='/', 
                              'http://linkinghub.elsevier.com/retrieve/pii/', PIIstrings)
sum(PIIurls!='')   ### 36, as expected.
browseURL(PIIurls[pmidsForPIIs[1]])
#### Not clear yet how to use PIIs for full text.


## Consolidate the full text urls ####
table(PMCurls!='', DOIurls!='') ### still 29 missing
pmid[PMCurls=='' & DOIurls==''] 
## For the first one, there IS a full text url: https://docs.google.com/viewer?url=https%3A%2F%2Fwww.jbuon.com%2Farchive%2F23-5-1297.pdf&pdf=true
browseURL(pmid_url[pmid==pmid[PMCurls=='' & DOIurls==''] [1] ])
titles[pmid==pmid[PMCurls=='' & DOIurls==''] [1]]  ### ok, right one.
## This one I can't extract the full text link from the pubmed page.
## Let's give up on the 29 missing

fulltextURLs = PMCurls
fulltextURLs[weNeedDOI] = DOIrealURLs[weNeedDOI] 
table(fulltextURLs=='')  ### yup, 29 missing.

wordIncidence = function(word, this_pmid=pmid[1],
                         check=c('titles', 'abstracts', 'fulltextURLs'),
                         ...) {
  this_pmid_number= match(this_pmid, pmid)
  answers = list()
  if(!is.na(pmatch('t', check)) )
    answers[['t']] = regexpr(word, titles[this_pmid_number], ...)
  if(!is.na(pmatch('a', check)) )
    answers[['a']] = regexpr(word, abstracts[this_pmid_number], ...)
  if(!is.na(pmatch('f', check)) ) {
    fulltext = try(RCurl::getURLContent(fulltextURLs[this_pmid_number]) )
    answers[['f']] = regexpr(word, fulltext, ...)
    if(length(this_pmid) == 1) ### write the fulltext to .GlobalEnv
      fulltext <<- fulltext
  }
  return(answers)
}

wordIncidence(word = 'Oncotype')
### first pmid:  hits only in full text
browseURL(fulltextURLs[1])
### Aha, so this is https://www.journalslibrary.nihr.ac.uk/hta/hta23300#/abstract
## which is a full report site. Still need to scrape the 
## The string oncotype appears in abstract only because we removed the italics!
##  and Oncotype does not.... not capitalized in the abstract!
## And, lo, OncotypeDX appears in metadata for the page!
substr(fulltext[1], start = 16000, stop=16090)



