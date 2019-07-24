
### 
grep("NCT0", abstracts)  ### 13 of them.
NCTabstracts = grep("NCT0", abstracts, v=T)  ### 13 of them.
### Without the '0', you pick up 3 that are 'neoadjuvant therapy' 
NCT_hits = regexpr("NCT0", abstracts)
NCT_hits = NCT_hits[NCT_hits>0]
## For context:
substring(NCTabstracts, NCT_hits-20, last = NCT_hits+20)
## the NCTnumbers:
NCTnumbers = sort(unique(substring(NCTabstracts, NCT_hits, last = NCT_hits+10) ))
## 8 different studies.

CTgovRecords = 
  sapply(NCTnumbers,
         function(num) {
           CTgovXMLstring = RCurl::getURL(
             paste0(
               "https://clinicaltrials.gov/ct2/results?",
               'show_xprt=Y&xprt=', num, '&displayxml=true&count=200'
             )
           )
           CTgovXML = 
             xml2::read_xml(CTgovXMLstring)
           CTgovAsList = xml2::as_list(CTgovXML)
         }
)


#### end of retrieval for the NCT numbers in our study ####


### General retrieval for oncotype dx ####
# XML::xmlParse
CTgovXMLstring = RCurl::getURL(
  paste0(
  "https://clinicaltrials.gov/ct2/results?",
  'show_xprt=Y&xprt=cancer+AND+',
  "(oncotype+OR+'21-gene')&displayxml=true&count=200"
  )
)
### "21-gene" doesn't add any.
CTgovXML = 
  xml2::read_xml(CTgovXMLstring)

CTgovAsList = xml2::as_list(CTgovXML)
length(CTgovAsList[[1]])  ## 39 oncotype dx, 17 mammaprint, 6 both, total 50
names(CTgovAsList[[1]] )

CTgovAsList[[1]] [['query']] [[1]]

CTs = CTgovAsList[[1]] [-(1:4)]
NCTs = sapply(CTs$clinical_study, function(ct) ct$clinical_study$nct_id[[1]])

length(CTs)  ## 35
CTs[[1]]
names(CTs[[1]])
#browseURL(CTs[[1]]$url[[1]])
xml2::xml_find_all(CTgovXML, ".//title")

### So 35 studies in cancer with oncotype dx.

## Only 3 studies have results, according to ClinicalTrials.Gov:
CTgovXMLresults = 
  xml2::read_xml(#as_html = TRUE,
    RCurl::getURL(paste0(
'https://clinicaltrials.gov/ct2/results?',
#'show_xprt=Y&xprt=',
'displayxml=true',
'cond=breast+cancer&term=oncotype+OR+mammaprint&type=&rslt=With'
#,'&age_v=&gndr=&intr=&titles=&outc=&spons=&lead=&id=&cntry=&state=&city=&dist=&locn=&strd_s=&strd_e=&prcd_s=&prcd_e=&sfpd_s=&sfpd_e=&lupd_s=&lupd_e=&sort='
)
)
)
## Here are the titles. All 3 are for Oncotype DX ####
xml2::xml_find_all(CTgovXMLresults, ".//title")
## Here are all the tags ####
unique(sort(sapply(xml2::xml_find_all(CTgovXMLresults, ".//*"),
       xml2::xml_name)
))
xml2::xml_find_all(CTgovXMLresults, ".//intervention_summary
                   ")
sapply(xml2::xml_text(xml2::xml_find_all(CTgovXMLresults, ".//url")), browseURL,
       browser = '"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"')

###  Below is older stuff. ####

hasIntervention = sapply(
  getNodeSet(CTgovXML, "/search_results/clinical_study"),
       function(node) {
         length(
           getNodeSet(node, "intervention_summary")
           ) > 0
       }
)
#160 do, 26 don't.
CTgovXMLwithIntervention =   getNodeSet(CTgovXML, "/search_results/clinical_study") [which(hasIntervention)]
length(CTgovXMLwithIntervention)  # 160

CTnames =   mvbutils:::cq(nct_id,url,title,status,condition_summary,intervention_summary)
CTframe = lapply(CTnames, 
                       function(attr)
                         gsub(pattern = paste0("</?", attr, ">"), "",
                              sapply(CTgovXMLwithIntervention, function(node)
                                capture.output(getNodeSet(node, print(attr)))
                              ) [3, ]
                         )
  )
CTframe = as.data.frame(CTframe) 
names(CTframe) = CTnames
table(CTframe$status)
table(table(CTframe$intervention_summary))
sum(grep('Mithra', CTframe$intervention_summary))
sum(grep('metformin', ignore.case=TRUE, CTframe$intervention_summary))  # 15
sum(grep('[Tt]ransplant', CTframe$intervention_summary))  # 0

length(grep('Drug:', CTframe$intervention_summary))  # 111

CTframe$intervention_summary_lc = tolower(CTframe$intervention_summary)

druglist = gsub(" $", "", (sort(grep("drug:?", unlist(strsplit(CTframe$intervention_summary_lc, "; ")), v=T))))
druglist = unlist(druglist) ### 193 drug entries,but some comma separated lists
druglist = gsub("drug:?", "", druglist)
druglist = unlist(strsplit(druglist, ', '))
length(druglist)  # ### 218 drug entries
druglist.unique = sort(unique(unlist(druglist))) 
length(druglist.unique)  ## 142 unique drugs

table(table(druglist))
#   1   2   3   4   5   7   9  12 
# 109  18   8   1   2   2   1   1 
table(druglist)[table(druglist) > 1]
drugtable = data.frame(drug=druglist)
rownames(drugtable) = drugtable$drug
drugtable$count[] = table(druglist)

#mk-0752 
#vantictumab (OMP-18R5)

ope = function(filename) system(command = paste("open ", filename))



