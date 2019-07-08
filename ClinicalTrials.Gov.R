
# XML::xmlParse
CTgovXMLstring = RCurl::getURL(
  "https://clinicaltrials.gov/ct2/results?show_xprt=Y&xprt=cancer+AND+oncotype&displayxml=true&count=200"
  )

CTgovXML = 
  xml2::read_xml(CTgovXMLstring)

CTgovAsList = xml2::as_list(CTgovXML)
length(CTgovAsList[[1]])  ## 39
names(CTgovAsList[[1]] )

CTgovAsList[[1]] [['query']] [[1]]

CTs = CTgovAsList[[1]] [-(1:4)]

length(CTs)  ## 35
CTs[[1]]
names(CTs[[1]])
browseURL(CTs[[1]]$url[[1]])
xml2::xml_find_all(CTgovXML, ".//title")


## Studies with results:
CTgovXMLresults = 
  xml2::read_xml(as_html = TRUE,
    RCurl::getURL(paste0(
'https://clinicaltrials.gov/ct2/results?',
'cond=breast+cancer&term=oncotype&type=&rslt=With'
#,'&age_v=&gndr=&intr=&titles=&outc=&spons=&lead=&id=&cntry=&state=&city=&dist=&locn=&strd_s=&strd_e=&prcd_s=&prcd_e=&sfpd_s=&sfpd_e=&lupd_s=&lupd_e=&sort='
)
)
)
xml2::xml_find_all(CTgovXMLresults, ".//title")


###  Below is older stuff.

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
drugtable$count[] = table(druglist))

mk-0752 
vantictumab (OMP-18R5)

ope = function(filename) system(command = paste("open ", filename))
