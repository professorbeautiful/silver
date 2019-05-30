library('xml2')

x = read_xml('biomarkers.xml')
str(x)

x[[1]]
xml_contents( xml_contents(xml_children(x))  [1] )

titles = xml_contents(xml_find_all(x, ".//title") )

titles[grep ('mammaprint', xml_contents(xml_find_all(x, ".//label") ) )
]

titles[grep (pattern = 'sensitivity', ignore.case = T, x = xml_contents(xml_find_all(x, ".//abstract") ) )
       ]


      