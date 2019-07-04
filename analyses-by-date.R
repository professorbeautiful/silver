allyears = as.numeric(sort(unique(years)))

table(years)

table(mammaprint_in_TiAbKw, oncotype_in_TiAbKw)

table(match(years, allyears))

plot(allyears, as.vector(table(years)), type='h', col='lightgrey' )
offset=0.2
lwd = 2
for(year in allyears) {
  points(type='h',
         offset+year, sum(years[mammaprint_in_TiAbKw&!oncotype_in_TiAbKw]==year),
         col='red', lwd=lwd)
  points(type='h',
         -offset+year, sum(years[oncotype_in_TiAbKw&!mammaprint_in_TiAbKw]==year),
         col='green', lwd=lwd)
  points(type='h',
         year, sum(years[mammaprint_in_TiAbKw&oncotype_in_TiAbKw]==year),
         col='blue', lwd=lwd)
  lines(c(year-offset,year+offset), c(0,0))
}
legendColors = c('green', 'red', 'blue', 'grey')
legend('topleft',
       legend=c('ODX', 'MP', 'both', 'total') , 
       lwd=lwd,
       text.col=legendColors,
       col=legendColors
)


#### why 2002 and 2020?
