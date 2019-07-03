allyears = as.numeric(sort(unique(years)))

table(years)

table(mammaprint_in_Ti, oncotype_in_Ti)

plot(allyears, table(years), type='h' )

table(match(years, allyears))

for(year in allyears)
  points(type='h',
  0.1+year, sum(years[mammaprint_in_TiAb]==year),
      col='red')
for(year in allyears)
  points(type='h',
    -0.1+year, sum(years[oncotype_in_TiAb]==year),
    col='green')

sort(unique(years[mammaprint_in_TiAb]))
sort(unique(years[oncotype_in_TiAb]))
