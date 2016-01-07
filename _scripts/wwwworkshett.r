library(grid)
library(maptools)
library(RColorBrewer)
setwd('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/world')
data = read.csv('C:/Users/yeamin/Downloads/iNZightMaps-dev-integrateplots/data/Gapminder-2008.csv',skip = 1)
source('C:/Users/yeamin/Documents/GitHub/iNZightMaps/R/shade.fun.r')
system.time(
	shade.map('TM_WORLD_BORDERS-0.3.shp',colby = 'IncomePerPerson',data = data,method = 'normal', display = 'heat',offset = 0.1)
)



shp.name = 'TM_WORLD_BORDERS-0.3.shp'
source('C:/Users/yeamin/Documents/GitHub/iNZightMaps/R/shade.fun.r')
system.time(shade.map(shp.name,colby = 'IncomePerPerson',data = data, method = 'normal',na.fill = 'White',offset = 0.2))

source('C:/Users/yeamin/Documents/GitHub/iNZightMaps/R/shade.fun.r')
shp.name = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/au/map.shp'
system.time(shade.map(shp.name,colby = 'IncomePerPerson', method = 'n',na.fill = 'White',offset =0.5))


shp.name = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/New Zealand/REGC2013_GV_Clipped.shp'
system.time(shade.map(shp.name,colby = 'IncomePerPerson', method = 'r',na.fill = 'White',offset =0.5))

offset = 0
impossible.number = '0.234959823475923487509234875'
orderd.col.trans = ifelse(is.na(orderd.data) == TRUE, 0.00001,orderd.data * (1 - offset) + (offset))

ss = readShapePoly(shp.name)
d.name = data$Country
s.name = as.character(ss@data$NAME)

ii = match(d.name,s.name)
missing.name = as.character(d.name[is.na(ii)])
for(i in 1:length(missing.name)
{

}
x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
# split x on the letter e
s.split = strsplit(s.name, " |-")
m.split = strsplit(missing.name," ")
for(i in 1:length(m.split))
{
	for(j in 1:m.split[[i]])
	{
		
	}
}

match(m.split[[3]], s.split)

lapply(s.split,function(x) which(x == 'United'))



o = function(x,y) match(x,y)
mapply(o,a,b)

p = 0
e = rep(FALSE,length(a))
for(i in 1:length(a)){
	for(j in 1:length(b)){
		e[i] = any(c(any(a[[i]] %in% b[[j]]),e[i]))
	}
}
a = list(c('aa','bb'),'fg','gg','cc')
b = list('aa',c('aa','ee'),'cc','dd')

####useing
e = rep(FALSE,length(m.split))
for(i in 1:length(m.split)){
	for(j in 1:length(s.split)){
		e[i] = any(c(any(m.split[[i]] %in% s.split[[j]]),e[i]))
	}
}

aa = c(-1,-2,-3,0,1,2,3)

ifelse(aa > 0,