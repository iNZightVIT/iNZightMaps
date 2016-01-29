obj.location = 'C:/Users/yeamin/Desktop/world/ne_110m_admin_0_scale_rank.shp'
obj = readShapeSpatial(obj.location)
shp = shape.extract(obj,7)
class(shp) <- c("inzightshapemap", class(shp))

center = getSpPPolygonsLabptSlots(obj)

saveRDS(shp,
    'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/world.rds')


location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/shpdata/New Zealand.rds'
obj <- iNZightShapeMap(shp.RDS = location)

obj <- iNZightShapeMap(location = obj.location,column.index = 4)


shape.extract(obj.location,1)



pattern <- "a.b"
strings <- c("ABB", "aaB", "aab")
str_detect(strings, pattern)
str_detect(strings, ignore.case(pattern))


match("B", tolower(c("A","B","C")))
