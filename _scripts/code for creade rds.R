obj.location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/NZ/map.shp'
obj = readShapeSpatial(obj.location)
shp = shape.extract(obj,2)
class(shp) <- c("inzightshapemap", class(shp))

saveRDS(shp,
    'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/shpdata/New Zealand.rds')


location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/shpdata/New Zealand.rds'
obj <- iNZightShapeMap(shp.RDS = location)

obj <- iNZightShapeMap(location = obj.location,column.index = 4)


shape.extract(obj.location,1)
