obj.location = 'C:/Users/yeamin/Desktop/america/Special_submarine_features.shp'
obj = readShapeSpatial(obj.location)
shp = shape.extract(obj,1)
  shp$center.region
class(shp) <- c("inzightshapemap", class(shp))
saveRDS(shp,
    'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/SouthAmerica.rds')


location = 'C:/Users/yeamin/Documents/GitHub/iNZightMaps/data/shpdata/New Zealand.rds'
obj <- iNZightShapeMap(shp.RDS = location)

obj <- iNZightShapeMap(location = obj.location,column.index = 4)


shape.extract(obj.location,1)


