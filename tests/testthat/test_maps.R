# test these functions
test_that("ClickOnZoom", {
    skip("Cannot test this function")
})

test_that("decodeMapDir", {

})

test_that("download.shapefiles", {

})

test_that("findBestMatch", {

})

test_that("getMinMax", {

})

test_that("iNZightMap", {
    m <- iNZightMap(~lat, ~long, data = quakes)
    expect_is(m, "inzightmap")

    expect_message(plot(m), "Map tiles by Stamen Design")
})

test_that("iNZightMapAggregation", {

})

test_that("iNZightMapPlot", {

})

test_that("iNZightMapProjections", {

})

test_that("iNZightMapRegions", {

})

test_that("iNZightMapVars", {

})

test_that("iNZightShapeMap", {
    
})

test_that("matchVariables", {

})

test_that("read.mapmetadata", {

})

test_that("retrieveMap", {

})

test_that("sClickOnZoom", {

})
