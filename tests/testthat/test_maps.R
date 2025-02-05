test_that("iNZightMap", {
    m <- iNZightMap(~lat, ~long, data = quakes)
    expect_is(m, "inzightmap")
})
