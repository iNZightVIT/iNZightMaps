context("Subsetting maps")

m <- iNZightMap(~Latitude, ~Longitude, nzquakes)
test_that("Subsetting by two variables includes necessary labels", {
    plot(m)
    expect_equal(grid.get("inz-main-title")$label, "Map of nzquakes")

    plot(m, g1 = Felt)
    expect_equal(
        grid.get("inz-main-title")$label,
        "Map of nzquakes subset by Felt"
    )

    plot(m, g1 = Felt, g2 = NorMidSth, g2.level = "_MULTI")
    expect_equal(
        grid.get("inz-main-title")$label,
        "Map of nzquakes subset by Felt and NorMidSth"
    )

    plot(m, g1 = Felt, g1.level="N", g2 = NorMidSth, g2.level = "South")
    expect_equal(
        grid.get("inz-main-title")$label,
        "Map of nzquakes subset by Felt, for NorMidSth = South"
    )
})

test_that("Size included in title", {
    plot(m, sizeby = Magnitude)
    expect_equal(
        grid.get("inz-main-title")$label,
        "Map of nzquakes (size proportional to Magnitude)"
    )
})
