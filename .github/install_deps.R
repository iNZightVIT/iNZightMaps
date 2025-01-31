# R script
github_deps <- c(
    "cran/maptools",
    "cran/rgeos",
    "cran/rgdal",
    "tmelliott/surveyspec"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM <- Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    )
)

if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
}

pak::pkg_install(github_deps, dependencies = TRUE)
pak::local_install_dev_deps(upgrade = FALSE, dependencies = TRUE)
pak::pkg_install("rcmdcheck")

# if (OS != "Linux" && !requireNamespace("XML", quietly = TRUE)) {
#     install.packages("XML", type = "binary")
# }

# if (!requireNamespace("remotes", quietly = TRUE)) {
#     install.packages("remotes")
# }

# remotes::install_github(github_deps,
#     INSTALL_opts = c("--no-multiarch")
# )
# remotes::install_deps(
#     dependencies = TRUE,
#     INSTALL_opts = c("--no-multiarch")
# )
# remotes::install_cran("rcmdcheck",
#     INSTALL_opts = c("--no-multiarch")
# )
