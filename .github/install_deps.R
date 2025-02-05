# R script
pkg_deps <- c(
    "iNZightVIT/iNZightPlots@release/2.16.0",
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

pak::pkg_install(pkg_deps, dependencies = TRUE)
pak::local_install_dev_deps(upgrade = FALSE, dependencies = TRUE)
pak::pkg_install("rcmdcheck")

# if (OS != "Linux" && !requireNamespace("XML", quietly = TRUE)) {
#     install.packages("XML", type = "binary")
# }

# if (!requireNamespace("remotes", quietly = TRUE)) {
#     install.packages("remotes")
# }

# remotes::install_github(pkg_deps,
#     INSTALL_opts = c("--no-multiarch")
# )
# remotes::install_deps(
#     dependencies = TRUE,
#     INSTALL_opts = c("--no-multiarch")
# )
# remotes::install_cran("rcmdcheck",
#     INSTALL_opts = c("--no-multiarch")
# )
