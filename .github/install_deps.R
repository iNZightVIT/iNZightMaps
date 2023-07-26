# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@1.13.0",
    "iNZightVIT/iNZightMR@2.2.7",
    "iNZightVIT/iNZightPlots@2.15.1"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM <- Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS != "Linux" && !requireNamespace("XML", quietly = TRUE)) {
    install.packages("XML", type = "binary")
}

if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}

remotes::install_github(github_deps,
    INSTALL_opts = c("--no-multiarch")
)
remotes::install_deps(
    dependencies = TRUE,
    INSTALL_opts = c("--no-multiarch")
)
remotes::install_cran("rcmdcheck",
    INSTALL_opts = c("--no-multiarch")
)
