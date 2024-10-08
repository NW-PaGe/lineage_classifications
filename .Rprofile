source("renv/activate.R")

# options(
#   repos = PPM = "https://packagemanager.posit.co/cran/latest",
#             # CRAN = "https://cran.rstudio.com/"),
#   renv.config.auto.snapshot = TRUE
# )

options(repos = c(PPM = "https://packagemanager.posit.co/cran/latest"))

# Since RSPM does not provide Mac binaries, always install packages from CRAN
# on mac or windows, even if renv.lock specifies they came from RSPM
if (Sys.info()[["sysname"]] %in% c("Darwin", "Windows")) {
  options(renv.config.repos.override = c(
    # CRAN = "https://cran.rstudio.com/",
    PPM = "https://packagemanager.posit.co/cran/latest",
    INLA = "https://inla.r-inla-download.org/R/testing"))
  print("windows")
} else if (Sys.info()[["sysname"]] == "Linux") {
  options(renv.config.repos.override = c(
    PPM = "https://packagemanager.posit.co/cran/latest",
    INLA = "https://inla.r-inla-download.org/R/testing"))
  print("linux")
}
