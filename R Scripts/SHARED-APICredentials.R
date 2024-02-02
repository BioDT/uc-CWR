API_User <- 284500
API_Key <- "4b988f80-d86e-4d02-9eb0-0d5d0a72c6e5"

# Choose the number of parallel processes
numberOfCores <- strtoi(Sys.getenv("SLURM_NTASKS"))
if (is.na(numberOfCores)) {
    numberOfCores <- parallel::detectCores()
}

options(gbif_user = "biodt-cwr")
options(gbif_email = "biodt-robot@gbif.no")
options(gbif_pwd = "CWRpDT2024")