API_User <- 301719
API_Key <- "5f600bec-435c-43df-b80e-1e8ea5eb1abf"

# Choose the number of parallel processes
RUNNING_ON_LUMI <- !is.na(strtoi(Sys.getenv("CWR_ON_LUMI")))
if (RUNNING_ON_LUMI) {
    numberOfCores <- strtoi(Sys.getenv("SLURM_NTASKS"))
    if (is.na(numberOfCores)) {
        numberOfCores <- 1
    }
} else {
    numberOfCores <- parallel::detectCores()
}

options(gbif_user = "biodt-cwr")
options(gbif_email = "biodt-robot@gbif.no")
options(gbif_pwd = "SavePasswordNotPublic24!")
