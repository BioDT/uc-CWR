## API Credentials available for Project Partners upon request
message("API Credentials available for Project Partners upon request")

## CDS Credentials
# API_User <- 
# API_Key <- 

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

# GBIF Credentials 
# options(gbif_user = "")
# options(gbif_email = "")
# options(gbif_pwd = "")
