## API Credentials available for Project Partners upon request
message("API Credentials available for Project Partners upon request")

# CDS
API_User <- "yourUserNameHere@gbif.no" # pw: yourSuperSecurePasswordHere
API_Key <- "yourSecretKeyHere"

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
# numberOfCores <- 1

# GBIF
options(gbif_user = "yourUserName")
options(gbif_email = "yourUserNameHere@gbif.no")
options(gbif_pwd = "yourGBIFpasswordHere")
