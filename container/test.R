library(pbapply)

# Dummy function that takes some time to evaluate
f <- function(i) {
    n = 2048
    a <- matrix(1:n**2, nrow=n)
    b <- a
    for (j in 1:32) {
        b <- b %*% a
    }
    return(NULL)
}

# Single sequential run
t0 <- Sys.time()
f(1)
t1 <- Sys.time()
print(t1 - t0)

# Choose the number of parallel processes
n <- strtoi(Sys.getenv("SLURM_NTASKS"))
if (is.na(n)) {
    n <- 1
}
print(n)

# Parallel runs
t0 <- Sys.time()
cl <- n  # Linux only
# cl <- parallel::makeCluster(n)  # Doesn't work with large n
pblapply(1:64, f, cl = cl)
# parallel::stopCluster(cl)
t1 <- Sys.time()
print(t1 - t0)
