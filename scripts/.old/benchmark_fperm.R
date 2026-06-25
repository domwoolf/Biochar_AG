# benchmark_fperm.R

# Ensure BiocharAG is loaded
devtools::load_all()

# Check if microbenchmark is available
if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    install.packages("microbenchmark", repos = "https://cloud.r-project.org")
}
library(microbenchmark)

# Test Inputs
val <- 0.35
soil_temp <- 15
method <- "HC"

# Run Benchmark
cat("Benchmarking Fperm Calculations (1000 evaluations)...\n")
mb <- microbenchmark(
    Exact = calculate_fperm(val, method = method, soil_temp = soil_temp),
    Approx = calculate_fperm_approx(val, method = method, soil_temp = soil_temp),
    times = 1000
)

print(mb)

# Vectorized Benchmark (simulate global run)
cat("\nBenchmarking Vectorized Operation (1000 inputs)...\n")
vals <- runif(1000, 0, 0.7)
temps <- runif(1000, -10, 30)

# Note: The functions scalar by default in their current implementation structure (calculate_fperm uses single logic flow),
# but approx is naturally vectorizable if written carefully. calculate_fperm uses 'lm' which is scalar-oriented per call.
# We will wrapper apply for realistic usage comparison.

wrapper_exact <- function(v, t) {
    mapply(function(vi, ti) calculate_fperm(vi, "HC", ti), v, t)
}

wrapper_approx <- function(v, t) {
    # approx function uses findInterval which is vectorized, so we can check if it handles vectors natively
    # calculate_fperm_approx implementation:
    # val <- pmax(...) -> Vectorized
    # idx_x <- findInterval(...) -> Vectorized
    # result assignment -> Vectorized
    # So approx should be natively vectorized!
    calculate_fperm_approx(v, "HC", t)
}

# Verify approx is vectorized
tryCatch(
    {
        res <- calculate_fperm_approx(vals[1:5], "HC", temps[1:5])
        if (length(res) == 5) cat("Approx function appears natively vectorized.\n")
    },
    error = function(e) cat("Approx function NOT natively vectorized.\n")
)


mb_vec <- microbenchmark(
    Exact_Loop = wrapper_exact(vals[1:100], temps[1:100]), # Smaller set for exact because it's slow
    Approx_Vec = wrapper_approx(vals[1:100], temps[1:100]),
    times = 10
)

print(mb_vec)
