
# Load functions for analysis
source("R/functions_data_prep.R") # Data preparation
source("R/functions_models.R") # Modeling and posteriors
source("R/functions_tables.R") # Create tables
source("R/functions_figures.R") # Create figures
source("R/functions_other_output.R") # Create text results and supplementary

# Load the Drake plan outlining the workflow
source("R/plan.R")

options(clustermq.scheduler = "multicore") # Use parallel computing

# Configure the drake plan, to run the full analysis
drake::drake_config(
  plan,
  verbose = 2,
  parallelism = "clustermq",
  jobs = parallel::detectCores()
)

