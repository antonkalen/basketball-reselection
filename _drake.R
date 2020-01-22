source("R/packages.R")  # loads packages
source("R/functions.R") # defines the create_plot() function
source("R/plan.R")      # creates the drake plan

options(clustermq.scheduler = "multicore") # optional parallel computing. Also needs parallelism = "clustermq"

drake::drake_config(
  plan,
  verbose = 2,
  parallelism = "clustermq"
)

