# Retrieve input arguments from command line
args <- commandArgs(trailingOnly = TRUE)

# Convert input arguments to numeric
m <- as.numeric(args[1])
j <- as.numeric(args[2])
n <- as.numeric(args[3])
path <- args[4]

# Calculate sum of input arguments
## Load functions
source('./00_source.r')
parameters <- create_prob_vector_cont_data(n, j)

simulation_results <- obtain_group_continuous_data_simulation_distance_metrics(x = parameters$x, 
                                                                               prob = parameters$prob, 
                                                                               m = m)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_continuous_data_simulation_distance_metrics(x = parameters$x,
                                                                                           prob = parameters$prob,
                                                                                           m = m)
  simulation_results <- rbind(simulation_results, simulation_results_replicate)
  replicates <- replicates + 1
}

sink(path)
write.csv(simulation_results)
sink()
print(simulation_results)
