####################################################################################################
###
### File:    01_randomized_quantile_residuals_categorical_data_simulation.r
### Purpose: Obtain the results of the simulations using randomized quantile residuals 
###         for grouped categorical data scenario
### Authors: Gabriel Rodrigues Palma
### Date:    25/03/23
###
####################################################################################################
# Load functions and packages -----
source('00_source.r')

# Siimulation scenarios -----
set.seed(1401)
################################################################################################################################################
#################################### Simulation with n = 50, k = 5 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 3)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 5)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
  print(replicates)
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j3_m5.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 5 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 3)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 5)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
  print(replicates)
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j3_m5.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 5 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 3)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 5)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
  print(replicates)
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j3_m5.csv')
write.csv(simulation_results_n200)
sink()

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

################################################################################################################################################
#################################### Simulation with n = 50, k = 5 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 4)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 5)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j4_m5.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 5 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 4)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 5)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j4_m5.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 5 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 4)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 5)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
  print(replicates)
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j4_m5.csv')
write.csv(simulation_results_n200)
sink()

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

################################################################################################################################################
#################################### Simulation with n = 50, k = 5 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 5)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 5)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j5_m5.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 5 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 5)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 5)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j5_m5.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 5 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 5)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 5)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 5)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j5_m5.csv')
write.csv(simulation_results_n200)
sink()

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

################################################################################################################################################
#################################### Simulation with n = 50, k = 10 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 3)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 10)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j3_m10.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 10 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 3)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 10)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j3_m10.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 10 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 3)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 10)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j3_m10.csv')
write.csv(simulation_results_n200)
sink()

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

################################################################################################################################################
#################################### Simulation with n = 50, k = 10 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 4)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 10)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j4_m10.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 10 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 4)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 10)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j4_m10.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 10 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 4)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 10)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j4_m10.csv')
write.csv(simulation_results_n200)
sink()

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

################################################################################################################################################
#################################### Simulation with n = 50, k = 10 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 5)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 10)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j5_m10.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 10 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 5)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 10)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j5_m10.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 10 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 5)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 10)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 10)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j5_m10.csv')
write.csv(simulation_results_n200)
sink()


################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

################################################################################################################################################
#################################### Simulation with n = 50, k = 15 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 3)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 15)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j3_m15.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 15 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 3)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 15)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j3_m15.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 15 and J = 3 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 3)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 15)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j3_m15.csv')
write.csv(simulation_results_n200)
sink()

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

################################################################################################################################################
#################################### Simulation with n = 50, k = 15 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 4)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 15)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j4_m15.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 15 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 4)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 15)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j4_m15.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 15 and J = 4 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 4)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 15)

replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j4_m15.csv')
write.csv(simulation_results_n200)
sink()

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

################################################################################################################################################
#################################### Simulation with n = 50, k = 15 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 50, j = 5)
simulation_results_n50 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                              x_factors = parameters$x_factor, 
                                                                              m = 15)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n50 <- rbind(simulation_results_n50, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j5_m15.csv')
write.csv(simulation_results_n50)
sink()

################################################################################################################################################
#################################### Simulation with n = 100, k = 15 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 100, j = 5)
simulation_results_n100 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 15)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n100 <- rbind(simulation_results_n100, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j5_m15.csv')
write.csv(simulation_results_n100)
sink()

################################################################################################################################################
#################################### Simulation with n = 200, k = 15 and J = 5 ############################################################################################################
################################################################################################################################################
# Results of plots
parameters <- create_prob_vector_categ_data(n = 200, j = 5)
simulation_results_n200 <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                               x_factors = parameters$x_factor, 
                                                                               m = 15)
replicates <- 1
while (replicates < 1000){
  simulation_results_replicate <- obtain_group_categorical_data_simulation_statistics(x = parameters$x, prob = parameters$prob,
                                                                                      x_factors = parameters$x_factor, 
                                                                                      m = 15)
  simulation_results_n200 <- rbind(simulation_results_n200, simulation_results_replicate)
  replicates <- replicates + 1
}
sink('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j5_m15.csv')
write.csv(simulation_results_n200)
sink()
