####################################################################################################
###
### File:    03_individual_data_visualization.r
### Purpose: Code required to the visualisation of the simulation results
### Authors: Gabriel Rodrigues Palma
### Date:    05/05/23
###
####################################################################################################
# Load packages -----
source('00_source.r')

# Reading the data -----
## Loading Categorical data
simulation_results_n50_j3 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n50_j3.csv',header = T)[,-1]
simulation_results_n50_j3$N <- rep('50', nrow(simulation_results_n50_j3))
simulation_results_n50_j3$J <- rep('3', nrow(simulation_results_n50_j3))

simulation_results_n100_j3 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n100_j3.csv',header = T)[,-1]
simulation_results_n100_j3$N <- rep('100', nrow(simulation_results_n50_j3))
simulation_results_n100_j3$J <- rep('3', nrow(simulation_results_n50_j3))

simulation_results_n200_j3 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n200_j3.csv',header = T)[,-1]
simulation_results_n200_j3$N <- rep('200', nrow(simulation_results_n50_j3))
simulation_results_n200_j3$J <- rep('3', nrow(simulation_results_n50_j3))

simulation_results_n50_j4 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n50_j4.csv',header = T)[1:1000,-1]
simulation_results_n50_j4$N <- rep('50', nrow(simulation_results_n50_j3))
simulation_results_n50_j4$J <- rep('4', nrow(simulation_results_n50_j3))

simulation_results_n100_j4 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n100_j4.csv',header = T)[,-1]
simulation_results_n100_j4$N <- rep('100', nrow(simulation_results_n50_j3))
simulation_results_n100_j4$J <- rep('4', nrow(simulation_results_n50_j3))

simulation_results_n200_j4 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n200_j4.csv',header = T)[,-1]
simulation_results_n200_j4$N <- rep('200', nrow(simulation_results_n50_j3))
simulation_results_n200_j4$J <- rep('4', nrow(simulation_results_n50_j3))

simulation_results_n50_j5 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n50_j5.csv',header = T)[,-1]
simulation_results_n50_j5$N <- rep('50', nrow(simulation_results_n50_j3))
simulation_results_n50_j5$J <- rep('5', nrow(simulation_results_n50_j3))

simulation_results_n100_j5 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n100_j5.csv',header = T)[,-1]
simulation_results_n100_j5$N <- rep('100', nrow(simulation_results_n50_j3))
simulation_results_n100_j5$J <- rep('5', nrow(simulation_results_n50_j3))

simulation_results_n200_j5 <- read.csv('output_data/Individual_data_simulation/Categorical_data/simulation_results_n200_j5.csv',header = T)[,-1]
simulation_results_n200_j5$N <- rep('200', nrow(simulation_results_n50_j3))
simulation_results_n200_j5$J <- rep('5', nrow(simulation_results_n50_j3))

individual_simulation_data_categorical <- rbind(simulation_results_n50_j3, 
                                                simulation_results_n100_j3, 
                                                simulation_results_n200_j3,
                                                simulation_results_n50_j4, 
                                                simulation_results_n100_j4, 
                                                simulation_results_n200_j4,
                                                simulation_results_n50_j5, 
                                                simulation_results_n100_j5, 
                                                simulation_results_n200_j5)
individual_simulation_data_categorical_p_values <- individual_simulation_data_categorical %>%
  dplyr::select(Null_model_pvalues, Correct_model_pvalues, N, J) %>%
  pivot_longer(cols = 1:2)
colnames(individual_simulation_data_categorical_p_values) <- c('N', 'J', 
                                                               'Model', 'P_values')
## Loading continuous data 
simulation_results_n50_j3 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n50_j3.csv',header = T)[,-1]
simulation_results_n50_j3$N <- rep('50', nrow(simulation_results_n50_j3))
simulation_results_n50_j3$J <- rep('3', nrow(simulation_results_n50_j3))

simulation_results_n100_j3 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n100_j3.csv',header = T)[,-1]
simulation_results_n100_j3$N <- rep('100', nrow(simulation_results_n50_j3))
simulation_results_n100_j3$J <- rep('3', nrow(simulation_results_n50_j3))

simulation_results_n200_j3 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n200_j3.csv',header = T)[,-1]
simulation_results_n200_j3$N <- rep('200', nrow(simulation_results_n50_j3))
simulation_results_n200_j3$J <- rep('3', nrow(simulation_results_n50_j3))

simulation_results_n50_j4 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n50_j4.csv',header = T)[1:1000,-1]
simulation_results_n50_j4$N <- rep('50', nrow(simulation_results_n50_j3))
simulation_results_n50_j4$J <- rep('4', nrow(simulation_results_n50_j3))

simulation_results_n100_j4 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n100_j4.csv',header = T)[,-1]
simulation_results_n100_j4$N <- rep('100', nrow(simulation_results_n50_j3))
simulation_results_n100_j4$J <- rep('4', nrow(simulation_results_n50_j3))

simulation_results_n200_j4 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n200_j4.csv',header = T)[,-1]
simulation_results_n200_j4$N <- rep('200', nrow(simulation_results_n50_j3))
simulation_results_n200_j4$J <- rep('4', nrow(simulation_results_n50_j3))

simulation_results_n50_j5 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n50_j5.csv',header = T)[,-1]
simulation_results_n50_j5$N <- rep('50', nrow(simulation_results_n50_j3))
simulation_results_n50_j5$J <- rep('5', nrow(simulation_results_n50_j3))

simulation_results_n100_j5 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n100_j5.csv',header = T)[,-1]
simulation_results_n100_j5$N <- rep('100', nrow(simulation_results_n50_j3))
simulation_results_n100_j5$J <- rep('5', nrow(simulation_results_n50_j3))

simulation_results_n200_j5 <- read.csv('output_data/Individual_data_simulation/Continuous_data/simulation_results_n200_j5.csv',header = T)[,-1]
simulation_results_n200_j5$N <- rep('200', nrow(simulation_results_n50_j3))
simulation_results_n200_j5$J <- rep('5', nrow(simulation_results_n50_j3))

individual_simulation_data_continuous <- rbind(simulation_results_n50_j3, 
                                                simulation_results_n100_j3, 
                                                simulation_results_n200_j3,
                                                simulation_results_n50_j4, 
                                                simulation_results_n100_j4, 
                                                simulation_results_n200_j4,
                                                simulation_results_n50_j5, 
                                                simulation_results_n100_j5, 
                                                simulation_results_n200_j5)
individual_simulation_data_continuous_p_values <- individual_simulation_data_continuous %>%
  dplyr::select(Null_model_pvalues, Correct_model_pvalues, N, J) %>%
  pivot_longer(cols = 1:2)
colnames(individual_simulation_data_continuous_p_values) <- c('N', 'J', 
                                                               'Model', 'P_values')

## Working with continuous data
individual_simulation_data_continuous_p_values %>%
  mutate(p_less = P_values < 0.05) %>%
  group_by(N, J, Model) %>%
  summarise(p_medio = mean(P_values),
            porcentagem = mean(p_less)) %>%
  View()


individual_simulation_data_continuous_p_values$N <-
  factor(individual_simulation_data_continuous_p_values$N,
         levels = c("50","100","200"))

individual_simulation_data_continuous_p_values %>%
  ggplot(mapping = aes(x = P_values, fill = Model)) +
  geom_histogram(bins = 50, position = 'dodge') +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Frequency') +
  xlab('P values') +
  scale_color_manual(values = c("#A3C4D9", "#043259"), 
                     labels = c("Correct", "Null")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259"), 
                    labels = c("Correct", "Null"))+
  scale_y_continuous(#sec.axis = sec_axis(~./1, name = "Correct"), 
                     trans = "sqrt")
ggsave('Plots/Individual_data_results/individual_simulation_data_continuous_p_values.png', height = 6, width = 8)

## Working with categorical data
individual_simulation_data_categorical_p_values %>%
  mutate(p_less = P_values < 0.05) %>%
  group_by(N, J, Model) %>%
  summarise(p_medio = mean(P_values),
            porcentagem = mean(p_less)) %>%
  View()


individual_simulation_data_categorical_p_values$N <-
  factor(individual_simulation_data_categorical_p_values$N,
         levels = c("50","100","200"))

individual_simulation_data_categorical_p_values %>%
  ggplot(mapping = aes(x = P_values, fill = Model)) +
  geom_histogram(bins = 50, position = 'dodge') +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Frequency') +
  xlab('P values') +
  scale_color_manual(values = c("#A3C4D9", "#043259"), 
                     labels = c("Correct", "Null")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259"), 
                    labels = c("Correct", "Null"))+
  scale_y_continuous(#sec.axis = sec_axis(~./1, name = "Correct"), 
    trans = "sqrt")
ggsave('Plots/Individual_data_results/individual_simulation_data_categorical_p_values.png', height = 6, width = 8)
