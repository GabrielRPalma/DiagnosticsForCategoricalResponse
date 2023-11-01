####################################################################################################
###
### File:    03_distance_based_grouped_data_visualization.r
### Purpose: Code required for the visualizations presented in the paper " Diagnostics 
###         for categorical response models based on quantile residuals and
###         distance measures.
###          create interactive maps.
### Authors: Gabriel Rodrigues Palma
### Date:    02/04/23
###
####################################################################################################
# Loading packages -----
source('00_source.r')

# Reading the data -----
##############################################################################################################
###################### Distance based on model 2 ##################################################################
##############################################################################################################
simulation_results_n50_j3_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_3_50.out',header = T)[,-1]
simulation_results_n50_j3_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_3_100.out',header = T)[,-1]
simulation_results_n100_j3_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_3_200.out',header = T)[,-1]
simulation_results_n200_j3_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_4_50.out',header = T)[,-1]
simulation_results_n50_j4_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_4_100.out',header = T)[,-1]
simulation_results_n100_j4_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_4_200.out',header = T)[,-1]
simulation_results_n200_j4_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_5_50.out',header = T)[,-1]
simulation_results_n50_j5_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_5_100.out',header = T)[,-1]
simulation_results_n100_j5_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_5_200.out',header = T)[,-1]
simulation_results_n200_j5_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

#################################### M = 10 ########################################################################
simulation_results_n50_j3_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_3_50.out',header = T)[,-1]
simulation_results_n50_j3_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_3_100.out',header = T)[,-1]
simulation_results_n100_j3_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_3_200.out',header = T)[,-1]
simulation_results_n200_j3_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_4_50.out',header = T)[,-1]
simulation_results_n50_j4_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_4_100.out',header = T)[,-1]
simulation_results_n100_j4_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_4_200.out',header = T)[,-1]
simulation_results_n200_j4_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_5_50.out',header = T)[,-1]
simulation_results_n50_j5_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_5_100.out',header = T)[,-1]
simulation_results_n100_j5_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_5_200.out',header = T)[,-1]
simulation_results_n200_j5_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

#################################### M = 15 ########################################################################
simulation_results_n50_j3_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_3_50.out',header = T)[,-1]
simulation_results_n50_j3_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_3_100.out',header = T)[,-1]
simulation_results_n100_j3_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_3_200.out',header = T)[,-1]
simulation_results_n200_j3_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_4_50.out',header = T)[,-1]
simulation_results_n50_j4_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_4_100.out',header = T)[,-1]
simulation_results_n100_j4_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_4_200.out',header = T)[,-1]
simulation_results_n200_j4_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_5_50.out',header = T)[,-1]
simulation_results_n50_j5_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_5_100.out',header = T)[,-1]
simulation_results_n100_j5_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_15_5_200.out',header = T)[,-1]
simulation_results_n200_j5_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))



grouped_simulation_data_categorical <- rbind(simulation_results_n50_j3_m5, 
                                                simulation_results_n100_j3_m5, 
                                                simulation_results_n200_j3_m5,
                                                simulation_results_n50_j4_m5, 
                                                simulation_results_n100_j4_m5, 
                                                simulation_results_n200_j4_m5,
                                                simulation_results_n50_j5_m5, 
                                                simulation_results_n100_j5_m5, 
                                                simulation_results_n200_j5_m5, 
                                                
                                                simulation_results_n50_j3_m10, 
                                                simulation_results_n100_j3_m10, 
                                                simulation_results_n200_j3_m10,
                                                simulation_results_n50_j4_m10, 
                                                simulation_results_n100_j4_m10, 
                                                simulation_results_n200_j4_m10,
                                                simulation_results_n50_j5_m10, 
                                                simulation_results_n100_j5_m10, 
                                                simulation_results_n200_j5_m10, 
                                                
                                                simulation_results_n50_j3_m15, 
                                                simulation_results_n100_j3_m15, 
                                                simulation_results_n200_j3_m15,
                                                simulation_results_n50_j4_m15, 
                                                simulation_results_n100_j4_m15, 
                                                simulation_results_n200_j4_m15,
                                                simulation_results_n50_j5_m15, 
                                                simulation_results_n100_j5_m15, 
                                                simulation_results_n200_j5_m15)
colnames(grouped_simulation_data_categorical)

grouped_simulation_data_categorical_p_values_correct <- grouped_simulation_data_categorical %>%
  dplyr::select(Correct_model_perc_eucledian, Correct_model_perc_mahalanovis, 
                N, J, M)
colnames(grouped_simulation_data_categorical_p_values_correct) <- c('Eucledian',
                                                                       'Mahalanovis', 
                                                                       'N', 'J', 'M')
grouped_simulation_data_categorical_p_values_correct <- 
  grouped_simulation_data_categorical_p_values_correct %>%
  pivot_longer(cols = 1:2)
grouped_simulation_data_categorical_p_values_correct$Model <- rep('Correct', 
                                                                  nrow(grouped_simulation_data_categorical_p_values_correct))

colnames(grouped_simulation_data_categorical_p_values_correct) <- c('N', 'J', 'M',
                                                                    'Distance_type', 
                                                                    'Value', 
                                                                    'Model')


grouped_simulation_data_categorical_p_values_null <- grouped_simulation_data_categorical %>%
  dplyr::select(Null_model_perc_eucledian, Null_model_perc_mahalanovis, 
                N, J, M)
colnames(grouped_simulation_data_categorical_p_values_null) <- c('Eucledian',
                                                                       'Mahalanovis', 
                                                                       'N', 'J', 'M')
grouped_simulation_data_categorical_p_values_null <- 
  grouped_simulation_data_categorical_p_values_null %>%
  pivot_longer(cols = 1:2)
grouped_simulation_data_categorical_p_values_null$Model <- rep('Null', 
                                                                  nrow(grouped_simulation_data_categorical_p_values_null))
colnames(grouped_simulation_data_categorical_p_values_null) <- c('N', 'J', 'M',
                                                                 'Distance_type', 
                                                                 'Value', 
                                                                 'Model')

grouped_simulation_data_categorical_percentages <- rbind(grouped_simulation_data_categorical_p_values_correct, 
                                                            grouped_simulation_data_categorical_p_values_null)
# Visualization of the simulation study -----
grouped_simulation_data_categorical_percentages %>%
  mutate(p_less = Value > 50) %>%
  group_by(N, J, M, Distance_type, Model) %>%
  summarise(p_medio = mean(Value),
            porcentagem = mean(Value)) %>%
  View()

grouped_simulation_data_categorical_percentages$N <-
  factor(grouped_simulation_data_categorical_percentages$N,
         levels = c("50","100","200"))
grouped_simulation_data_categorical_percentages$M <-
  factor(grouped_simulation_data_categorical_percentages$M,
         levels = c("5","10","15"))
grouped_simulation_data_categorical_percentages$J <-
  factor(grouped_simulation_data_categorical_percentages$J,
         levels = c("3","4","5"))

############################### M == 5 ####################################################
grouped_simulation_data_categorical_percentages %>%
  filter(Distance_type == 'Mahalanovis', 
         M == '5') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type')
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model2_mahalanobis_distance_m5.png', height = 4, width = 6)

grouped_simulation_data_categorical_percentages %>%
  filter(Distance_type == 'Eucledian', 
         M == '5') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type')
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model2_eucledian_distance_m5.png', height = 4, width = 6)
############################### M == 10 ####################################################
grouped_simulation_data_categorical_percentages %>%
  filter(Distance_type == 'Mahalanovis', 
         M == '10') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type')
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model2_mahalanobis_distance_m10.png', height = 4, width = 6)

grouped_simulation_data_categorical_percentages %>%
  filter(Distance_type == 'Eucledian', 
         M == '10') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model2_eucledian_distance_m10.png', height = 4, width = 6)

############################### M == 15 ####################################################
grouped_simulation_data_categorical_percentages %>%
  filter(Distance_type == 'Mahalanovis', 
         M == '15') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type')
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model2_mahalanobis_distance_m15.png', height = 4, width = 6)

grouped_simulation_data_categorical_percentages %>%
  filter(Distance_type == 'Eucledian', 
         M == '15') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model2_eucledian_distance_m15.png', height = 4, width = 6)



##############################################################################################################
###################### Distance based data Continuous ##################################################################
##############################################################################################################
simulation_results_n50_j3_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_3_50.out',header = T)[,-1]
simulation_results_n50_j3_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_3_100.out',header = T)[,-1]
simulation_results_n100_j3_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_3_200.out',header = T)[,-1]
simulation_results_n200_j3_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_4_50.out',header = T)[,-1]
simulation_results_n50_j4_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_4_100.out',header = T)[,-1]
simulation_results_n100_j4_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_4_200.out',header = T)[,-1]
simulation_results_n200_j4_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_5_50.out',header = T)[,-1]
simulation_results_n50_j5_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_5_100.out',header = T)[,-1]
simulation_results_n100_j5_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_5_5_200.out',header = T)[,-1]
simulation_results_n200_j5_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

#################################### M = 10 ########################################################################
simulation_results_n50_j3_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_3_50.out',header = T)[,-1]
simulation_results_n50_j3_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_3_100.out',header = T)[,-1]
simulation_results_n100_j3_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_3_200.out',header = T)[,-1]
simulation_results_n200_j3_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_4_50.out',header = T)[,-1]
simulation_results_n50_j4_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_4_100.out',header = T)[,-1]
simulation_results_n100_j4_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_4_200.out',header = T)[,-1]
simulation_results_n200_j4_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_5_50.out',header = T)[,-1]
simulation_results_n50_j5_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_5_100.out',header = T)[,-1]
simulation_results_n100_j5_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10 <- read.csv('output_data/grouped_data_simulation/distance_metrics/categorical_data/out_10_5_200.out',header = T)[,-1]
simulation_results_n200_j5_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

#################################### M = 15 ########################################################################
simulation_results_n50_j3_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_3_50.out',header = T)[,-1]
simulation_results_n50_j3_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_3_100.out',header = T)[,-1]
simulation_results_n100_j3_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_3_200.out',header = T)[,-1]
simulation_results_n200_j3_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_4_50.out',header = T)[,-1]
simulation_results_n50_j4_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_4_100.out',header = T)[,-1]
simulation_results_n100_j4_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_4_200.out',header = T)[,-1]
simulation_results_n200_j4_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_5_50.out',header = T)[,-1]
simulation_results_n50_j5_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_5_100.out',header = T)[,-1]
simulation_results_n100_j5_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m15 <- read.csv('output_data/grouped_data_simulation/distance_metrics/continuous_data/out_15_5_200.out',header = T)[,-1]
simulation_results_n200_j5_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))



grouped_simulation_data_continuous <- rbind(simulation_results_n50_j3_m5, 
                                             simulation_results_n100_j3_m5, 
                                             simulation_results_n200_j3_m5,
                                             simulation_results_n50_j4_m5, 
                                             simulation_results_n100_j4_m5, 
                                             simulation_results_n200_j4_m5,
                                             simulation_results_n50_j5_m5, 
                                             simulation_results_n100_j5_m5, 
                                             simulation_results_n200_j5_m5, 
                                             
                                             simulation_results_n50_j3_m10, 
                                             simulation_results_n100_j3_m10, 
                                             simulation_results_n200_j3_m10,
                                             simulation_results_n50_j4_m10, 
                                             simulation_results_n100_j4_m10, 
                                             simulation_results_n200_j4_m10,
                                             simulation_results_n50_j5_m10, 
                                             simulation_results_n100_j5_m10, 
                                             simulation_results_n200_j5_m10, 
                                             
                                             simulation_results_n50_j3_m15, 
                                             simulation_results_n100_j3_m15, 
                                             simulation_results_n200_j3_m15,
                                             simulation_results_n50_j4_m15, 
                                             simulation_results_n100_j4_m15, 
                                             simulation_results_n200_j4_m15,
                                             simulation_results_n50_j5_m15, 
                                             simulation_results_n100_j5_m15, 
                                             simulation_results_n200_j5_m15)
colnames(grouped_simulation_data_continuous)

grouped_simulation_data_continuous_p_values_correct <- grouped_simulation_data_continuous %>%
  dplyr::select(Correct_model_perc_eucledian, Correct_model_perc_mahalanovis, 
                N, J, M)
colnames(grouped_simulation_data_continuous_p_values_correct) <- c('Eucledian',
                                                                    'Mahalanovis', 
                                                                    'N', 'J', 'M')
grouped_simulation_data_continuous_p_values_correct <- 
  grouped_simulation_data_continuous_p_values_correct %>%
  pivot_longer(cols = 1:2)
grouped_simulation_data_continuous_p_values_correct$Model <- rep('Correct', 
                                                                  nrow(grouped_simulation_data_continuous_p_values_correct))

colnames(grouped_simulation_data_continuous_p_values_correct) <- c('N', 'J', 'M',
                                                                    'Distance_type', 
                                                                    'Value', 
                                                                    'Model')


grouped_simulation_data_continuous_p_values_null <- grouped_simulation_data_continuous %>%
  dplyr::select(Null_model_perc_eucledian, Null_model_perc_mahalanovis, 
                N, J, M)
colnames(grouped_simulation_data_continuous_p_values_null) <- c('Eucledian',
                                                                 'Mahalanovis', 
                                                                 'N', 'J', 'M')
grouped_simulation_data_continuous_p_values_null <- 
  grouped_simulation_data_continuous_p_values_null %>%
  pivot_longer(cols = 1:2)
grouped_simulation_data_continuous_p_values_null$Model <- rep('Null', 
                                                               nrow(grouped_simulation_data_continuous_p_values_null))
colnames(grouped_simulation_data_continuous_p_values_null) <- c('N', 'J', 'M',
                                                                 'Distance_type', 
                                                                 'Value', 
                                                                 'Model')

grouped_simulation_data_continuous_percentages <- rbind(grouped_simulation_data_continuous_p_values_correct, 
                                                         grouped_simulation_data_continuous_p_values_null)
# Visualization of the simulation study -----
grouped_simulation_data_continuous_percentages %>%
  mutate(p_less = Value > 50) %>%
  group_by(N, J, M, Distance_type, Model) %>%
  summarise(p_medio = mean(Value),
            porcentagem = mean(Value)) %>%
  View()

grouped_simulation_data_continuous_percentages$N <-
  factor(grouped_simulation_data_continuous_percentages$N,
         levels = c("50","100","200"))
grouped_simulation_data_continuous_percentages$M <-
  factor(grouped_simulation_data_continuous_percentages$M,
         levels = c("5","10","15"))
grouped_simulation_data_continuous_percentages$J <-
  factor(grouped_simulation_data_continuous_percentages$J,
         levels = c("3","4","5"))

############################### M ==5 ####################################################
grouped_simulation_data_continuous_percentages %>%
  filter(Distance_type == 'Mahalanovis', 
         M == '5') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model1_mahalanobis_distance_m5.png', height = 4, width = 6)

grouped_simulation_data_continuous_percentages %>%
  filter(Distance_type == 'Eucledian', 
         M == '5') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model1_eucledian_distance_m5.png', height = 4, width = 6)
############################### M == 10 ####################################################
grouped_simulation_data_continuous_percentages %>%
  filter(Distance_type == 'Mahalanovis', 
         M == '10') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model1_mahalanobis_distance_m10.png', height = 4, width = 6)

grouped_simulation_data_continuous_percentages %>%
  filter(Distance_type == 'Eucledian', 
         M == '10') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model1_eucledian_distance_m10.png', height = 4, width = 6)
############################### M == 15 ####################################################
grouped_simulation_data_continuous_percentages %>%
  filter(Distance_type == 'Mahalanovis', 
         M == '15') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model1_mahalanobis_distance_m15.png', height = 4, width = 6)

grouped_simulation_data_continuous_percentages %>%
  filter(Distance_type == 'Eucledian', 
         M == '15') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Distance_based/grouped_simulation_data_model1_eucledian_distance_m15.png', height = 4, width = 6)
