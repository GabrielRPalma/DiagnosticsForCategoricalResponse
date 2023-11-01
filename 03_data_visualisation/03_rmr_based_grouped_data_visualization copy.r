####################################################################################################
###
### File:    03_rqr_based_grouped_data_visualization.r
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
###################### Distance based data categorical ##################################################################
##############################################################################################################
simulation_results_n50_j3_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j3_m5.csv',header = T)[,-1]
simulation_results_n50_j3_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j3_m5.csv',header = T)[,-1]
simulation_results_n100_j3_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j3_m5.csv',header = T)[,-1]
simulation_results_n200_j3_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j4_m5.csv',header = T)[,-1]
simulation_results_n50_j4_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j4_m5.csv',header = T)[,-1]
simulation_results_n100_j4_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j4_m5.csv',header = T)[,-1]
simulation_results_n200_j4_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j5_m5.csv',header = T)[,-1]
simulation_results_n50_j5_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j5_m5.csv',header = T)[,-1]
simulation_results_n100_j5_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j5_m5.csv',header = T)[,-1]
simulation_results_n200_j5_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

#################################### M = 10 ########################################################################
simulation_results_n50_j3_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j3_m10.csv',header = T)[,-1]
simulation_results_n50_j3_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j3_m10.csv',header = T)[,-1]
simulation_results_n100_j3_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j3_m10.csv',header = T)[,-1]
simulation_results_n200_j3_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j4_m10.csv',header = T)[,-1]
simulation_results_n50_j4_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j4_m10.csv',header = T)[,-1]
simulation_results_n100_j4_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j4_m10.csv',header = T)[,-1]
simulation_results_n200_j4_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j5_m10.csv',header = T)[,-1]
simulation_results_n50_j5_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j5_m10.csv',header = T)[,-1]
simulation_results_n100_j5_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j5_m10.csv',header = T)[,-1]
simulation_results_n200_j5_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

#################################### M = 15 ########################################################################
simulation_results_n50_j3_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j3_m15.csv',header = T)[,-1]
simulation_results_n50_j3_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j3_m15.csv',header = T)[,-1]
simulation_results_n100_j3_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j3_m15.csv',header = T)[,-1]
simulation_results_n200_j3_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j4_m15.csv',header = T)[,-1]
simulation_results_n50_j4_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j4_m15.csv',header = T)[,-1]
simulation_results_n100_j4_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j4_m15.csv',header = T)[,-1]
simulation_results_n200_j4_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n50_j5_m15.csv',header = T)[,-1]
simulation_results_n50_j5_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n100_j5_m15.csv',header = T)[,-1]
simulation_results_n100_j5_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/categorical_data/simulation_results_n200_j5_m15.csv',header = T)[,-1]
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
  dplyr::select(Correct_model_perc, Correct_model_pvalues, 
                N, J, M)
colnames(grouped_simulation_data_categorical_p_values_correct) <- c('Percentage',
                                                                       'P_values', 
                                                                       'N', 'J', 'M')
grouped_simulation_data_categorical_p_values_correct <- 
  grouped_simulation_data_categorical_p_values_correct %>%
  pivot_longer(cols = 1:2)
grouped_simulation_data_categorical_p_values_correct$Model <- rep('Correct', 
                                                                  nrow(grouped_simulation_data_categorical_p_values_correct))

colnames(grouped_simulation_data_categorical_p_values_correct) <- c('N', 'J', 'M',
                                                                    'Metric', 
                                                                    'Value', 
                                                                    'Model')


grouped_simulation_data_categorical_p_values_null <- grouped_simulation_data_categorical %>%
  dplyr::select(Null_model_perc, Null_model_pvalues, 
                N, J, M)
colnames(grouped_simulation_data_categorical_p_values_null) <- c('Percentage',
                                                                       'P_values', 
                                                                       'N', 'J', 'M')
grouped_simulation_data_categorical_p_values_null <- 
  grouped_simulation_data_categorical_p_values_null %>%
  pivot_longer(cols = 1:2)
grouped_simulation_data_categorical_p_values_null$Model <- rep('Null', 
                                                                  nrow(grouped_simulation_data_categorical_p_values_null))
colnames(grouped_simulation_data_categorical_p_values_null) <- c('N', 'J', 'M',
                                                                 'Metric', 
                                                                 'Value', 
                                                                 'Model')

grouped_simulation_data_categorical_percentages <- rbind(grouped_simulation_data_categorical_p_values_correct, 
                                                            grouped_simulation_data_categorical_p_values_null)
# Visualization of the simulation study -----
grouped_simulation_data_categorical_percentages %>%
  mutate(p_less = Value > 50) %>%
  group_by(N, J, M, Metric, Model) %>%
  summarise(p_medio = mean(Value),
            porcentagem = mean(Value)) %>%
  View()

################################################################################
#################### End changing the scale ########################################
################################################################################
# grouped_simulation_data_categorical_percentages %>%
#   mutate(p_less = Value > 50) %>%
#   filter(Metric == 'P_values') %>%
#   group_by(N, J, M, Metric, Model)  %>%
#   summarise(frequency = n()) %>%
#   mutate(bin = cut(Value, breaks = seq(min(Value), max(Value), by = 10)))

################################################################################
#################### End changing the scale ########################################
################################################################################

grouped_simulation_data_categorical_percentages$N <-
  factor(grouped_simulation_data_categorical_percentages$N,
         levels = c("50","100","200"))
grouped_simulation_data_categorical_percentages$M <-
  factor(grouped_simulation_data_categorical_percentages$M,
         levels = c("5","10","15"))
grouped_simulation_data_categorical_percentages$J <-
  factor(grouped_simulation_data_categorical_percentages$J,
         levels = c("3","4","5"))

############################### M ==5 ####################################################
my_transformation <-trans_new(
  "my_transformation",
  transform = function(x) x^(1/2),
  inverse = function(x) x^2
)
grouped_simulation_data_categorical_percentages %>%
  filter(Metric == 'P_values', 
         M == '5') %>%
  ggplot(mapping = aes(x = Value, fill = Model)) +
  geom_histogram(bins = 50, position = 'dodge') +
  theme_new() +
  facet_wrap(N~J, scales = "free") +
  ylab('Frequency') +
  xlab('P values') +
  scale_color_manual(values = c("#A3C4D9", "#043259"), 
                     labels = c("Correct", "Null")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259"), 
                    labels = c("Correct", "Null"))+
  scale_y_continuous(#sec.axis = sec_axis(~./1, name = "Correct"), 
                     trans = my_transformation)
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_categorical_rqr_Pvalues_m5.png',  height = 6, width = 8)

grouped_simulation_data_categorical_percentages %>%
  filter(Metric == 'Percentage', 
         M == '5') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type')
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_categorical_rqr_percentage_m5.png', height = 6, width = 8)
############################### M == 10 ####################################################
grouped_simulation_data_categorical_percentages %>%
  filter(Metric == 'P_values', 
         M == '10') %>%
  ggplot(mapping = aes(x = Value, fill = Model)) +
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
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_categorical_rqr_Pvalues_m10.png',  height = 6, width = 8)

grouped_simulation_data_categorical_percentages %>%
  filter(Metric == 'Percentage', 
         M == '10') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_categorical_rqr_percentage_m10.png',  height = 6, width = 8)

############################### M == 15 ####################################################
grouped_simulation_data_categorical_percentages %>%
  filter(Metric == 'P_values', 
         M == '15') %>%
  ggplot(mapping = aes(x = Value, fill = Model)) +
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
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_categorical_rqr_Pvalues_m15.png',  height = 6, width = 8)

grouped_simulation_data_categorical_percentages %>%
  filter(Metric == 'Percentage', 
         M == '15') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_categorical_rqr_percentage_m15.png',  height = 6, width = 8)



##############################################################################################################
###################### Distance based data Continuous ##################################################################
##############################################################################################################
simulation_results_n50_j3_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j3_m5.csv',header = T)[,-1]
simulation_results_n50_j3_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j3_m5.csv',header = T)[,-1]
simulation_results_n100_j3_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j3_m5.csv',header = T)[,-1]
simulation_results_n200_j3_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j4_m5.csv',header = T)[,-1]
simulation_results_n50_j4_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j4_m5.csv',header = T)[,-1]
simulation_results_n100_j4_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j4_m5.csv',header = T)[,-1]
simulation_results_n200_j4_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j5_m5.csv',header = T)[,-1]
simulation_results_n50_j5_m5$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j5_m5.csv',header = T)[,-1]
simulation_results_n100_j5_m5$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j5_m5.csv',header = T)[,-1]
simulation_results_n200_j5_m5$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m5$M <- rep('5', nrow(simulation_results_n50_j3_m5))

#################################### M = 10 ########################################################################
simulation_results_n50_j3_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j3_m10.csv',header = T)[,-1]
simulation_results_n50_j3_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j3_m10.csv',header = T)[,-1]
simulation_results_n100_j3_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j3_m10.csv',header = T)[,-1]
simulation_results_n200_j3_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j4_m10.csv',header = T)[,-1]
simulation_results_n50_j4_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j4_m10.csv',header = T)[,-1]
simulation_results_n100_j4_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j4_m10.csv',header = T)[,-1]
simulation_results_n200_j4_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j5_m10.csv',header = T)[,-1]
simulation_results_n50_j5_m10$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j5_m10.csv',header = T)[,-1]
simulation_results_n100_j5_m10$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j5_m10.csv',header = T)[,-1]
simulation_results_n200_j5_m10$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m10$M <- rep('10', nrow(simulation_results_n50_j3_m5))

#################################### M = 15 ########################################################################
simulation_results_n50_j3_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j3_m15.csv',header = T)[,-1]
simulation_results_n50_j3_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j3_m15.csv',header = T)[,-1]
simulation_results_n100_j3_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j3_m15.csv',header = T)[,-1]
simulation_results_n200_j3_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15$J <- rep('3', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j3_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j4_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j4_m15.csv',header = T)[,-1]
simulation_results_n50_j4_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j4_m15.csv',header = T)[,-1]
simulation_results_n100_j4_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j4_m15.csv',header = T)[,-1]
simulation_results_n200_j4_m15$N <- rep('200', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15$J <- rep('4', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j4_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))

simulation_results_n50_j5_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n50_j5_m15.csv',header = T)[,-1]
simulation_results_n50_j5_m15$N <- rep('50', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n50_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n100_j5_m15.csv',header = T)[,-1]
simulation_results_n100_j5_m15$N <- rep('100', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15$J <- rep('5', nrow(simulation_results_n50_j3_m5))
simulation_results_n100_j5_m15$M <- rep('15', nrow(simulation_results_n50_j3_m5))
simulation_results_n200_j5_m15 <- read.csv('output_data/grouped_data_simulation/randomized_quantile_residuals/continuous_data/simulation_results_n200_j5_m15.csv',header = T)[,-1]
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
  dplyr::select(Correct_model_perc, Correct_model_pvalues, 
                N, J, M)
colnames(grouped_simulation_data_continuous_p_values_correct) <- c('Percentage',
                                                                    'P_values', 
                                                                    'N', 'J', 'M')
grouped_simulation_data_continuous_p_values_correct <- 
  grouped_simulation_data_continuous_p_values_correct %>%
  pivot_longer(cols = 1:2)
grouped_simulation_data_continuous_p_values_correct$Model <- rep('Correct', 
                                                                  nrow(grouped_simulation_data_continuous_p_values_correct))

colnames(grouped_simulation_data_continuous_p_values_correct) <- c('N', 'J', 'M',
                                                                    'Metric', 
                                                                    'Value', 
                                                                    'Model')


grouped_simulation_data_continuous_p_values_null <- grouped_simulation_data_continuous %>%
  dplyr::select(Null_model_perc, Null_model_pvalues, 
                N, J, M)
colnames(grouped_simulation_data_continuous_p_values_null) <- c('Percentage',
                                                                 'P_values', 
                                                                 'N', 'J', 'M')
grouped_simulation_data_continuous_p_values_null <- 
  grouped_simulation_data_continuous_p_values_null %>%
  pivot_longer(cols = 1:2)
grouped_simulation_data_continuous_p_values_null$Model <- rep('Null', 
                                                               nrow(grouped_simulation_data_continuous_p_values_null))
colnames(grouped_simulation_data_continuous_p_values_null) <- c('N', 'J', 'M',
                                                                 'Metric', 
                                                                 'Value', 
                                                                 'Model')

grouped_simulation_data_continuous_percentages <- rbind(grouped_simulation_data_continuous_p_values_correct, 
                                                         grouped_simulation_data_continuous_p_values_null)
# Visualization of the simulation study -----
grouped_simulation_data_continuous_percentages %>%
  mutate(p_less = Value > 50) %>%
  group_by(N, J, M, Metric, Model) %>%
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
  filter(Metric == 'P_values', 
         M == '5') %>%
  ggplot(mapping = aes(x = Value, fill = Model)) +
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
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_continuous_rqr_Pvalues_m5.png',  height = 6, width = 8)

grouped_simulation_data_continuous_percentages %>%
  filter(Metric == 'Percentage', 
         M == '5') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_continuous_rqr_percentage_m5.png',  height = 6, width = 8)
############################### M == 10 ####################################################
grouped_simulation_data_continuous_percentages %>%
  filter(Metric == 'P_values', 
         M == '10') %>%
  ggplot(mapping = aes(x = Value, fill = Model)) +
  geom_histogram(bins = 50, position = 'dodge') +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Frequency') +
  xlab('P values') +
  scale_color_manual(values = c("#A3C4D9", "#043259"), 
                     labels = c("Correct", "Null")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259"), 
                    labels = c("Correct", "Null")) +
  scale_y_continuous(#sec.axis = sec_axis(~./1, name = "Correct"), 
                     trans = "sqrt")
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_continuous_rqr_Pvalues_m10.png',  height = 6, width = 8)

grouped_simulation_data_continuous_percentages %>%
  filter(Metric == 'Percentage', 
         M == '10') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_continuous_rqr_percentage_m10.png',  height = 6, width = 8)

############################### M == 15 ####################################################
grouped_simulation_data_continuous_percentages %>%
  filter(Metric == 'P_values', 
         M == '15') %>%
  ggplot(mapping = aes(x = Value, fill = Model)) +
  geom_histogram(bins = 50, position = 'dodge') +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Frequency') +
  xlab('P values') +
  scale_color_manual(values = c("#A3C4D9", "#043259"), 
                     labels = c("Correct", "Null")) +
  scale_fill_manual(values = c("#A3C4D9", "#043259"), 
                    labels = c("Correct", "Null")) +
  scale_y_continuous(#sec.axis = sec_axis(~./1, name = "Correct"), 
                     trans = "sqrt")
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_continuous_rqr_Pvalues_m15.png',  height = 6, width = 8)

grouped_simulation_data_continuous_percentages %>%
  filter(Metric == 'Percentage', 
         M == '15') %>%
  ggplot(mapping = aes(x = Model, y = Value)) +
  geom_boxplot() +
  theme_new() +
  facet_wrap(N~J, scales = "free_y") +
  ylab('Number of points outside of the envelop (%)') +
  xlab('Model type') 
ggsave('Plots/Grouped_data_results/Rqr_based/grouped_simulation_data_continuous_rqr_percentage_m15.png',  height = 6, width = 8)