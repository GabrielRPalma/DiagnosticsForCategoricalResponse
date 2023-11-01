####################################################################################################
###
### File:    00_source.R
### Purpose: Load required packages and functions used to 
###          create interactive maps.
### Authors: Gabriel Rodrigues Palma
### Date:    19/03/23
###
####################################################################################################

# packages required ---------------------------

packages <- c('nnet', #Fit Multinomial Log-linear Models
              'hnp', #half-normal plot using envelope simulation
              'pmultinom', #Calculate cdf of multinomial dist
              'ggplot2', # Data visualisation
              'dplyr', # Data processing
              'tidyr', # Data processing
              'moments', # Functions to calculate: moments, Pearson's kurtosis, Geary's kurtosis and skewness; tests related to them
              'gridExtra', # Provides a number of user-level functions to work with ``grid'' graphics, notably to arrange multiple grid-based plots on a page, and draw tables.
              'here', 
              'scatterplot3d', 
              'VGAM',
              'haven'
)

install.packages(setdiff(packages, rownames(installed.packages())), dependencies = T)
lapply(packages, library, character.only = TRUE)
if (!require("ggflags")) {
  devtools::install_github("rensa/ggflags")
  library(ggflags)
}

# Main functions ---------------------------

create_prob_vector_categ_data <- function(n, j){
  # This function receives the number of categories 
  # and creates a vector of probability for each category
  if(j == 3){
    x <<- rnorm(n) # Covariate
    x_factor<-gl(2, n / 2, labels = c("Control", "Treatment")) # categorical covariate
    z2 <- 1.5-3*x + 1.5*(x_factor=="Treatment")
    z3 <- 3-5*x + 2.5*(x_factor=="Treatment")
    
    den <- 1+exp(z2)+exp(z3)
    p1 <- 1/den; p2<-exp(z2)/den; p3<-exp(z3)/den
    prob <- cbind(p1,p2,p3)
    
  }
  else if(j == 4){
    
    x <<- rnorm(n) # Covariate
    x_factor<-gl(2, n / 2, labels = c("Control", "Treatment")) # categorical covariate
    z2 <- 1.5-3*x + 1.5*(x_factor=="Treatment")
    z3 <- 3-5*x + 2.5*(x_factor=="Treatment")
    z4 <- 2 - 4*x + 3*(x_factor=="Treatment")
    
    den <- 1+exp(z2)+exp(z3)+exp(z4)
    p1 <- 1/den; p2<-exp(z2)/den; p3<-exp(z3)/den; p4<-exp(z4)/den
    prob <- cbind(p1,p2,p3, p4)
    
  }
  else{
    x <<- rnorm(n) # Covariate
    x_factor<-gl(2, n / 2, labels = c("Control", "Treatment")) # categorical covariate
    z2 <- 1.5-3*x + 1.5*(x_factor=="Treatment")
    z3 <- 3-5*x + 2.5*(x_factor=="Treatment")
    z4 <- 2 - 4*x + 3*(x_factor=="Treatment")
    z5 <- 4 - 7*x + 3.5*(x_factor=="Treatment")
    
    den <- 1+exp(z2)+exp(z3)+exp(z4)+exp(z5)
    p1 <- 1/den; p2<-exp(z2)/den; p3<-exp(z3)/den; p4<-exp(z4)/den;p5 <- exp(z5)/den
    prob <- cbind(p1,p2,p3, p4, p5)
  }
  result <- list()
  result$prob <- prob
  result$x_factor <- x_factor
  result$x <- x
  
  return(result)
}

create_prob_vector_cont_data <- function(n, j){
  # This function receives the number of categories 
  # and creates a vector of probability for each category
  if(j == 3){
    x <<- rnorm(n) # Covariate
    z2 <- 1.5-3*x
    z3 <- 3-5*x
    den <- 1+exp(z2)+exp(z3)
    p1 <- 1/den; p2<-exp(z2)/den; p3<-exp(z3)/den
    prob <- cbind(p1,p2,p3)
    
  }
  else if(j == 4){
    
    x <<- rnorm(n) # Covariate
    z2 <- 1.5-3*x
    z3 <- 3-5*x
    z4 <- 2 - 4*x
    den <- 1+exp(z2)+exp(z3)+exp(z4)
    p1 <- 1/den; p2<-exp(z2)/den; p3<-exp(z3)/den; p4<-exp(z4)/den
    prob <- cbind(p1,p2,p3, p4)
    
  }
  else{
    x <<- rnorm(n) # Covariate
    z2 <- 1.5-3*x
    z3 <- 3-5*x
    z4 <- 2 - 4*x
    z5 <- 4 - 7*x
    den <- 1+exp(z2)+exp(z3)+exp(z4)+exp(z5)
    p1 <- 1/den; p2<-exp(z2)/den; p3<-exp(z3)/den; p4<-exp(z4)/den;p5 <- exp(z5)/den
    prob <- cbind(p1,p2,p3, p4, p5)
  }
  result <- list()
  result$prob <- prob
  result$x <- x
  
  return(result)
}

## Implementing new class for hnp
simulation_fun <- function(n, model_obj) {
  # This function was created to obtain probability prediction from a given model to the hnp function
  pred <-predict(model_obj, type = "prob")
  newresp <- t(apply(pred, 1, function(x) rmultinom(1, size = m, x)))
  newresp
}

null_model_fun <- function(newresp) {
  # This function was created to add the null model to the hnp function
  multinom(newresp ~ 1, data = polytomous_data_frame) 
}

correct_model_fun <- function(newresp) {
  # This function was created to add the correct model to the hnp function
  multinom(newresp ~ x, data = polytomous_data_frame) 
}
categorical_correct_model_fun <- function(newresp) {
  # This function was created to add the categorical correct model to the hnp function
  multinom(newresp ~ x+x_factors, data = polytomous_data_frame) 
}

euclidian_distance<- function(obj) {
  # This function obtains the Euclidian distance for the resusials of a certain model
  r <- resid(obj)
  l2_r <- apply(r, 1, function(x) dist(rbind(x,rep(0,length(x)))))
  return(as.numeric(l2_r))
}

mahalanobis_distance <- function(obj) {
  # This function obtaing the Mahalanobs distance for the resusials of a certain model
  
  r <- resid(obj)
  k<-ncol(r)
  Sr<-solve(cov(r)+diag(rep(1e-6,k)))
  D2 <- mahalanobis(r, rep(0,nrow(r)),Sr,inverted = T)
  return(D2)
}

get_randomized_quantile_residuals <- function(y_index, y,
                                              model_predictions, 
                                              y_vector){

  # This function obtain the randomized quantile residuals (rqr) for individual data
  # Function details.
  # Input:
  #        y_index = Position index of the category y_ij = 1 on the vector y
  #        y_vector = Vector containing a J elements 1.
  #        y = Matrix of polytomous response variables
  #        model_predictions = The predicted probabilities for each element of a polytomous response variable
  # Output:
  #        rqr = The randomized quantile residuals
  
  J <- length(y_vector)
  n <- nrow(y)
  rqr <- vector()
  for (i in 1:n) {
    y_vector[unlist(y_index[i])] <- 0
    rqr[i] <- NaN
    #indicator <- 1
    while(is.na(rqr[i]) | is.infinite(rqr[i])){
      #cat('Loading the function for th', indicator, "th time \n")
      rqr[i] <- qnorm(pmultinom(upper = y_vector, size = 1, probs = model_predictions[i,], method="exact") +
                        dmultinom(y[i,], size = 1, model_predictions[i,]) * runif(1))
      #indicator <- indicator + 1
    }
    
    y_vector <- rep(1, J)
  }
  return(rqr)
}

get_grouped_randomized_quantile_residuals <- function(y,
                                              model_predictions, 
                                              m){
  
  # This function obtain the randomized quantile residuals (rqr) for individual data
  # Function details.
  # Input:
  #        y = Matrix of polytomous response variables
  #        model_predictions = The predicted probabilities for each element of a polytomous response variable
  # Output:
  #        rqr = The randomized quantile residuals
  
  rqr <- vector()
  n <- nrow(y)
  for (i in 1:n) {
    rqr[i] <- NaN
    #indicator <- 1
    while(is.na(rqr[i]) | is.infinite(rqr[i])){
      #cat('Loading the function for th', indicator, "th time \n")
      rqr[i] <- qnorm(pmultinom(upper = m-y[i,], size = m, probs = model_predictions[i,], method="exact") +
                        dmultinom(y[i,], size = m, model_predictions[i,]) * runif(1))
      #indicator <- indicator + 1
    }
    
    
  }
  return(rqr)
}

obtain_individual_continuous_data_simulation_statistics <- function(x,prob, y_vector) {
  # This function obtains the simulation's results for individual data presented in section 6.2 of the paper.
  # Function details
  # Input: 
  #       x = 
  #       prob =
  #       y_vector = Vector containing a J elements 1.
  #       N = Sample size
  #       J = Number of categories on the polytomous response variables 
  # Output:
  #       simulation_statistics = A data frame containing the simulation's statistics for individual data.
  y <- t(apply(prob, 1, rmultinom, n=1, size = 1))
  y_index <-factor(apply(y, 1, function(x) which(x==1))) #Possibly I can change it here
  
  polytomous_data_frame <- data.frame(x,y)
  
  # Fittings null and correct models
  null_model_fit <- multinom(y~ 1,data = polytomous_data_frame,trace="FALSE") 
  correct_model_fit <- multinom(y ~ x,data = polytomous_data_frame,trace="FALSE") 
  
  # Obtaining probabilities based on the created models
  null_model_pred <- predict(null_model_fit, type = "prob")
  correct_model_pred <- predict(correct_model_fit, type = "prob")
  
  # Obtaining p values for the created models
  p_value <- anova(null_model_fit, correct_model_fit)[2,7] #p-value of the test
  
  
  # Obtaining Randomized quantile residuals
  null_model_rqr <- get_randomized_quantile_residuals(y_index = y_index, y = y, 
                      model_predictions =  null_model_pred, y_vector = y_vector)
  correct_model_rqr <- get_randomized_quantile_residuals(y_index = y_index, y = y, 
                                                         model_predictions =  correct_model_pred, 
                                                         y_vector = y_vector)
  
  invisible(capture.output(null_model_hnp <- hnp(null_model_rqr, print = T, scale = T, plot.sim = "FALSE", sim = 1000)))
  invisible(capture.output(correct_model_hnp <- hnp(correct_model_rqr, print = T, scale = T, plot.sim = "FALSE", sim = 1000)))
  
  #Obtaining the percentage of points outside the envelope
  null_model_npoints <- null_model_hnp$out
  null_model_perc <-round((null_model_npoints/null_model_hnp$total)*100,2)
  
  correct_model_npoints <- correct_model_hnp$out
  correct_model_perc <- round((correct_model_npoints/correct_model_hnp$total)*100,2)
  
  # Obtaining the difference of points outside the envelope between the incorrect and correct models
  differ <- null_model_perc - correct_model_perc
  
  
  # Performing the Shapiro Wilk Test in reasiduals based on the created models to extract p values
  null_model_pvalues <- shapiro.test(null_model_rqr)$p.value
  correct_model_pvalues <- shapiro.test(correct_model_rqr)$p.value
  
  # Obtaining statistics of the null model based on the randomized quantile residuals
  null_model_mean <- mean(null_model_rqr)
  null_model_sd <- sd(null_model_rqr)
  null_model_kurt <- kurtosis(null_model_rqr)
  null_model_skew<-skewness(null_model_rqr)
  
  # Obtaining statistics of the correct model based on the randomized quantile residuals
  correct_model_mean <- mean(correct_model_rqr)
  correct_model_sd <- sd(correct_model_rqr)
  correct_model_kurt <- kurtosis(correct_model_rqr)
  correct_model_skew <- skewness(correct_model_rqr)
  
  
  # Compiling the main results of the simulation
  simulation_statistics <- data.frame(Correct_model_perc = correct_model_perc, 
                                  Correct_model_pvalues = correct_model_pvalues,
                                  Correct_model_mean = correct_model_mean, 
                                  Correct_model_sd = correct_model_sd, 
                                  Correct_model_kurt = correct_model_kurt, 
                                  Correct_model_skew = correct_model_skew,
                                 
                                  Null_model_perc = null_model_perc, 
                                  Null_model_pvalues = null_model_pvalues,
                                  Null_model_mean = null_model_mean, 
                                  Null_model_sd = null_model_sd, 
                                  Null_model_kurt = null_model_kurt, 
                                  Null_model_skew = null_model_skew,
                                 
                                  Percentage_difference = differ, 
                                  P_value = p_value)

  
  return(simulation_statistics)
  
  
  
}


obtain_individual_categorical_data_simulation_statistics <- function(x, x_factors, prob, y_vector) {
  # This function obtains the simulation's results for individual data presented in section 6.2 of the paper.
  # Function details
  # Input: 
  #       x = 
  #       x_factors = The factors involved in each treatment
  #       prob =
  #       y_vector = Vector containing a J elements 1.
  #       N = Sample size
  #       J = Number of categories on the polytomous response variables 
  # Output:
  #       simulation_statistics = A data frame containing the simulation's statistics for individual data.
  y <- t(apply(prob, 1, rmultinom,n=1, size = 1))
  y_index <-factor(apply(y, 1, function(x) which(x==1)))
  
  polytomous_data_frame <- data.frame(x, x_factors, y)
  
  # Fittings null and correct models
  null_model_fit <- multinom(y~ 1,data = polytomous_data_frame,trace="FALSE") 
  correct_model_fit <- multinom(y ~ x+x_factors,data = polytomous_data_frame,trace="FALSE") 
  
  # Obtaining probabilities based on the created models
  null_model_pred <- predict(null_model_fit, type = "prob")
  correct_model_pred <- predict(correct_model_fit, type = "prob")
  
  # Obtaining p values for the created models
  p_value <- anova(null_model_fit, correct_model_fit)[2,7] #p-value of the test
  
  
  # Obtaining Randomized quantile residuals
  null_model_rqr <- get_randomized_quantile_residuals(y_index = y_index, y = y, 
                                                      model_predictions =  null_model_pred,
                                                      y_vector = y_vector)
  correct_model_rqr <- get_randomized_quantile_residuals(y_index = y_index, y = y, 
                                                         model_predictions =  correct_model_pred, 
                                                         y_vector = y_vector)
  
  invisible(capture.output(null_model_hnp <- hnp(null_model_rqr, print = T, scale = T, plot.sim = "FALSE", sim = 1000)))
  invisible(capture.output(correct_model_hnp <- hnp(correct_model_rqr, print = T, scale = T, plot.sim = "FALSE", sim = 1000)))
  
  #Obtaining the percentage of points outside the envelope
  null_model_npoints <- null_model_hnp$out
  null_model_perc <-round((null_model_npoints/null_model_hnp$total)*100,2)
  
  correct_model_npoints <- correct_model_hnp$out
  correct_model_perc <- round((correct_model_npoints/correct_model_hnp$total)*100,2)
  
  # Obtaining the difference of points outside the envelope between the incorrect and correct models
  differ <- null_model_perc - correct_model_perc
  
  
  # Performing the Shapiro Wilk Test in reasiduals based on the created models to extract p values
  null_model_pvalues <- shapiro.test(null_model_rqr)$p.value
  correct_model_pvalues <- shapiro.test(correct_model_rqr)$p.value
  
  # Obtaining statistics of the null model based on the randomized quantile residuals
  null_model_mean <- mean(null_model_rqr)
  null_model_sd <- sd(null_model_rqr)
  null_model_kurt <- kurtosis(null_model_rqr)
  null_model_skew<-skewness(null_model_rqr)
  
  # Obtaining statistics of the correct model based on the randomized quantile residuals
  correct_model_mean <- mean(correct_model_rqr)
  correct_model_sd <- sd(correct_model_rqr)
  correct_model_kurt <- kurtosis(correct_model_rqr)
  correct_model_skew <- skewness(correct_model_rqr)
  
  
  # Compiling the main results of the simulation
  simulation_statistics <- data.frame(Correct_model_perc = correct_model_perc, 
                                      Correct_model_pvalues = correct_model_pvalues,
                                      Correct_model_mean = correct_model_mean, 
                                      Correct_model_sd = correct_model_sd, 
                                      Correct_model_kurt = correct_model_kurt, 
                                      Correct_model_skew = correct_model_skew,
                                      
                                      Null_model_perc = null_model_perc, 
                                      Null_model_pvalues = null_model_pvalues,
                                      Null_model_mean = null_model_mean, 
                                      Null_model_sd = null_model_sd, 
                                      Null_model_kurt = null_model_kurt, 
                                      Null_model_skew = null_model_skew,
                                      
                                      Percentage_difference = differ, 
                                      P_value = p_value)
  
  
  return(simulation_statistics)
  
  
  
}


# Grouped data -----
obtain_group_continuous_data_simulation_statistics <- function(x,prob, m) {
  # This function obtains the simulation's results for individual data presented in section 6.2 of the paper.
  # Function details
  # Input: 
  #       y_vector = Vector containing a J elements 1.
  #       N = Sample size
  #       J = Number of categories on the polytomous response variables 
  # Output:
  #       simulation_statistics = A data frame containing the simulation's statistics for individual data.
  y <- t(apply(prob, 1, rmultinom,n=1, size = m))
  
  polytomous_data_frame <- data.frame(x,y)
  
  # Fittings null and correct models
  null_model_fit <- multinom(y~ 1,data = polytomous_data_frame,trace="FALSE") 
  correct_model_fit <- multinom(y ~ x,data = polytomous_data_frame,trace="FALSE") 
  
  # Obtaining probabilities based on the created models
  null_model_pred <- predict(null_model_fit, type = "prob")
  correct_model_pred <- predict(correct_model_fit, type = "prob")
  
  # Obtaining p values for the created models
  p_value <- anova(null_model_fit, correct_model_fit)[2,7] #p-value of the test
  
  
  # Obtaining Randomized quantile residuals
  null_model_rqr <- get_grouped_randomized_quantile_residuals(y = y, 
                                                      model_predictions =  null_model_pred, 
                                                      m = m)
  correct_model_rqr <- get_grouped_randomized_quantile_residuals(y = y, 
                                                         model_predictions =  correct_model_pred, 
                                                         m = m)
  
  invisible(capture.output(null_model_hnp <- hnp(null_model_rqr, print = T, scale = T, plot.sim = "FALSE", sim = 1000)))
  invisible(capture.output(correct_model_hnp <- hnp(correct_model_rqr, print = T, scale = T, plot.sim = "FALSE", sim = 1000)))
  
  #Obtaining the percentage of points outside the envelope
  null_model_npoints <- null_model_hnp$out
  null_model_perc <-round((null_model_npoints/null_model_hnp$total)*100,2)
  
  correct_model_npoints <- correct_model_hnp$out
  correct_model_perc <- round((correct_model_npoints/correct_model_hnp$total)*100,2)
  
  # Obtaining the difference of points outside the envelope between the incorrect and correct models
  differ <- null_model_perc - correct_model_perc
  
  
  # Performing the Shapiro Wilk Test in reasiduals based on the created models to extract p values
  null_model_pvalues <- shapiro.test(null_model_rqr)$p.value
  correct_model_pvalues <- shapiro.test(correct_model_rqr)$p.value
  
  # Obtaining statistics of the null model based on the randomized quantile residuals
  null_model_mean <- mean(null_model_rqr)
  null_model_sd <- sd(null_model_rqr)
  null_model_kurt <- kurtosis(null_model_rqr)
  null_model_skew<-skewness(null_model_rqr)
  
  # Obtaining statistics of the correct model based on the randomized quantile residuals
  correct_model_mean <- mean(correct_model_rqr)
  correct_model_sd <- sd(correct_model_rqr)
  correct_model_kurt <- kurtosis(correct_model_rqr)
  correct_model_skew <- skewness(correct_model_rqr)
  
  
  # Compiling the main results of the simulation
  simulation_statistics <- data.frame(Correct_model_perc = correct_model_perc, 
                                      Correct_model_pvalues = correct_model_pvalues,
                                      Correct_model_mean = correct_model_mean, 
                                      Correct_model_sd = correct_model_sd, 
                                      Correct_model_kurt = correct_model_kurt, 
                                      Correct_model_skew = correct_model_skew,
                                      
                                      Null_model_perc = null_model_perc, 
                                      Null_model_pvalues = null_model_pvalues,
                                      Null_model_mean = null_model_mean, 
                                      Null_model_sd = null_model_sd, 
                                      Null_model_kurt = null_model_kurt, 
                                      Null_model_skew = null_model_skew,
                                      
                                      Percentage_difference = differ, 
                                      P_value = p_value)
  
  
  return(simulation_statistics)
  
  
  
}


obtain_group_categorical_data_simulation_statistics <- function(x, x_factors, prob, m) {
  # This function obtains the simulation's results for individual data presented in section 6.2 of the paper.
  # Function details
  # Input: 
  #       x = Explanatory variable
  #       x_factors = The factors involved in each treatment
  #       prob =
  #       y_vector = Vector containing a J elements 1.
  #       N = Sample size
  #       J = Number of categories on the polytomous response variables 
  # Output:
  #       simulation_statistics = A data frame containing the simulation's statistics for individual data.
  y <- t(apply(prob, 1, rmultinom,n=1, size = m))
  
  polytomous_data_frame <- data.frame(x, x_factors, y)
  
  # Fittings null and correct models
  null_model_fit <- multinom(y~ 1,data = polytomous_data_frame,trace="FALSE") 
  correct_model_fit <- multinom(y ~ x+x_factors,data = polytomous_data_frame,trace="FALSE") 
  
  # Obtaining probabilities based on the created models
  null_model_pred <- predict(null_model_fit, type = "prob")
  correct_model_pred <- predict(correct_model_fit, type = "prob")
  
  # Obtaining p values for the created models
  p_value <- anova(null_model_fit, correct_model_fit)[2,7] #p-value of the test
  
  
  # Obtaining Randomized quantile residuals
  null_model_rqr <- get_grouped_randomized_quantile_residuals(y = y, 
                                                      model_predictions =  null_model_pred, 
                                                      m = m)
  correct_model_rqr <- get_grouped_randomized_quantile_residuals(y = y, 
                                                         model_predictions =  correct_model_pred, 
                                                         m = m)
  
  invisible(capture.output(null_model_hnp <- hnp(null_model_rqr, print = T, scale = T, plot.sim = "FALSE", sim = 1000)))
  invisible(capture.output(correct_model_hnp <- hnp(correct_model_rqr, print = T, scale = T, plot.sim = "FALSE", sim = 1000)))
  
  #Obtaining the percentage of points outside the envelope
  null_model_npoints <- null_model_hnp$out
  null_model_perc <-round((null_model_npoints/null_model_hnp$total)*100,2)
  
  correct_model_npoints <- correct_model_hnp$out
  correct_model_perc <- round((correct_model_npoints/correct_model_hnp$total)*100,2)
  
  # Obtaining the difference of points outside the envelope between the incorrect and correct models
  differ <- null_model_perc - correct_model_perc
  
  
  # Performing the Shapiro Wilk Test in reasiduals based on the created models to extract p values
  null_model_pvalues <- shapiro.test(null_model_rqr)$p.value
  correct_model_pvalues <- shapiro.test(correct_model_rqr)$p.value
  
  # Obtaining statistics of the null model based on the randomized quantile residuals
  null_model_mean <- mean(null_model_rqr)
  null_model_sd <- sd(null_model_rqr)
  null_model_kurt <- kurtosis(null_model_rqr)
  null_model_skew<-skewness(null_model_rqr)
  
  # Obtaining statistics of the correct model based on the randomized quantile residuals
  correct_model_mean <- mean(correct_model_rqr)
  correct_model_sd <- sd(correct_model_rqr)
  correct_model_kurt <- kurtosis(correct_model_rqr)
  correct_model_skew <- skewness(correct_model_rqr)
  
  
  # Compiling the main results of the simulation
  simulation_statistics <- data.frame(Correct_model_perc = correct_model_perc, 
                                      Correct_model_pvalues = correct_model_pvalues,
                                      Correct_model_mean = correct_model_mean, 
                                      Correct_model_sd = correct_model_sd, 
                                      Correct_model_kurt = correct_model_kurt, 
                                      Correct_model_skew = correct_model_skew,
                                      
                                      Null_model_perc = null_model_perc, 
                                      Null_model_pvalues = null_model_pvalues,
                                      Null_model_mean = null_model_mean, 
                                      Null_model_sd = null_model_sd, 
                                      Null_model_kurt = null_model_kurt, 
                                      Null_model_skew = null_model_skew,
                                      
                                      Percentage_difference = differ, 
                                      P_value = p_value)
  
  
  return(simulation_statistics)
  
  
  
}

##### Distance based metrics 
obtain_group_continuous_data_simulation_distance_metrics <- function(x,prob, m) {
  # This function obtains the simulation's results for individual data presented in section 6.2 of the paper.
  # Function details
  # Input: 
  #       x = 
  #       prob =
  #       y_vector = Vector containing a J elements 1.
  #       N = Sample size
  #       J = Number of categories on the polytomous response variables 
  # Output:
  #       simulation_statistics = A data frame containing the simulation's statistics for individual data.
  y <- t(apply(prob, 1, rmultinom,n=1, size = m))
  
  polytomous_data_frame <<- data.frame(x,y)
  
  # Fittings null and correct models
  null_model_fit <- multinom(y~ 1,data = polytomous_data_frame,trace="FALSE") 
  correct_model_fit <- multinom(y ~ x,data = polytomous_data_frame,trace="FALSE") 
  
  # Obtaining probabilities based on the created models
  null_model_pred <- predict(null_model_fit, type = "prob")
  correct_model_pred <- predict(correct_model_fit, type = "prob")
  
  n <- length(x)
  
  ########################################## Eucledian distance metrics ##############################################################
  invisible(capture.output(null_model_hnp <- hnp(null_model_fit,
                                                 newclass = TRUE,
                                                 diagfun = euclidian_distance, 
                                                 simfun = simulation_fun, 
                                                 fitfun = null_model_fun,
                                                 how.many.out = T, 
                                                 plot.sim = "FALSE", sim = 1000)))
  invisible(capture.output(correct_model_hnp <- hnp(correct_model_fit, 
                                                    newclass = TRUE,
                                                    diagfun = euclidian_distance, 
                                                    simfun = simulation_fun, 
                                                    fitfun = correct_model_fun, 
                                                    how.many.out = T,
                                                    plot.sim = "FALSE", sim = 1000)))
  
  #Obtaining the percentage of points outside the envelope
  null_model_npoints <- null_model_hnp$out
  null_model_perc_eucledian <-round((null_model_npoints/null_model_hnp$total)*100,2)
  
  correct_model_npoints <- correct_model_hnp$out
  correct_model_perc_eucledian <- round((correct_model_npoints/correct_model_hnp$total)*100,2)
  
  # Obtaining the difference of points outside the envelope between the incorrect and correct models
  differ_eucledian <- null_model_perc_eucledian - correct_model_perc_eucledian
  
  ########################################## Mahalanovis distance metrics ##############################################################
  invisible(capture.output(null_model_hnp <- hnp(null_model_fit, 
                                                 print = T, scale = T, 
                                                 newclass = TRUE,
                                                 diagfun = euclidian_distance, 
                                                 simfun = simulation_fun, 
                                                 fitfun = null_model_fun,
                                                 how.many.out = T, 
                                                 plot.sim = "FALSE", sim = 1000)))
  invisible(capture.output(correct_model_hnp <- hnp(correct_model_fit, 
                                                    newclass = TRUE,
                                                    print = T, scale = T, 
                                                    diagfun = mahalanobis_distance, 
                                                    simfun = simulation_fun, 
                                                    fitfun = correct_model_fun, 
                                                    how.many.out = T,
                                                    plot.sim = "FALSE", sim = 1000)))
  
  #Obtaining the percentage of points outside the envelope
  null_model_npoints <- null_model_hnp$out
  null_model_perc_mahalanobis <-round((null_model_npoints/null_model_hnp$total)*100,2)
  
  correct_model_npoints <- correct_model_hnp$out
  correct_model_perc_mahalanobis <- round((correct_model_npoints/correct_model_hnp$total)*100,2)
  
  # Obtaining the difference of points outside the envelope between the incorrect and correct models
  differ_mahalanobis <- null_model_perc_mahalanobis - correct_model_perc_mahalanobis
  
  # Compiling the main results of the simulation
  simulation_statistics <- data.frame(Correct_model_perc_eucledian = correct_model_perc_eucledian, 
                                      Correct_model_perc_mahalanovis = correct_model_perc_mahalanobis, 
                                      
                                      Null_model_perc_eucledian = null_model_perc_eucledian, 
                                      Null_model_perc_mahalanovis = null_model_perc_mahalanobis, 
                                      
                                      Percentage_difference_eucledian = differ_eucledian, 
                                      Percentage_difference_mahalanovis = differ_mahalanobis)
  
  
  return(simulation_statistics)
  
  
  
}


obtain_group_categorical_data_simulation_distance_metrics <- function(x, 
                                                                      x_factors, 
                                                                      prob, m) {
  # This function obtains the simulation's results for individual data presented in section 6.2 of the paper.
  # Function details
  # Input: 
  #       x = Explanatory variable
  #       x_factors = The factors involved in each treatment
  #       prob =
  #       J = Number of categories on the polytomous response variables 
  # Output:
  #       simulation_statistics = A data frame containing the simulation's statistics for individual data.
  m <<- m
  y <- t(apply(prob, 1, rmultinom,n=1, size = m))
  
  polytomous_data_frame <<- data.frame(x, x_factors, y)
  
  # Fittings null and correct models
  null_model_fit <- multinom(y~ 1,data = polytomous_data_frame,trace="FALSE") 
  correct_model_fit <- multinom(y ~ x+x_factors,data = polytomous_data_frame,trace="FALSE") 
  
  ########################################## Eucledian distance metrics ##############################################################
  invisible(capture.output(null_model_hnp <- hnp(null_model_fit, 
                                                 print = T, scale = T, 
                                                 newclass = TRUE,
                                                 diagfun = euclidian_distance, 
                                                 simfun = simulation_fun, 
                                                 fitfun = null_model_fun,
                                                 how.many.out = T, 
                                                 plot.sim = "FALSE", sim = 1000)))
  invisible(capture.output(correct_model_hnp <- hnp(correct_model_fit, 
                                                    newclass = TRUE,
                                                    print = T, scale = T, 
                                                    diagfun = euclidian_distance, 
                                                    simfun = simulation_fun, 
                                                    fitfun = categorical_correct_model_fun, 
                                                    how.many.out = T,
                                                    plot.sim = "FALSE", sim = 1000)))
  
  #Obtaining the percentage of points outside the envelope
  null_model_npoints <- null_model_hnp$out
  null_model_perc_eucledian <-round((null_model_npoints/null_model_hnp$total)*100,2)
  
  correct_model_npoints <- correct_model_hnp$out
  correct_model_perc_eucledian <- round((correct_model_npoints/correct_model_hnp$total)*100,2)
  
  # Obtaining the difference of points outside the envelope between the incorrect and correct models
  differ_eucledian <- null_model_perc_eucledian - correct_model_perc_eucledian
  
  ########################################## Mahalanovis distance metrics ##############################################################
  invisible(capture.output(null_model_hnp <- hnp(null_model_fit, 
                                                 print = T, scale = T, 
                                                 newclass = TRUE,
                                                 diagfun = euclidian_distance, 
                                                 simfun = simulation_fun, 
                                                 fitfun = null_model_fun,
                                                 how.many.out = T, 
                                                 plot.sim = "FALSE", sim = 1000)))
  invisible(capture.output(correct_model_hnp <- hnp(correct_model_fit, 
                                                    newclass = TRUE,
                                                    print = T, scale = T, 
                                                    diagfun = mahalanobis_distance, 
                                                    simfun = simulation_fun, 
                                                    fitfun = categorical_correct_model_fun, 
                                                    how.many.out = T,
                                                    plot.sim = "FALSE", sim = 1000)))
  
  #Obtaining the percentage of points outside the envelope
  null_model_npoints <- null_model_hnp$out
  null_model_perc_mahalanobis <-round((null_model_npoints/null_model_hnp$total)*100,2)
  
  correct_model_npoints <- correct_model_hnp$out
  correct_model_perc_mahalanobis <- round((correct_model_npoints/correct_model_hnp$total)*100,2)
  
  # Obtaining the difference of points outside the envelope between the incorrect and correct models
  differ_mahalanobis <- null_model_perc_mahalanobis - correct_model_perc_mahalanobis
  
  # Compiling the main results of the simulation
  simulation_statistics <- data.frame(Correct_model_perc_eucledian = correct_model_perc_eucledian, 
                                      Correct_model_perc_mahalanovis = correct_model_perc_mahalanobis, 
                                      
                                      Null_model_perc_eucledian = null_model_perc_eucledian, 
                                      Null_model_perc_mahalanovis = null_model_perc_mahalanobis, 
                                      
                                      Percentage_difference_eucledian = differ_eucledian, 
                                      Percentage_difference_mahalanovis = differ_mahalanobis)
  
  
  return(simulation_statistics)
  
  
  
}


# plot settings ---------------------------
pallete = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]

theme_new <- function(base_size = 10, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 10, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}