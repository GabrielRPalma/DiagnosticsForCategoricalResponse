####################################################################################################
###
### File:    04_school_example.r
### Purpose: Code required for the applying the techniques proposed by the paper
###         using the school dataset
###          create interactive maps.
### Authors: Gabriel Rodrigues Palma
### Date:    02/04/23
###
####################################################################################################
# Loading packages -----
source('00_source.r')

# Read dataset -----
load('input_data/wine.RData')
str(wine)
train <- sample_frac(wine, 1) # Subsample of the data
attach(train)
table(Type)

# Visualisation of the data -----
levels(train$Type)<-c("1", "2", "3")
ggplot(train, aes(x = Type)) +
  theme_new() +
  geom_bar(width = 0.3, show.legend = FALSE) + 
  ylab('Frequency')+
  xlab("Cultivar")
ggsave('Plots/Examples/Wine/wine_dataset.png', height = 4, width = 6)

# Statistical analysis -----
train$Type <- relevel(train$Type, ref = "1") # Setting the basline 

multinom_fit0 <- multinom(Type ~ 1, data = train) # Training the multinomial model
multinom_fit1 <- multinom(Type ~ Phenols, data = train)
multinom_fit2 <- multinom(Type ~  Phenols + Magnesium, data = train)
multinom_fit3 <- multinom(Type ~ Phenols * Magnesium, data = train)

## Obtaining models statistics
anova(multinom_fit0,multinom_fit1,
      multinom_fit2,multinom_fit3)
summary(multinom_fit2)

AIC <- c(multinom_fit0$AIC,
         multinom_fit1$AIC,
         multinom_fit2$AIC,
         multinom_fit3$AIC)

confint(multinom_fit2,type = "Wald") 

## Plot obs probabilities vs estimated probabilities
multinom_fit0_prob <- predict(multinom_fit0, type = "prob")

multinom_fit0_pred <- predict(multinom_fit0,
                              se.fit=TRUE,interval = T) 

multinom_fit2_prob <- predict(multinom_fit2, type = "prob")

multinom_fit2_pred <- predict(multinom_fit2,
                              se.fit=TRUE,interval = T) 
prob <- prop.table(table(multinom_fit2_pred))

probs <-as.vector(t(prob)) #predict

p1 <- as.vector(t(prop.table(table(Type)))) #observed


prob_final<-data.frame(cultivar=rep(1:3, times=2),
                      response=rep((1:3),times=2))


final_dataset <- cbind(prob_final,
                       proba=c(probs,p1),
                       type=rep(c("Estimated","Observed"),each=3))

final_dataset$cultivar <- as.factor(final_dataset$cultivar)
final_dataset$response <- as.factor(final_dataset$response)

levels(final_dataset$response)<-c("1","2","3")
levels(final_dataset$cultivar)<-c("Cultivar 1","Cultivar 2", "Cultivar 3")

final_dataset %>%
  ggplot(mapping =aes(x=response, y=proba, colour=type)) +
  geom_point(size=3) +
  theme_new() +
  xlim("1","2","3")+
  xlab('\n Cultivar \n')+ ylab('Proportion\n')+ylim(0,0.41)+
  scale_colour_manual(name="",breaks=c('Estimated','Observed'),
                      values=c('#A3C4D9','#043259')) 
ggsave('Plots/Examples/Wine/Propotions_estimated_observed_plot.png', height = 4, width = 6)

# Analysis goodness of fit ----
####################################################################################################
######################################## Correct model #############################################
####################################################################################################
y_index <- as.numeric(train$Type)
n <- length(y_index)
y <- matrix(rep(0, length(y_index) * 3), length(y_index), 3)
for(i in 1:n){
  y[i,][y_index[i]] <- 1
}


rqr <- get_randomized_quantile_residuals(y_index = y_index, 
                                         y = y,
                                         model_predictions = multinom_fit2_prob, 
                                         y_vector = c(1, 1, 1))

rqr_normalised <- (rqr-mean(rqr))/sd(rqr)

mean_x<-mean(rqr_normalised); sd_x<-sd(rqr_normalised)

par(mfrow=c(1,2))
hist(rqr_normalised, freq = FALSE, main = "(a)",xlab = "Randomized quantile residuals",  col = c("lightblue"))
curve(dnorm(x, mean = mean_x, sd = sd_x), add = TRUE, col = "red",lwd = 2)

kurtosis(rqr_normalised);skewness(rqr_normalised)

#QQplot
#qqnorm(rqr_normalised);abline(a=0,b=1)

#Shapiro-Wilk test
shapiro.test(rqr_normalised)$p.value

# hnp and Residual vs fitted values -----
value_fit <- predict(multinom_fit2, type = "class")

ym<-matrix(NA,178,3)

for (i in 1:n) {
  if(value_fit[i]==1){
    ym[i,] <-c(1,0,0)
    
  }else if(value_fit[i]==2){
    ym[i,] <-c(0,1,0)
    
  }else{
    ym[i,] <-c(0,0,1)}}

x <- ym * multinom_fit2_prob
xf <- t(x[1:n,])[which(t(x[1:n,])>0)]

plot(xf,rqr_normalised,ylab = "Randomized quantile residual",
     xlab="Fitted values",main="(b)")
abline(h=0,col="red")

hnp(rqr_normalised, print=T,sim = 1000,
    ylab="Randomized quantile residual", 
    main="(a)")

####################################################################################################
######################################## Null model #############################################
####################################################################################################
y_index <- as.numeric(train$Type)
n <- length(y_index)
y <- matrix(rep(0, length(y_index) * 3), length(y_index), 3)
for(i in 1:n){
  y[i,][y_index[i]] <- 1
}


rqr <- get_randomized_quantile_residuals(y_index = y_index, 
                                         y = y,
                                         model_predictions = multinom_fit0_prob, 
                                         y_vector = c(1, 1, 1))

rqr_normalised <- (rqr-mean(rqr))/sd(rqr)

mean_x<-mean(rqr_normalised); sd_x<-sd(rqr_normalised)
par(mfrow=c(1,2))

# hist(rqr_normalised, freq = FALSE, main = "",xlab = "Randomized quantile residuals",  col = c("lightblue"))
# curve(dnorm(x, mean = mean_x, sd = sd_x), add = TRUE, col = "red",lwd = 2)

kurtosis(rqr_normalised);skewness(rqr_normalised)

#QQplot
#qqnorm(rqr_normalised);abline(a=0,b=1)

#Shapiro-Wilk test
shapiro.test(rqr_normalised)$p.value

# hnp and Residual vs fitted values -----
value_fit <- predict(multinom_fit0, type = "class")

ym<-matrix(NA,178,3)

for (i in 1:n) {
  if(value_fit[i]==1){
    ym[i,] <-c(1,0,0)
    
  }else if(value_fit[i]==2){
    ym[i,] <-c(0,1,0)
    
  }else{
    ym[i,] <-c(0,0,1)}}

x <- ym * multinom_fit0_prob
xf <- t(x[1:n,])[which(t(x[1:n,])>0)]


# plot(xf,rqr_normalised,
#      ylab = "Randomized quantile residual",
#      xlab="Fitted values", main="(b)")
# abline(h=0,col="red")

hnp(rqr_normalised, print=T,sim = 1000,
    ylab="Randomized quantile residual", 
    main="(b)")
