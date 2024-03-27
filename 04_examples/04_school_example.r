####################################################################################################
###
### File:    04_school_example.r
### Purpose: Code required for the applying the techniques proposed by the paper
###         using the school dataset
###          create interactive maps.
### Authors: Gabriel Rodrigues Palma
### Date:    11/05/23
###
####################################################################################################
# Loading packages -----
source('00_source.r')

# Read dataset -----
set.seed(1401)
ml <- read_dta("input_data/hsbdemo.dta")
glimpse(ml)

####################################################################################################
######################################## New analysis ##############################################
####################################################################################################
# Grouped data
## Creating a new dataset based on reviewers' suggestion
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

updated_school_data <- ml %>%
  group_by(math) %>%
  summarise(female = getmode(female), 
            ses = getmode(ses), 
            schtyp = getmode(schtyp), 
            read = getmode(read), 
            write = getmode(write), 
            science = getmode(science), 
            socst = getmode(socst), 
            honors = getmode(honors), 
            awards = getmode(awards))

school_data <- read.csv("input_data/math.csv", head=TRUE, sep=";", dec=",") 
school_data <- school_data %>%
  mutate(M =  Academic + General + Vocation)
school_data <- school_data %>%
  filter(M > 1)
updated_schoold_data <- inner_join(updated_school_data, school_data, by = 'math')

# Selecting covariates for the model -----
check_lrt<- function(mod_simpler, mod_complex){
  LRT <- 2*(logLik(mod_complex)-logLik(mod_simpler)) # Likelihood ration test value
  dl <- length(coef(mod_complex))-length(coef(mod_simpler)) # Computing degree of freedom
  p <- 1-pchisq(LRT,dl) # P valye based on Chisq distribution
  return(cbind(LRT, dl, p))
  
}

## Creating the models
mod0 <- vglm(cbind(Academic,General,Vocation) ~ 1,
             multinomial(refLevel = 1), data = updated_schoold_data) 

modall <- vglm(cbind(Academic,General,Vocation)~female+ses+ schtyp+
               read+write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod0, mod_complex = modall)

## Selecting covariates based on Likelihood ratio test -----
### Checking covariate sex
mod1 <- vglm(cbind(Academic,General,Vocation)~female+ses+ schtyp+
               read+write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~ses+schtyp+
               read+write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate ses
mod1 <- vglm(cbind(Academic,General,Vocation)~ses+schtyp+
               read+write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~schtyp+
               read+write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate schtyp
mod1 <- vglm(cbind(Academic,General,Vocation)~schtyp+
               read+write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~read+write+science+
               socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate read
mod1 <- vglm(cbind(Academic,General,Vocation)~read+write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~write+science+
               socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate read
mod1 <- vglm(cbind(Academic,General,Vocation)~read+write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~write+science+
               socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate write
mod1 <- vglm(cbind(Academic,General,Vocation)~write+science+socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~science+
               socst+honors+awards+math,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate science
mod1 <- vglm(cbind(Academic,General,Vocation)~science+socst+honors+awards+math +write,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~socst+honors+awards+math+write,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate socst
mod1 <- vglm(cbind(Academic,General,Vocation)~socst+honors+awards+math+write,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~honors+awards+math+write,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate honors
mod1 <- vglm(cbind(Academic,General,Vocation)~honors+awards+math+write,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~awards+math+write,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate awards
mod1 <- vglm(cbind(Academic,General,Vocation)~awards+math+write,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~math+write,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Checking covariate math
mod1 <- vglm(cbind(Academic,General,Vocation)~math+write,
             multinomial(refLevel=1),data = updated_schoold_data)
mod2 <- vglm(cbind(Academic,General,Vocation)~write,
             multinomial(refLevel=1),data = updated_schoold_data)
check_lrt(mod_simpler = mod2, mod_complex = mod1)

### Selecting the final mode
final_mod <- vglm(cbind(Academic,General,Vocation)~math+write,
                  multinomial(refLevel=1),data = updated_schoold_data)

AIC(modall)
AIC(final_mod) # Better model

### Obtaining model statistics
summary(final_mod)
coef(final_mod, matrix = TRUE) # Model coefficients
exp(coefficients(final_mod)) #odds ratio
confint(final_mod) #Confidence Intervals for Parameters
prob_pred<-fitted(final_mod) #estimated probabilities
prob_final<-final_mod@y-prob_pred #observed probabilities - estimated probabilities

##########################################################################################
################################## Accesing godness of fit ###############################
##########################################################################################
n <- nrow(updated_schoold_data)
m <- vector()
for (i in 1:n) {
  m[i]<- sum(updated_schoold_data[i,11:13])
  
}
#Implementing new class for hnp
s_fun <- function(n, obj) {
  pred<-fitted(obj)
  newresp<-matrix(NA,n,3)
  for (i in 1:n) {
    newresp[i,]<- t(rmultinom(1, size = m[i], pred[i,]))
    
  }
  newresp
}
########################################################################################################
#wrong model
null_model <- function(newresp) {
  vglm(cbind(newresp[,1],newresp[,2],newresp[,3]) ~ 1,
       multinomial(refLevel = 1), data = updated_schoold_data) 
}

#correct model
correct_model <- function(newresp) {
  vglm(cbind(newresp[,1],newresp[,2],newresp[,3])~math+write,
       multinomial(refLevel=1),data = updated_schoold_data)
}

#Plots
par(mfrow=c(1,2))

hnp_null_model_eucledian <- hnp(mod0, newclass = TRUE, diagfun = euclidian_distance, 
                                simfun = s_fun, fitfun = null_model, 
                                print =T, sim = 1000,main="(a)",
                                ylab="Values of Euclidian distance")


hnp_correct_model_eucledian <- hnp(mod1, newclass = TRUE, 
                                   diagfun = euclidian_distance,
                                   simfun = s_fun, 
                                   fitfun = correct_model, 
                                   print =T, sim = 1000,main="(b)",
                                   ylab="Values of Euclidian distance")



hnp_null_model_mahalanobis <- hnp(mod0, newclass = TRUE, 
                                  diagfun = mahalanobis_distance,
                                  sim = 1000, simfun = s_fun, 
                                  fitfun = null_model,  
                                  print = T,main="(a)",
                                  ylab="Values of Mahalanobis distance")


hnp_correct_model_mahalanobis <- hnp(mod1, newclass = TRUE, 
                                     diagfun = mahalanobis_distance, 
                                     simfun = s_fun,fitfun = correct_model,
                                     print = T, sim = 1000, main="(b)", 
                                     ylab="Values of Mahalanobis distance")

############
################################
####################################################################################################
######################################## Old analysis ##############################################
####################################################################################################
#Grouped Data
with(ml, table(math, prog)) 
school_data <- read.csv("input_data/math.csv", head=TRUE, sep=";", dec=",") 
school_data <- school_data %>%
  mutate(M =  Academic + General + Vocation)
school_data <- school_data %>%
  filter(M > 1)

attach(school_data)

############################################################################################################
#Histogram
Math<-rep(math,each=3)
Categ<-rep(1:3,times=length(math))
Freq<-as.vector(t(school_data[,2:4]))
dados.plot<-cbind.data.frame(Math,Categ,Freq)

attach(dados.plot)

dados.plot$Categ<-as.factor(dados.plot$Categ)
levels(dados.plot$Categ)<-c("Academic","General", "Vocation")

#Histogram
ggplot(dados.plot, aes(x = Math, y = Freq))+
  geom_col()+
  facet_wrap(~Categ) +
  theme(legend.position = "top",legend.title=element_blank())+
  labs(x= "Math Score", y = "Frequency")

## Creating the models
mod0 <- vglm(cbind(Academic,General,Vocation) ~ 1,
             multinomial(refLevel = 1), data = school_data) 

mod1 <- vglm(cbind(Academic,General,Vocation)~math,
             multinomial(refLevel=1),data = school_data)

### Ratio likelihood test
LRT <- 2*(logLik(mod1)-logLik(mod0)) # Likelihood ration test value
dl <- length(coef(mod1))-length(coef(mod0)) # Computing degree of freedom
p <- 1-pchisq(LRT,dl) # P valye based on Chisq distribution
cbind(LRT, dl, p)

### AIC for each model
AIC(mod0)
AIC(mod1)

### Obtaining model statistics
summary(mod1)
coef(mod1, matrix = TRUE) # Model coefficients
exp(coefficients(mod1)) #odds ratio
confint(mod1) #Confidence Intervals for Parameters
prob_pred<-fitted(mod1) #estimated probabilities
prob_final<-mod1@y-prob_pred #observed probabilities - estimated probabilities

##########################################################################################
################################## Accesing godness of fit ###############################
##########################################################################################
n <- nrow(school_data)
m <- vector()
for (i in 1:n) {
  m[i]<- sum(school_data[i,2:4])
  
}
#Implementing new class for hnp

s_fun <- function(n, obj) {
  pred<-fitted(obj)
  newresp<-matrix(NA,n,3)
  for (i in 1:n) {
    newresp[i,]<- t(rmultinom(1, size = m[i], pred[i,]))
    
  }
  newresp
}


########################################################################################################
#wrong model
null_model <- function(newresp) {
  vglm(cbind(newresp[,1],newresp[,2],newresp[,3]) ~ 1,
       multinomial(refLevel = 1), data = school_data) 
}

#correct model
correct_model <- function(newresp) {
  vglm(cbind(newresp[,1],newresp[,2],newresp[,3])~math,
       multinomial(refLevel=1),data = school_data)
}

#Plots
par(mfrow=c(1,2))

hnp_null_model_eucledian <- hnp(mod0, newclass = TRUE, diagfun = euclidian_distance, 
                      simfun = s_fun, fitfun = null_model, 
                      print =T, sim = 1000,main="(a)",
                      ylab="values of Euclidian Distance")


hnp_correct_model_eucledian <- hnp(mod1, newclass = TRUE, 
                                   diagfun = euclidian_distance,
                                   simfun = s_fun, 
                                   fitfun = correct_model, 
                                   print =T, sim = 1000,main="(b)",
                                   ylab="Values of Euclidian Distance")



hnp_null_model_mahalanobis <- hnp(mod0, newclass = TRUE, 
                                  diagfun = mahalanobis_distance,
                                  sim = 1000, simfun = s_fun, 
                                  fitfun = null_model,  
                                  print =T,main="(a)",
                                  ylab="values of Mahalanobis distance")


hnp_correct_model_mahalanobis <- hnp(mod1, newclass = TRUE, 
                                     diagfun = mahalanobis_distance, 
                                     simfun = s_fun,fitfun = correct_model,
                                     print = T,sim = 1000, main="(b)", 
                                     ylab="Values of Mahalanobis distance")
