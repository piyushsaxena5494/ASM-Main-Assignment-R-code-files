#Q1.a.i.
#-------

library(dplyr)

Data <- read.csv("C:/Users/john/Desktop/MSc Data Science coursework/Semester 2/APPLIED STATISTICAL MODELLING/Assignments/Main Assignment/wine-reviews/wine-data-merged.csv")
Data_CH <- filter(Data, price == '15' & country == 'Chile' & variety == 'Chardonnay')
Data_SB <- filter(Data, price == '15' & country == 'South Africa' & variety == 'Sauvignon Blanc')
Data_combined <- rbind(Data_SB,Data_CH)
library(ggplot2) 

ggplot(Data_combined) + 
geom_boxplot(aes(Data_combined$variety, Data_combined$points, fill = Data_combined$variety), 
             fill=c('powderblue','mistyrose')) + 
geom_jitter(aes(Data_combined$variety, Data_combined$points, shape = Data_combined$variety))                                     
                                     
Data_combined <- within(Data_combined, variety <- factor(variety, labels = c(1, 2)))

tapply(Data_combined$points, Data_combined$variety, mean)
tapply(Data_combined$points, Data_combined$variety, median)
tapply(Data_combined$points, Data_combined$variety, sd)

t.test(Data_combined$points ~ Data_combined$variety, data=Data_combined, var.equal = TRUE)

library(MCMCpack)

compare_2_gibbs <- function(y, ind, mu0 = 50, tau0 = 1/400, del0 = 0, gamma0 = 1/400, a0 = 1, b0 = 1/400, maxiter = 5000) 
{ 
  y1 <- y[ind == 1] 
  y2 <- y[ind == 2]
  
  n1 <- length(y1) 
  n2 <- length(y2)
  
  ##### starting values 
  mu <- (mean(y1) + mean(y2)) / 2 
  del <- (mean(y1) - mean(y2)) / 2
  
  mat_store <- matrix(0, nrow = maxiter, ncol = 3) 
  #####
  
  ##### Gibbs sampler 
  an <- a0 + (n1 + n2)/2

  for(s in 1 : maxiter) 
  {
    ##update tau 
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2)) 
    tau <- rgamma(1, an, bn) 
    ##
  
    ##update mu 
    taun <- tau0 + tau * (n1 + n2) 
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun 
    mu <- rnorm(1, mun, sqrt(1/taun)) 
    ##
    
    ##update del 
    gamman <- tau0 + tau*(n1 + n2) 
    deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman 
    del<-rnorm(1, deln, sqrt(1/gamman)) 
    ##
    
    ## store parameter values 
    mat_store[s, ] <- c(mu, del, tau)
  } 
  colnames(mat_store) <- c("mu", "del", "tau") 
  return(mat_store) 
}

fit <- compare_2_gibbs(Data_combined$points, as.factor(Data_combined$variety))
plot(as.mcmc(fit))
raftery.diag(as.mcmc(fit))
apply(fit, 2, mean)
apply(fit, 2, sd)
mean(1/sqrt(fit[, 3]))
sd(1/sqrt(fit[, 3]))


#Q1.a.ii.
#--------
y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3])) 
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))
ggplot(data.frame(y_sim_diff = y1_sim - y2_sim)) + stat_bin(aes(y_sim_diff))

mean(y1_sim > y2_sim)

ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)


