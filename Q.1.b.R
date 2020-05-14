Data <- read.csv("C:/Users/john/Desktop/MSc Data Science coursework/Semester 2/APPLIED STATISTICAL MODELLING/Assignments/Main Assignment/wine-reviews/wine-data-merged.csv")

#Data <- factor(Data$region_1)

nlevels(Data$region_1)

Data_IT <- na.omit(droplevels(subset(Data, (Data$country == 'Italy' & Data$price < 20) , select = c(points,region_1))))

region_wise_count <- aggregate(Data_IT$points , by = list(Data_IT$region_1) , FUN = length)

regions_big <- subset(region_wise_count , region_wise_count$x>3 , select = 'Group.1')

Data_IT_new <- droplevels(subset(Data_IT, Data_IT$region_1 %in% as.vector(regions_big$Group.1)))

nlevels(Data_IT_new$region_1)

ggplot(Data_IT_new) + geom_boxplot(aes(x = reorder(region_1, points, median), points, fill = reorder(region_1, points, median)), show.legend=FALSE)

ggplot(Data_IT_new, aes(x = reorder(region_1, region_1, length))) + stat_count()

ggplot(Data_IT_new, aes(points)) + stat_bin()

ggplot(data.frame(size = tapply(Data_IT_new$points, Data_IT_new$region_1, length), 
                  mean_score = tapply(Data_IT_new$points, Data_IT_new$region_1, mean)), 
                  aes(size, mean_score)) + 
geom_point() + xlab("region sample size") + ylab("mean points") + ggtitle("Effect size versus sample size")

compare_m_gibbs <- function(y, ind, mu0 = 50, tau0 = 1/400, a0 = 1, b0 = 1/400, alpha0 = 1, beta0 = 50, maxiter = 5000) 
{
  ### weakly informative priors 
  a0 <- 1/2 ; b0 <- 50 ## tau_w hyperparameters 
  alpha0 <- 1/2 ; beta0 <- 50 ## tau_b hyperparameters 
  mu0 <- 50 ; tau0 <- 1/25 
  ###
  
  #adding slight noise as variance for some regions' points is 0 
  y <- y + rnorm(length(y),1,1)/10000
  
  ### starting values 
  m <- nlevels(ind) 
  ybar <- theta <- tapply(y, ind, mean) 
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision 
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision 
  n_m <- tapply(y, ind, length) 
  alphan <- alpha0 + sum(n_m)/2 
  ###
  
  ### setup MCMC 
  theta_mat <- matrix(0, nrow=maxiter, ncol=m) 
  mat_store <- matrix(0, nrow=maxiter, ncol=3) 
  ###
  
  ### MCMC algorithm 
  for(s in 1:maxiter) 
  {
    # sample new values of the thetas 
    for(j in 1:m) 
    { 
      taun <- n_m[j] * tau_w + tau_b 
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun 
      theta[j] <- rnorm(1, thetan, 1/sqrt(taun)) 
    }
    # sample new value of tau_w 
    ss <- 0 
    for(j in 1:m)
    { 
      ss <- ss + sum((y[ind == j] - theta[j])^2) 
    } 
    betan <- beta0 + ss/2 
    tau_w <- rgamma(1, alphan, betan)
    
    # sample a new value of mu 
    taum <- m * tau_b + tau0 
    mum <- (mean(theta) * m * tau_b + mu0 * tau0) / taum 
    mu <- rnorm(1, mum, 1/ sqrt(taum))
    
    # sample a new value of tau_b 
    am <- a0 + m/2 
    bm <- b0 + sum((theta - mu)^2) / 2 
    tau_b <- rgamma(1, am, bm)
    
    # store results 
    theta_mat[s,] <- theta 
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  } 
  colnames(mat_store) <- c("mu", "tau_w", "tau_b") 
  return(list(params = mat_store, theta = theta_mat)) 
}

fit2 <- compare_m_gibbs(Data_IT_new$points, Data_IT_new$region_1)

apply(fit2$params, 2, mean)

apply(fit2$params, 2, sd)

theta_hat <- apply(fit2$theta, 2, mean) ## get basic posterior summary 
names(theta_hat) <- 1:221 ## keep track of different regions
sort(theta_hat, decreasing = TRUE) ## which schools did best and worst?
theta_ci <- apply(fit2$theta, 2, quantile, prob = c(0.025, .975)) ## upper/lower bounds for thetas
df_error <- data.frame(lower = theta_ci[1, ], upper = theta_ci[2, ], mean = theta_hat, region_1 = factor(1:221)) 
ggplot(df_error, aes(x = reorder(region_1, mean), mean)) + geom_errorbar(aes(ymin = lower, ymax = upper))
## reformat samples for ggplot 
theta_df <- data.frame(samples = as.numeric(fit2$theta), region_1 = rep(1:ncol(fit2$theta), each = nrow(fit2$theta)))
ggplot(theta_df) + geom_boxplot(aes(x = reorder(region_1, samples, median), samples, fill = reorder(region_1, samples, median)), show.legend=FALSE)
ggplot(data.frame(size = tapply(Data_IT_new$points, Data_IT_new$region_1, length), theta_hat = theta_hat), aes(size, theta_hat)) + geom_point()
ggplot(data.frame(ybar = tapply(Data_IT_new$points, Data_IT_new$region_1, mean), theta_hat = theta_hat), aes(ybar, theta_hat)) + geom_point()

better_regions <- (theta_hat>86.3197002)

length(which(better_regions))

levels(Data_IT_new$region_1)
