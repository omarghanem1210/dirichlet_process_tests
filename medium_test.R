library(dirichletprocess)
library(ggplot2)
library(dplyr)
options(scipen = 999)

lognormal_mixture <- function(mixing_weights, parameters, num_points){
  #Samples from a mixture of log normal distributions given the mixing wights and parameters
  #Number of clusters = length(mixing_weights) = length(parameters)
  #num_points is the number of points to be sampled
  #parameters is a list of vectors where each vectors contains the mean and standard deviation
  
  stopifnot(all(mixing_weights >=0),
                sum(mixing_weights) == 1, 
                length(mixing_weights) == length(parameters))
  values <- c()
  
  for (n in 1:num_points) {
    sampled_parameters <- sample(parameters, size = 1, prob = mixing_weights)
    sampled_parameters <- unlist(sampled_parameters)
    mean <- sampled_parameters[1]
    sd <- sampled_parameters[2]
    values <- append(values, rlnorm(1, meanlog = mean, sdlog = sd))
  }
  
  return(values)
}

fit_exponential_process <- function(y, alpha_prior){
  model <- DirichletProcessExponential(y, alphaPriors = alpha_prior)
  model <- Fit(model, 100, progressBar = TRUE)
  return(model$numberClusters)
}


explore_prior <- function(n, min, max, y){
  a_1 <- runif(n, min, max)
  a_2 <- runif(n, min, max)
  num_clusters <- c()
  
  for (i in 1:n) {
    alpha_prior <- c(a_1[i], a_2[i])
    num_clusters <- append(num_clusters, fit_exponential_process(y, alpha_prior))
  }
  
  return(cbind(a_1, a_2, num_clusters))
  
}



mixing_weights <- c(0.5, 0.2, 0.3) 
parameters <- list(c(0, 1), c(2, 3), c(1.5, 0.5))

y <- lognormal_mixture(mixing_weights, parameters, 10000)

model <- DirichletProcessExponential(y)
model$numberClusters
model <- Fit(model, 100, progressBar = TRUE)

# I really should increase the number of iterations. But fitting takes too long
prior_data <- explore_prior(10, 1, 20, y) 
hist(prior_data[, 3])

#Positive and negative correlations for the different prior components show that these statistics aren't to be trusted.
# Since there should be no difference between the updating of the two components
# Unless I am seriously misunderstanding something.
cor(prior_data[, 3], prior_data[, 2])
cor(prior_data[, 3], prior_data[, 1])
