library(dirichletprocess)
library(ggplot2)
library(dplyr)

# Fit normal mixture model to faithful dataset
data("faithful")

faithful.transformed <- (faithful$waiting - mean(faithful$waiting)) / sd(faithful$waiting)
estimated.distribution <- DirichletProcessGaussian(faithful.transformed)
estimated.distribution <- Fit(estimated.distribution, 100, progressBar = TRUE)
plot(estimated.distribution)


# Fit multivariate normal model to iris dataset
data("iris")
iris
ggplot(data = iris, mapping = aes(x=Sepal.Width, y=Petal.Width, colour=Species)) + geom_point()

iris %>% 
  select(Sepal.Width, Petal.Width) %>% 
  scale -> train_data

dp <- DirichletProcessMvnormal(train_data)
dp <- Fit(dp, 2500)
plot(dp)
