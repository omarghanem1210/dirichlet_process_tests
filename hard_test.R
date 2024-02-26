library(dirichletprocess)
library(ggplot2)
options(scipen = 999)

Likelihood.cauchy <- function(mdobj, x, theta){
  return(as.numeric(dcauchy(x, theta[[1]], theta[[2]])))
}

PriorDraw.cauchy <- function(mdobj, n=1){
  theta <- list()
  theta[[1]] = array(rexp(n, mdobj$priorParameters[1]), dim=c(1,1, n))
  theta[[2]] = array(rexp(n, mdobj$priorParameters[2]), dim=c(1,1, n))
  return(theta)
}

PriorDensity.cauchy <- function(mdobj, theta){
  priorParameters <- mdobj$priorParameters
  thetaDensity <- dexp(theta[[1]], priorParameters[1])
  thetaDensity <- thetaDensity * dexp(theta[[2]], priorParameters[2])
  return(as.numeric(thetaDensity))
}

MhParameterProposal.cauchy <- function(mdobj, oldParams){
  mhStepSize <- mdobj$mhStepSize
  newParams <- oldParams
  newParams[[1]] <- oldParams[[1]] + mhStepSize[1]*rnorm(1)
  newParams[[2]] <- abs(oldParams[[2]] + mhStepSize[2]*rnorm(1))
  return(newParams)
}

cauchyMd <- MixingDistribution(distribution = "cauchy",
                               priorParameters = c(0.1, 0.1),
                               conjugate = "nonconjugate",
                               mhStepSize = c(0.1, 0.1))


y <- c(rcauchy(100, 1, 2), rcauchy(100, 10, 1))

dp <- DirichletProcessCreate(y, cauchyMd)
dp <- Initialise(dp)
dp <- Fit(dp, 1000)
pf <- PosteriorFrame(dp, ppoints(100)*1000, 1000)
trueFrame <- data.frame(x=ppoints(100)*1000,
                        y= 0.5*dcauchy(ppoints(100)*1000, 1, 2) +
                          + 0.5*dcauchy(ppoints(100)*1000, 10, 1))

ggplot() +
  geom_ribbon(data=pf,
              aes(x=x,ymin=X5.,ymax=X95.),
              colour=NA, fill="red", alpha=0.2) +
  geom_line(data=pf, aes(x=x, y=Mean), colour="red") +
  geom_line(data=trueFrame, aes(x=x, y=y))
