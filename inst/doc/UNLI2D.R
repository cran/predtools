## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(predtools)

## -----------------------------------------------------------------------------
library(mvtnorm)
n <- 1000
mus <- c(-2,3)
sigmas <- c(2,4)
rho <- 0.7
Sigma <- matrix(c(sigmas[1],rho*sqrt(sigmas[1]*sigmas[2]),rho*sqrt(sigmas[1]*sigmas[2]),
                  sigmas[2]),nrow=2)
sim_data <- rmvnorm(n,mean=c(-2,3),sigma = Sigma)

## -----------------------------------------------------------------------------
sample_mus <- apply(sim_data,2,mean)
sample_Sigma <- cov(sim_data)
sample_rho <- sample_Sigma[1,2]/sqrt(sample_Sigma[1,1]*sample_Sigma[2,2])
EVPI <- predtools::mu_max_trunc_bvn(sample_mus[1],
                            sample_mus[2],
                            sqrt(sample_Sigma[1,1]),
                            sqrt(sample_Sigma[2,2]),sample_rho) -
  max(c(0,sample_mus))

