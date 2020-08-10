rm(list=ls())

# Simulating data from Normal Mixture Distribution of sample size 50 with 1000 replicates
k <- 1000
n <- 50         # value can be changed
pi <- 0.8       # value can be changed
x <- matrix(0,nrow = k,ncol = n)
u <- NULL
for(j in 1:k)
{
  u <- runif(n = n,min = 0,max = 1)
  for(i in 1:n)
  {
    if(pi>=u[i])
      x[j,i] <- rnorm(n = 1,mean = 10,sd = 1)
    else
      x[j,i] <- rnorm(n = 1,mean = 20,sd = 1)
  }
}
#View(x)
pilot_mean <- median(x)

## Trimmed Mean ##
trim_estimate <- NULL
for(j in 1:k)
{
  trim_estimate[j] <- mean(x[j,],trim = 0.10)
}
#View(trim_estimate)
var_trim <- var(trim_estimate)
avg_trim <- mean(trim_estimate)

mse_trim_actual <- var_trim+((avg_trim-10)^2)
mse_trim_actual
mse_trim_pilot <- var_trim+((avg_trim-pilot_mean)^2)
mse_trim_pilot

## Winsorized Mean ##
wm_estimate <- NULL
library(psych)
for(j in 1:k)
{
  wm_estimate[j] <- winsor.mean(x[j,],trim = 0.10)
}
#View(wm_estimate)
var_wm <- var(wm_estimate)
avg_wm <- mean(wm_estimate)

mse_wm_actual <- var_wm+((avg_wm-10)^2)
mse_wm_actual
mse_wm_pilot <- var_wm+((avg_wm-pilot_mean)^2)
mse_wm_pilot

## Median ##
median_estimate <- NULL
for(j in 1:k)
{
  median_estimate[j] <- median(x[j,])
}
#View(median_estimate)
var_median <- var(median_estimate)
avg_median <- mean(median_estimate)

mse_median_actual <- var_median+((avg_median-10)^2)
mse_median_actual
mse_median_pilot <- var_median+((avg_median-pilot_mean)^2)
mse_median_pilot

## Finding optimal alpha ##
cp_0 <- lower_range <- opt_alpha <- NULL
for(j in 1:k)
{
  cp_0[j] <- (sd(x[j,])/mean(x[j,]))        # Coefficient of Variance
  # Since mu2>mu1 so optimal alpha belongs to range (lower_range,1)
  lower_range[j] <- ((1-sqrt(1+(8/cp_0[j]^2)))/2)
  opt_alpha[j] <- ((lower_range[j]+1)/2)
}
#View(cp_0)
#View(lower_range)
#View(opt_alpha)

# Generalized Mean for Optimal alpha and alpha=1
gm_estimate <- gm_estimate_1 <- NULL
a <- 1
for(j in 1:k)
{
  gm_estimate[j] <- (sum(x[j,]^opt_alpha[j])/n)^(1/opt_alpha[j])    # Generalized mean for optimal alpha
  gm_estimate_1[j] <- (sum(x[j,]^a)/n)^(1/a)                        # Generalized mean for alpha=1
}
#View(gm_estimate)
var_gm <- var(gm_estimate)
avg_gm <- mean(gm_estimate)

# MSE of generalized mean for optimal alpha
mse_gm_actual <- var_gm+((avg_gm-10)^2)
mse_gm_actual          
mse_gm_pilot <- var_gm+((avg_gm-pilot_mean)^2)
mse_gm_pilot          

#View(gm_estimate_1)
var_gm_1 <- var(gm_estimate_1)
avg_gm_1 <- mean(gm_estimate_1)

# MSE of generalized mean for alpha=1
mse_gm_1_actual <- var_gm_1+((avg_gm_1-10)^2)
mse_gm_1_actual        
mse_gm_1_pilot <- var_gm_1+((avg_gm_1-pilot_mean)^2)
mse_gm_1_pilot        

## BHH with known sigma##
library(nleqslv)
dpd_actualsd = NULL
for(i in 1:k){
  fun1 = function(theta){
    assign("z", x[i,], envir = .GlobalEnv)
    n = length(z)
    f1 = numeric(1)
    alpha = 0.5
    g = rep(0,n)
    for(j in 1:n){
      g[j] = (1/sqrt(1+alpha))-((1/n)*(1+(1/alpha)))*sum(exp(-alpha*(z[j]-theta)^2/2))
    }
    f1 = sum(g)
    f1
  }
  median_x = median(x[i,])
  thetastart1 = c(median_x)
  A1 = nleqslv(thetastart1, fun1, control = list(btol = 0.01))
  theta_hat1 = A1$x
  dpd_actualsd[i] = theta_hat1
}
#dpd_actualsd
var_dpd_1 <- var(dpd_actualsd)
avg_dpd_1 <- mean(dpd_actualsd)

mse_dpd_actual_1 <- var_dpd_1+((avg_dpd_1-10)^2)
mse_dpd_actual_1
mse_dpd_pilot_1 <- var_dpd_1+((avg_dpd_1-pilot_mean)^2)
mse_dpd_pilot_1

## BHH with unknown sigma##
library(nleqslv)
dpd_pilotsd = NULL
for(i in 1:k){
  fun = function(theta){
    assign("z", x[i,], envir = .GlobalEnv)
    n = length(z)
    f = numeric(2)
    alpha = 0.5
    g1 = rep(0,n)
    g2 = rep(0,n)
    for(j in 1:n){
      g1[j] = (z[j]-theta[1])*exp(-alpha*(z[j]-theta[1])^2 / (2*theta[2]^2))
      g2[j] = ((z[j]-theta[1])^2 - theta[2]^2)*exp(-alpha*(z[j]-theta[1])^2 / (2*theta[2]^2))
    }
    f[1] = sum(g1)
    f[2] = sum(g2)
    f
  }
  median_x = median(x[i,])
  a = abs(x[i,] - median_x)
  pilot_sd = median(a)/0.6745
  thetastart = c(median_x, pilot_sd)
  A = nleqslv(thetastart, fun, control = list(btol = 0.01))
  theta_hat = A$x
  dpd_pilotsd[i] = theta_hat[1]
}
#dpd_pilotsd

var_dpd <- var(dpd_pilotsd)
avg_dpd <- mean(dpd_pilotsd)

mse_dpd_actual <- var_dpd+((avg_dpd-10)^2)
mse_dpd_actual
mse_dpd_pilot <- var_dpd+((avg_dpd-pilot_mean)^2)
mse_dpd_pilot