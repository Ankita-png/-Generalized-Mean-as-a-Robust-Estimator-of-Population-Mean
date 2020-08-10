rm(list = ls())
col_pallate = c("red","orange","blue","green","maroon")
######### Snapper Length ###########
library(FSAdata)
data = Snapper
d1 = c(data$len)
mu_start_robust=median(d1)
w=abs(d1-mu_start_robust)
sigma_start_robust=median(w)/0.6745
thetastart=c(mu_start_robust,sigma_start_robust)
x=seq(2.5,13,0.001)
hist(d1,prob=T,main = "Densities fitted to Snappers Data by method of Basu et. al.",xlab = "Snapper Length(in inches)",col = "grey")
# MLE of mean and sd of normal dist is known(for alpha=0)
mle_mean = mean(d1)
mle_sd = sqrt(mean((d1-mean(d1))^2))
curve(dnorm(x,mle_mean,mle_sd),lty=1,lwd=2,col=col_pallate[1],add = T)
for(i in 2:5){
  a = c(0,0.25,0.5,0.75,1)
  F=function(theta)
  {
    alpha = a[i]
    n=length(d1)
    (1/sqrt(1+alpha)-((1/n)*(1+(1/alpha)))*sum(exp(-alpha*(d1-theta[1])^2/(2*(theta[2])^2))))/(theta[2])^alpha
  }
  library(nleqslv)
  theta_hat <- optim(thetastart, F, method = "BFGS")$par
  curve(dnorm(x,theta_hat[1],theta_hat[2]),lwd=2,lty=i,col = col_pallate[i],add=TRUE)
}
expr = vector("expression",length = length(a))
for(i in 1:length(a)){
  expr[[i]] = bquote(alpha==.(a[i]))
}
legend('topright',legend = expr,col = col_pallate,lwd=2,lty = 1:5,cex=1,bty = "n",seg.len = 1.5)


## Finding optimal alpha ##
cp_0 = (sd(d1)/mean(d1))        # Coefficient of Variance
cp_0
# Since 2nd highest peak is right of highest peak so alpha lies b/w (lower_range,1)
lower_range <- ((1-sqrt(1+(8/cp_0^2)))/2)
lower_range
opt_alpha <- ((1+lower_range)/2)
opt_alpha
# Generalized Mean for Optimal alpha and alpha=1
n = length(d1)
gm_estimate <- (sum(d1^opt_alpha)/n)^(1/opt_alpha)    # Generalized mean for optimal alpha
AM <- sum(d1)/n                        # Generalized mean for alpha=1
hist(d1,prob = T,main = "Densities fitted to Snappers Data by Generalized Mean Method",xlab = "Snapper Length(in inches)",col = "grey")
x = seq(2.5,13,0.0001)
curve(dnorm(x,AM,sd(d1)),lty=1,lwd=2,col=col_pallate[1],add = T)
curve(dnorm(x,gm_estimate,sd(d1)),lty=2,lwd=2,col=col_pallate[2],add=T)
legend('topright',legend = c(expression(alpha~"=1"),bquote(alpha==.(opt_alpha))),lwd=2,col = col_pallate[1:2],bty='n',cex=1,lty=c(1,2),seg.len = 1.5)
