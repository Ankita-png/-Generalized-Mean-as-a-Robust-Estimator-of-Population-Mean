############# INFLUENCE CURVE FOR GENERALIZED MEAN ###################
rm(list=ls())

a <- c(-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,5,10,15,20,25,30)
x <- seq(0,40,0.00001)
exp_x <- col_pallate <- NULL
influence_func <- matrix(0,nrow = length(a),ncol = length(x))
for(i in 1:length(a)){
  f = function(x){(x^(a[i])/sqrt(2*0.314))*exp(-(x-10)^2/2)}
  exp_x[i] = integrate(f,lower = 0,upper = 40)$value
}
exp_x
for(i in 1:length(a)){
  for(j in 1:length(x)){
    influence_func[i,j] = (1/a[i])*(x[j]^a[i]*exp_x[i]^((1-a[i])/a[i])-exp_x[i]^(1/a[i]))
  }
}
influence_func
for (i in 1:8) {
  col_pallate[i] = "red"
}
for(i in 9:length(a)){
  col_pallate[i] = "blue"
}
l_mat = matrix(c(1,2), nrow = 1, byrow =TRUE)
layout(l_mat)
plot(x,influence_func[1,],type="l",ylim = c(-20,20),main="Influence Function of Generalized Mean:N(10,1)",xlab = "x",ylab = "Influence Function",lty=1,lwd=2,col = col_pallate[1])
for(i in 2:length(a)){
  lines(x,influence_func[i,],lty = i,lwd=2,col=col_pallate[i])
}
expr = vector("expression",length = length(a))
for(i in 1:length(a)){
  expr[[i]] = bquote(alpha==.(a[i]))
}
par(mar = c(0,0,0,0))
plot(1, type = "n", axes = FALSE, bty = "n", ylab = '')
legend('left',legend = expr,col = col_pallate,lty = seq(1,length(a),1),lwd = 2,cex=0.8)