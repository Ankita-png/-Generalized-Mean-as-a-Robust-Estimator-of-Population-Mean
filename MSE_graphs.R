rm(list=ls())
#====================== Function for reading excel data =====================
setwd(".../MSE_File")
files = list.files(pattern = ".xlsx")
library(readxl)
f = function(ipath,isheet,irange){
df = read_xlsx(path = ipath, sheet = isheet, range = irange)
df = as.data.frame(df)
rownames(df) = estimators
return(df)
}

#=========(1)================ Sample Size vs MSE (different pi) =================
sample_size <- c("50","100","500")
pi <- c("1.0","0.9","0.8","0.7","0.6")
estimators <- c("Trimmed Mean(10%)","Winsorized Mean","Median","M_alpha-Optimal alpha","M_alpha-alpha=1",
                "BHH_sd-0.25","BHH_sdcap-0.25","BHH_sd-0.5","BHH_sdcap-0.5","BHH_sd-0.75","BHH_sdcap-0.75")
x_axis <- c(50,100,500)
col_pallate = c("red","orange","blue","green","cyan","maroon","magenta","grey","pink","yellow","purple")

f_plot = function(df,heading,ylim_max){
  plot(x_axis,df[1,],main = heading,
       xlab = "Sample Size", ylab = "MSE",ylim = c(0,ylim_max),
       col = col_pallate[1],type = 'b', pch = 16, lty = 1, lwd = 2)
  for(i in 2:11){
    points(x_axis,df[i,],col = col_pallate[i],pch = 16)
    lines(x_axis,df[i,],col = col_pallate[i], lty = 1, lwd = 2)
  }
}
#========(1.1)========== Pi - actualmean ==========================================
df_1.0 = f(ipath = files[1], isheet = "1.0", irange = "C3:E14")
df_0.9 = f(ipath = files[1], isheet = "0.9", irange = "C3:E14")
df_0.8 = f(ipath = files[1], isheet = "0.8", irange = "C3:E14")
df_0.7 = f(ipath = files[1], isheet = "0.7", irange = "C3:E14")
df_0.6 = f(ipath = files[1], isheet = "0.6", irange = "C3:E14")

l_mat = matrix(c(1,2,3,4,5,6), nrow = 2, byrow =TRUE)
layout(l_mat)

df_list = list(df_1.0,df_0.9,df_0.8,df_0.7,df_0.6)

for(i in 1:5){
  heading_main = bquote("For "~pi==.(pi[i]))
  y_max = max(df_list[[i]])
  f_plot(df = df_list[[i]], heading = heading_main,ylim_max = y_max)
}

par(mar = c(0,0,0,0))
plot(1, type = "n", axes = FALSE, bty = "n", ylab = '')
legend('left',legend = c("Trimmed Mean(10%)","Winsorized Mean","Median",
                         expression("M"[alpha]~"-Optimal"~alpha),expression("M"[alpha]~"-"~alpha~"=1"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.25"),expression("BHH-"~sigma~"unknown"~alpha~"=0.25"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.5"),expression("BHH-"~sigma~"unknown"~alpha~"=0.5"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.75"),expression("BHH-"~sigma~"unknown"~alpha~"=0.75")),
       title = "Estimators(actual mean)",col = col_pallate,lty = 1, lwd = 2, pch = 16,horiz = F, cex = 1.3)
#=========(1.2)========= Pi - pilotmean ==========================================
graphics.off()
df_1.0 = f(ipath = files[2], isheet = "1.0", irange = "C3:E14")
df_0.9 = f(ipath = files[2], isheet = "0.9", irange = "C3:E14")
df_0.8 = f(ipath = files[2], isheet = "0.8", irange = "C3:E14")
df_0.7 = f(ipath = files[2], isheet = "0.7", irange = "C3:E14")
df_0.6 = f(ipath = files[2], isheet = "0.6", irange = "C3:E14")

l_mat = matrix(c(1,2,3,4,5,6), nrow = 2, byrow =TRUE)
layout(l_mat)

df_list = list(df_1.0,df_0.9,df_0.8,df_0.7,df_0.6)

for(i in 1:5){
  heading_main = bquote("For "~pi==.(pi[i]))
  y_max = max(df_list[[i]])
  f_plot(df = df_list[[i]], heading = heading_main,ylim_max = y_max)
}

par(mar = c(0,0,0,0))
plot(1, type = "n", axes = FALSE, bty = "n", ylab = '')
legend('left',legend = c("Trimmed Mean(10%)","Winsorized Mean","Median",
                         expression("M"[alpha]~"-Optimal"~alpha),expression("M"[alpha]~"-"~alpha~"=1"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.25"),expression("BHH-"~sigma~"unknown"~alpha~"=0.25"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.5"),expression("BHH-"~sigma~"unknown"~alpha~"=0.5"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.75"),expression("BHH-"~sigma~"unknown"~alpha~"=0.75")),
       title = "Estimators(pilot mean)",col = col_pallate,lty = 1, lwd = 2, pch = 16,horiz = F, cex = 1.3)

#===========(2)============== pi vs MSE (different sample size) =================
sample_size <- c("50","100","500")
estimators <- c("Trimmed Mean(10%)","Winsorized Mean","Median","M_alpha-Optimal alpha","M_alpha-alpha=1",
                "BHH_sd-0.25","BHH_sdcap-0.25","BHH_sd-0.5","BHH_sdcap-0.5","BHH_sd-0.75","BHH_sdcap-0.75")
x_axis <- c(1.0,0.9,0.8,0.7,0.6)
col_pallate = c("red","orange","blue","green","cyan","maroon","magenta","grey","pink","yellow","purple")

f_plot = function(df,heading,ylim_max){
  plot(x_axis,df[1,],main = heading,
       xlab = expression(pi), ylab = "MSE",ylim = c(0,ylim_max),
       col = col_pallate[1],type = 'b', pch = 16, lty = 1, lwd = 2)
  for(i in 2:11){
    points(x_axis,df[i,],col = col_pallate[i],pch = 16)
  lines(x_axis,df[i,],col = col_pallate[i], lty = 1, lwd = 2)
  }
}

#==========(2.1)======== Sample_size - actual mean ==========================================
graphics.off()
df_50  = f(ipath = files[3], isheet = "50", irange = "C3:G14")
df_100 = f(ipath = files[3], isheet = "100", irange = "C3:G14")
df_500 = f(ipath = files[3], isheet = "500", irange = "C3:G14")

l_mat = matrix(c(1,2,3,4), nrow = 2, byrow =TRUE)
layout(l_mat)

df_list = list(df_50,df_100,df_500)

for(i in 1:3){
  heading_main = paste("For Sample Size:",sample_size[i])
  y_max = max(df_list[[i]])
  f_plot(df = df_list[[i]], heading = heading_main,ylim_max = y_max)
}

par(mar = c(0,0,0,0))
plot(1, type = "n", axes = FALSE, bty = "n", ylab = '')
legend('left',legend = c("Trimmed Mean(10%)","Winsorized Mean","Median",
                         expression("M"[alpha]~"-Optimal"~alpha),expression("M"[alpha]~"-"~alpha~"=1"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.25"),expression("BHH-"~sigma~"unknown"~alpha~"=0.25"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.5"),expression("BHH-"~sigma~"unknown"~alpha~"=0.5"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.75"),expression("BHH-"~sigma~"unknown"~alpha~"=0.75")),
       title = "Estimators(actual mean)",col = col_pallate,lty = 1, lwd = 2, pch = 16,horiz = F, cex = 1)
#=========(2.2)========= Sample_size - pilot mean ==========================================
graphics.off()
df_50  = f(ipath = files[4], isheet = "50",  irange = "C3:G14")
df_100 = f(ipath = files[4], isheet = "100", irange = "C3:G14")
df_500 = f(ipath = files[4], isheet = "500", irange = "C3:G14")

l_mat = matrix(c(1,2,3,4), nrow = 2, byrow =TRUE)
layout(l_mat)

df_list = list(df_50,df_100,df_500)
for(i in 1:3){
  heading_main = paste("For Sample Size:",sample_size[i])
  y_max = max(df_list[[i]])
  f_plot(df = df_list[[i]], heading = heading_main,ylim_max = y_max)
}

par(mar = c(0,0,0,0))
plot(1, type = "n", axes = FALSE, bty = "n", ylab = '')
legend('left',legend = c("Trimmed Mean(10%)","Winsorized Mean","Median",
                         expression("M"[alpha]~"-Optimal"~alpha),expression("M"[alpha]~"-"~alpha~"=1"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.25"),expression("BHH-"~sigma~"unknown"~alpha~"=0.25"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.5"),expression("BHH-"~sigma~"unknown"~alpha~"=0.5"),
                         expression("BHH-"~sigma~"known"~alpha~"=0.75"),expression("BHH-"~sigma~"unknown"~alpha~"=0.75")),
       title = "Estimators(pilot mean)",col = col_pallate,lty = 1, lwd = 2, pch = 16,horiz = F, cex = 1)
