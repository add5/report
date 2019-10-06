library(RSpectra)
library(png)
library(animation)
require(ggplot2)

fz <-function(z, p_alpha, p_lambda, p_Looks, log=FALSE){

  if(log==FALSE) {
    lLz <- p_lambda * p_Looks* z
    return((2*p_lambda*p_Looks/(gamma(p_alpha)*gamma(p_Looks))) *
             (lLz)^((p_alpha+p_Looks)/2-1) *
             besselK(x = 2*sqrt(lLz), nu = p_alpha-p_Looks))
  }
}
urbanpic <- readPNG("E:/Report/ESAR_RGB_Annot.png")
r <- 0.299
g <- 0.587
b <- 0.114
R <- urbanpic[,,1]
G <- urbanpic[,,2]
B <- urbanpic[,,3]
urban <- r*R + g*G + b*B  
a <- hist(urban, breaks = seq(0,1,0.02), freq = F)
c <- matrix(unlist(a))
d <- c[102:151]
f <- rev(d)
inputdata <- data.frame(gray_value=seq(0,0.998,0.02),gray_frequency=f)

ggplot(data=inputdata,aes(x=gray_value,y=as.numeric(gray_frequency)/16))+geom_bar(stat="identity") +
  
  stat_function(fun=fz, geom = "line", size=2, col="black", args = list(p_alpha=12, p_lambda=33, p_Looks=9)) +
  stat_function(fun=fz, geom = "line", size=2, col="red", args = list(p_alpha=3, p_lambda=8, p_Looks=3)) +
  stat_function(fun=fz, geom = "line", size=2, col="blue", args = list(p_alpha=8, p_lambda=26, p_Looks=7)) +
  xlab("Grey Value") + ylab("Gray Histogram and K Densities")

