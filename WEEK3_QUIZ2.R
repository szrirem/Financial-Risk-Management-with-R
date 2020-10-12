library(quantmod)
library(moments)
getSymbols("DEXUSUK",src="FRED")
wilsh <- na.omit(DEXUSUK)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
logret <- diff(log(wilsh))[-1]
rvec <- as.vector(logret)
round(skewness(rvec),2)
rvec <- as.vector(logret)
round(kurtosis(rvec),2)
rvec <- as.vector(logret)
jarque.test(rvec)
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6)
alpha <- 0.01
set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
alpha <- 0.01
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 