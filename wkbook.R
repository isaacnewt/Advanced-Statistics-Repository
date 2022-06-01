arr <- seq(2,8,0.1)

dprob <- function(z){
  f <- (0.3*exp (-0.5*z) + 0.6* exp (-0.25*z))
  }
ans <- integrate(dprob, 2, 4)
print(ans)
hist(dprob(arr), ylim = c(0,6), xlim = c(0,0.6), prob = TRUE)
print(paste("The quantiles of the system reliability is:", quantile(dprob(x))) )
print(paste("mean of the system reliability is:", mean(dprob(x))))
print(paste("variance of the system reliability is:", var(dprob(x))))

abline(v=mean(dprob(x)), col="red")
abline(v=var(dprob(x)), col="blue")
abline(v=quantile(dprob(x)), col="green")
legend("topright", # Add legend to density
       legend = c("mean", "Variance", "Quantile"),
       col = c("red", "blue", "green"),
       lty = 1)
# Histogram and density
#lines(dprob(arr), col = "red")
box()

# d <- density(numeric(ans))==============================================
par(mfrow=c(1,1))

x= 0:20
plot(dprob(x),x)
abline(v=mean(dprob(x)), col="red")
lines(mean(dprob(x)), col ="green")

means <- aggregate(dprob(x), FUN = mean)
abline(v = means[1,2], col = 2)
abline(v = means[2,2], col = 3, lty = 2)
abline(v = means[3,2], col = 4, lty = 3)
# ========================================================================

hist(dprob(x), ylim = c(0,30), freq = FALSE, breaks = 30)
print(paste("The quantile for 0, 0.25, 0.5, 0.75, 1 of the system reliability is:", quantile(dprob(x))) )
print(paste("mean of the system reliability is:", mean(dprob(x))) )
print(paste("median of the system reliability is:", median(dprob(x))) )
print(paste("variance of the system reliability is:", var(dprob(x))) )
box()
# abline(v=quantile(dprob(x)))
abline(v=mean(dprob(x)), col="red")
abline(v=median(dprob(x)), col="blue")
# lines(mean(dprob(x), col ="green"))
legend("topright", # Add legend to density
       legend = c("mean", "Variance", "Quartile"),
       col = c("black", "red", "green"),
       lty = 1)

# ========================================================================
print(paste("The quantile for 0, 0.25, 0.5, 0.75, 1 of the system reliability is:", quantile(dprob(x))) )
print(paste("mean of the system reliability is:", mean(dprob(x))) )
print(paste("median of the system reliability is:", median(dprob(x))) )
print(paste("variance of the system reliability is:", var(dprob(x))) )
box()
par(mfrow=c(1,1))
dev.off
# PDF(file="Figure1_7.pdf",width=7,height=5)
# jpeg("wkbook_3.jpeg", width = 6, height = 4, units = 'in', res = 600)

rng <- 0:2; dbinom(rng,2,1/13); dhyper(rng,4,48,2)   
rng=0:7; y=dpois(rng,2); plot(y~rng,type="h",ylab="pmf",xlab="Rng");
points(y~rng,pch=16,cex=2)
# =============================================================================
par(mfrow=c(3,3)); x= 0:35; lam=seq(2,18,2);

for(y in lam){plot(dpois(x,y)~x); title(paste("Mean is ",y))}

# pgamma(20+8.94,shape=5,scale=4)-pgamma(20-8.94,shape=5,scale=4)=.700

x=seq(.1,50,.1); plot(dgamma(x,shape=5,scale=4)~x)

vecs = sort(vec,decreasing=T)/n
n=sum(vecs)

nms = c("Medium","Fair","Dark","Red","Black")
barplot(vecs,beside=TRUE,names.arg=nms,ylab="",xlab="Haircolor")

theta=seq(.1,.5,.05); gam=ppois(2,theta*12)
plot(gam~theta,pch=" ",xlab=expression(theta),ylab=expression(gamma))
lines(gam~theta)
# 1a============================================================================
# plot Bernoulli

## probability mass functions of Binomial distributions
x <- seq(0,10,by=1)
binom_1 <- dbinom(x, size=10, prob=0.72)
binom_2 <- dbinom(x, size=10, prob=0.28)

barplot(binom_1, col = "red", density=10, names=x, xlab = "x",
        ylab="Probability", main = "Binomial distribution probability
mass function", legend.text = c("P(vote='For')","P(vote='Against')"), 
                                args.legend = list(x="topright"), 
        xlim = c(0,15), ylim = c(0, 0.5))

barplot(binom_2, col = "green", density=20, add = TRUE)
# abline(h=mean(binom_1), col="red")
# abline(h=var(binom_1), col="purple", )
box()

# PDF(file="Figure1_a.pdf",width=7,height=5)
# jpeg("Figure1_a.png", width = 6, height = 4, units = 'in', res = 600)

dev.off()
print(paste("Mean of the density is:", mean(y)))
# A 95% credible interval for the reliability is given 
#by CredInt_95= qbeta(c(.025,.975),a+d,b+g)
CredInt_95 = qbeta(c(.025,.975),a+d,b+g)
print(paste("95% Credible interval for the reliability is given
by:",CredInt_95[1],CredInt_95[2]))
# 1b==========================================================================
barplot(dpois(x=30:100,lambda=77), names.arg =30:100, ylim=c(0,0.05), 
        space=0, ylab ="Probability Density Function", xlab = "No. of Meteorites")
box()
# ======================================================================
rng=30:100; y=dpois(rng,77); plot(y~rng,type="h", ylab ="PDF", 
                                  xlab = "No. of Meteorites", 
                                 main="Poisson Distribution with lambda=77")
points(y~rng,pch=16,cex=2)
abline(v=77.0 , col="red")
abline(v=77.2 , lty=3, col="blue")
legend("topright", # Add legend to density
       legend = c("median", "Variance"),
       col = c("red", "blue"),
       lty = 1)
# ===============================================================
x <- seq(30, 90, 5); y <- dpois(x, 77)
plot(x,y, ylab ="Probability Density Function", 
     xlab = "No. of Meteorites", 
     main = "Poisson Distribution with lambda=77")
lines(y, x )
y <- dpois(x=30:100, 77)
abline(v=77 , col="red")
abline(v=77 , lty=3, col="blue")
legend("topright", # Add legend to density
       legend = c("median", "Variance"),
       col = c("red", "blue"),
       lty = 1)


print(paste("median of the system reliability is:", median(dpois(x,77))))
print(paste("variance of the system reliability is:", var(dpois(x,77))))

# 2a==========================================================================

x = c(-1.504, 0.371, 2.176, -3.627, -8.429, 2.759, 3.268, -9.362, -2.364, 
      3.569, 6.969, -0.466, 4.102, -8.282, 0.509, -0.422, -3.841, 7.938, 
      -1.062, -3.655)
y = c(-235.44, 166.51, 288.73, -421.32, 95.46, 476.25, 4.14, -366.79, 92.37, 
      612.29, -511.51, 268.67, -350.29, 234.35, 185.54, -217.03, -107.11, 
      367.32, -1019.37, -338.54)
abc = data.frame(x, y)
summary(abc)
plot(y~x, )
par(mfrow = c(1,1))

var(y)
var(x)
mean(y)
mean(x)
cov(x,y, use = "everything", method = "pearson")
# hmedian()
