# set local directory to where data set is 
setwd("C:/Users/Pedro/Dropbox/topicos_em_financasII/Nao_Linear/R_script")



# Load package using a function load_package-----------------------------------------------------------------
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

load_package('tseries')
load_package('urca')
load_package('dlm')
load_package('openxlsx')
load_package('xts')
load_package('class')
load_package('zoo')
load_package('fBasics')
load_package('qrmtools')
load_package('stats')
load_package('MTS')
load_package('vars')
load_package('graphics')
load_package('MASS')
load_package('urca')
load_package('NTS')
load_package('TSA')


# sample size
n <- 1000

##
# Fix the random seed 
##
set.seed(1234)

##
# generate a standard normal distribuition 
##

gwn1=ts(rnorm(n,mean=0,sd=1))


#
#
# simulated Bilinear Modelo BL(0,0,1,1) 
# r(t) = psi0 + u(t) + beta11*r(t-1)*u(t-1)
# u(t) ~ NI(0,1)
# r(1) = 1
# psi0=0
# beta11 = .5
##
r_bl <- 0
u <- 0 


r_bl[1] = 1
u[1]= gwn1[1]
psi0= 0

for(i in 2:n){
  u[i] = gwn1[i]
  r_bl[i] <- psi0 + 0.5*r_bl[i-1]*u[i-1]  + u[i] 
}

##
#plot the series r_bl(t)
##
par(mfrow=c(1,1))
plot(r_bl,type='l', col='blue', main = 'Modelo BL(0,0,1,1)')


##
# acf e pacf for r_bl(t)
#
par(mfrow=c(2,1))
acf(r_bl, lag.max=12)
pacf(r_bl, lag.max=12)

##
# Simulate a CHARMA (s,0,0) using s=1
#
# r(t) -  pis0 = a(t) = u(t)*sqrt(r(t)|I(t-1))
# Var(r(t)|I(t-1))= sigma(u)*(1+beta(1)a(t-1)+...+beta(s)a(t-s-1))^2
# u(t) ~ NI(0,1) and a(0) =0 

r_charma <- 0 
a <- 0
u <- 0
sigma <- 0
beta1 = 0.95
psi0 = 0
u[1]=gwn1[1]
sigmau=var(gwn1)
sigma[1]=sigmau*((1+beta1*0)^2)
a[1]=u[1]*sqrt(sigma[1])
r_charma[1]=psi0+a[1]


for(i in 2:n){
  u[i] = gwn1[i]
  sigma[i]=sigmau*((1+beta1*a[i-1])^2)
  a[i]=u[i]*sqrt(sigma[i])
  r_charma[i] = psi0 + a[i]
  }

##
#plot the series r_charma(t)
##
par(mfrow=c(1,1))
plot(r_charma,type='l', col='blue', main = 'Modelo CHARMA(1,0,0)')

##
# acf e pacf for r_charma(t)
#
par(mfrow=c(2,1))
acf(r_charma, lag.max=12)
pacf(r_charma, lag.max=12)

##
# define square of r_charma 
##
r_charma_sq=r_charma^2

##
#plot the series r_charma_sq(t)
##
par(mfrow=c(1,1))
plot(r_charma_sq,type='l', col='blue', main = 'Squared of Modelo CHARMA(1,0,0)')

##
# acf e pacf for r_charma_sq(t)
#
par(mfrow=c(2,1))
acf(r_charma_sq, lag.max=12)
pacf(r_charma_sq, lag.max=12)


##
# SETAR(2,1)
#
# r(t) = psi01 + psi11*r(t-1) + sigma1*u(t)  if  r(t-1) < 0
# r(t) = psi02 + psi12*r(t-1) + sigma2*u(t)  if  r(t-1) >= 0
# u(t) ~ NI(0,1)
# psi01 = 0
# pis11 = -1.5
# sigma1 = 1.0
# psi02 = 0
# psi12 = 0.5
# sigma2 = 1.0
##


r_setar <- 0 
u <- gwn1
sigma1 <- 1.0
psi01 <- 0
psi11=-1.5
psi02=0
psi12=0.5
sigma2=1.0


u[1]=sigma2*gwn1[1]
r_setar[1]=ts(psi02+psi12*0+ sigma2*u[1])


for(i in 2:n){
  if(r_setar[i-1]<0){
    r_setar[i] = psi01 + psi11*r_setar[i-1] + sigma1*u[i]
    
  }
  else {
    r_setar[i] = psi02 + psi12*r_setar[i-1] + sigma2*u[i]
  }
}   

##
#plot the series r_setar(t)
##
par(mfrow=c(1,1))
plot(r_setar,type='l', col='blue', main = 'Modelo SETAR(2,1)')

##
# acf e pacf for r_Setar(t)
#
par(mfrow=c(2,1))
acf(r_setar, lag.max=12)
pacf(r_setar, lag.max=12)

rsetar=ts(r_setar)

# Histogram r_setar
par(mfrow=c(1,1))
hist(rsetar, breaks = 50, freq=F,
     xlab='', ylab='', main='')
dist<-function(n){
  dnorm(n, mean(rsetar), sd(rsetar))
}
curve(dist, add=T, col='red')
d<-density(rsetar)
lines(d, col='blue')
legend('topleft', legend=c('RSETAR','Normal','Kernel'),
       col=c(1,2,4), pch=15)

## 
# descritive stat 
##
summary(r_setar)

##
# normality test
##
r_setar_jq =  jarque.bera.test(r_setar)
r_setar_jq


##
# scatter plot r_setar(t-1) x r_setar(t) and local smoothing method to fit best curve 
##
y <- rsetar[2:1000]
x <- rsetar[1:999]
msetar_loess <- loess( y ~x)
sx <- sort(x,index=T)
par(mfcol=c(1,1))
ix <- sx$ix
plot(x,y,xlab='x(t-1)',ylab='x(t)')
lines(x[ix],msetar_loess$fitted[ix],col='red')

## 
# the slope of the fitted line seems to change when x(t-1) is around 0.0.
# a two regime model seems suitable and the threshold should be 0.0
##

##
# To estimate this model by CLS use TAR in the TSA package 
##
rsetar.tar.1=tar(rsetar,1,1,d=1,is.constant1=TRUE,is.constant2=TRUE,transform="no",estimate.thd=TRUE,method=c("CLS"),print=TRUE)

##
# To estimate this model by MAIC use TAR in the TSA package 
##
rsetar.tar.1=tar(rsetar,1,1,d=1,is.constant1=TRUE,is.constant2=TRUE,transform="no",estimate.thd=TRUE,method=c("MAIC"),order.select=TRUE, print=TRUE)

##
# LSTAR
##


# r(t) = (psi01 + psi11*r(t-1))*(1-(1/(1+exp(-1*r(t-d))))) 
#       + (1/(1+exp(-1*r(t-d))))*(psi02 + psi12*r(t-1))+ u(t)
# # u(t) ~ NI(0,1)
# psi01 = 0.5
# pis11 = 0.8
# psi02= -0.5
# psi12=0.3
# d = 1 
##


r_Lstar <- 0 
u <- gwn1
psi0 <- 0.5
psi1 <- 0.8


u[1]=gwn1[1]
r_Lstar[1]=(0.5+0.8*0)*(1-(1/(1+exp(-1*0))))+ (1/(1+exp(-1*0)))*(-0.5+0.3*0) +u[1]




for(i in 2:n){
  
  r_Lstar[i]= (0.5+0.8*r_Lstar[i-1])*(1-(1/(1+exp(-1*r_Lstar[i-1]))))+ (1/(1+exp(-1*r_Lstar[i-1])))*(-0.5+0.3*r_Lstar[i-1]) +u[i]
  }   

##
#plot the series r_Lstar(t)
##
par(mfrow=c(1,1))
plot(r_Lstar,type='l', col='blue', main = 'Modelo LSTAR(2,1)')

##
# acf e pacf for r_Lstar(t)
#
par(mfrow=c(2,1))
acf(r_Lstar, lag.max=12)
pacf(r_Lstar, lag.max=12)
