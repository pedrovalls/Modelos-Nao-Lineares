#######################################################
############### Nao Linearidades no IBOVESPA #################
#######################  ####################### 
######## Pedro Valls - FGV-EESP ######
# clear workspace
rm(list=ls())
# set local directory to where data set is 
setwd("C:/Users/Pedro/Dropbox/topicos_em_financasII/Nao_Linear")



# Packages -----------------------------------------------------------------
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
  }
  require(x,character.only=TRUE)
}

load_package('readxl')
load_package('xts')
load_package('tseries')
load_package('forecast')
load_package('cents')
load_package('urca')
load_package('varhandle')
load_package('lmtest')


###

# Date
dados<-read_xls('bolsa.xls',"Ibv")
attach(dados)
dados<-xts(dados[,-1], order.by = Data)
ibov_ts=ts(dados$ibvf)

plot(Data,ibov_ts,type='l', col='blue', main = 'IBOVESPA',ylab="Ibov")

##
# transform the series to LIBOV=log(ibov)
##
libov=log(ibov_ts)
plot(Data,libov,type='l', col='red', main = 'LOG IBOVESPA',ylab="Ibov")

##
# take lag of the series and 1st and 2nd differences
##
# libov_1=c(NA,libov[1:length(libov)-1])

d1libov=diff(libov,differences=1)

##
# best model is an AR(0) with constant
# residuals from the model is the series without its mean
##
d1libov_dmean=d1libov-mean(d1libov)

##
# ACF and PACF for d1libov_dmean
##
par(mfrow=c(2,1))
acf(d1libov_dmean, lag.max=12)
pacf(d1libov_dmean, lag.max=12)

##
# squared of residuals
##
d1libov_dmean_sq=d1libov_dmean^2

##
# ACF and PACF for d1libov_dmean_sq
##
par(mfrow=c(2,1))
acf(d1libov_dmean_sq, lag.max=12)
pacf(d1libov_dmean_sq, lag.max=12)

##
# BDS test for DLIBOV
##
DLIBOV_BDS = bds.test(d1libov_dmean,m=6)

DLIBOV_BDS

##
# Test RAMSEY for DLIBOV
##
##
#first take as a regressor lag of d1libov
#
d1libov_1=d1libov[1:length(libov)-1]

## 
# test linear against quadratic
##

resettest(d1libov ~ d1libov_1, power =2, type="regressor")

## 
# test linear against cubic 
##

resettest(d1libov ~ d1libov_1, power =3, type="regressor")



