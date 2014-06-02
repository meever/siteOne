rm(list=ls())
library(RJSONIO)
args <- commandArgs(T)
source("VixHedgeLib.R")
options(warn=-1,digits=8);

####################################################


datestamp=format(Sys.time(),"%Y%m%d")
timestamp=format(Sys.time(),"%Y%m%d%H%M%S")

preJSON <-function(tmp){
  list(traits = colnames(tmp),values = unlist(apply(tmp,1,list),recursive=F))
}

if (args[2] == 'history') {
  param  <- list(StartDate ="2010-06-01",freq="d",
                 tickers =c("SPY","^VIX","VXX","XIV"),
                 EndDate=format(Sys.time() , "%Y-%m-%d"))
  attach.all(param,T)
  a <- getStock(tickers,StartDate,EndDate,freq)
  Volume <- a[[2]]
  a<- a[[1]]
  a <- na.exclude(AddTest(a,T))
  
  names(a)[-1]= c("SPY","VIX","VXX","XIV")
  r<- data.frame(date=as.Date(a[,1]),r=apply(a[,-1],2,getReturns))
  bookS<- main(shortMethod)
  bookL<- main(longMethod)
  save(bookS, bookL, file=paste('.//returns/VIXhedgebook',timestamp,'.book',sep=''))
  save.image(paste('.//returns/VIXhedgebook',datestamp,'.RData',sep=''))
  data=data.frame(date=bookS[,1],shortVol=cumsum(bookS[,4]/100),longVol=cumsum(bookL[,4]/100))
}

if (args[2] == 'live') {
  originalargs=args
  load(paste('.//returns/VIXhedgebook',datestamp,'.RData',sep=''))
  args=originalargs
  a<- AddTest(a,T)
  r<- data.frame(date=as.Date(a[,1]),r=apply(a[,-1],2,getReturns))
  S <- tail(today(bookS,shortMethod),1)
  L <- tail(today(bookL,longMethod),1)
  data=data.frame('short'=S,'long'=L)
  
}

write2Json=toJSON(list(data=preJSON(data), timestamp=toString(Sys.time()), 
                       args=args ), pretty=T)
jsonName=paste('returns/',args[4],'.json',sep="")

write(write2Json,jsonName)