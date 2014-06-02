library(quantmod)
library(MASS)
#library(ggplot2)
library(mixtools)


longMethod<- function(h,vix){
  if (vix>18) {m=10;ff=0;mmax=1} else {m=10;ff=0;mmax=1}
  h=tail(h,m)
  s=h$r.SPY; s2=I(h$r.SPY^2)
  roll=(h$r.XIV+h$r.VXX)/2;
  e=rlm(I(h$r.XIV-roll)~s+s2)	
  method="Long"
  alpha= coef(e)[1]+mean(roll); beta0=max(coef(e)[2],0)
  p= coef(summary(e))[1,3];gamma=coef(e)[3];
  if (alpha^2*gamma> 0.025) method="Short"
  return(list("method"=method,"m"=m,"ff"=ff,"mmax"=mmax,
              "alpha"=alpha,"beta0"=beta0,"gamma"=gamma,"p"=p,"e"=e))
}

shortMethod <- function(h,vix){
  if (vix>18) {m=9;ff=2;gg=-2;mmax=1} else {m=12;ff=1;gg=-1.3;mmax=1}
  h=tail(h,m)
  Rebalance=F
  s=h$r.SPY; s2=I(h$r.SPY^2)
  roll=(h$r.XIV+h$r.VXX)/2;
  e=rlm(I(h$r.XIV-roll)~s+s2)
  alpha= coef(e)[1]+mean(roll); beta0=max(coef(e)[2],0)
  p= coef(summary(e))[1,3];gamma=coef(e)[3];
  method="Short"
  if ( p< -0.05 && p> gg ||  p > 2) method="Long"
  return(list("method"=method,"m"=m,"ff"=ff,"mmax"=mmax,
              "alpha"=alpha,"beta0"=beta0,"gamma"=gamma,"p"=p,"e"=e))
  
}

NeutralMethod <- function(h,vix){
  L=longMethod(h,vix)
  S=shortMethod(h,vix)
  if (L$method == S$method) {
    if(L$method =="Long") return(S) else return(L)
  } else {
    return(list("method"="Neutral","m"=10,"ff"=0,"mmax"=1,
                "alpha"=0,"beta0"=Inf,"gamma"=0,"p"=0,"e"=S$e))
  }
}

onePeriod<- function(methodFinder,h,vix,d,lastj=0.25,nextr,b,pl=F)
{
  print(d)	
  if (is.null(methodFinder)) {if (d< "2014-01-01") M=shortMethod(h,vix) else M=longMethod(h,vix)}
  else M=methodFinder(h,vix)
  attach(M)
  
  series=1; flip=0
  if (!is.null(b)) {
    series=b$series+1
    if ( b$method != method) {
      flip=b$flip+1
      if (flip>ff && series >0)	{
        series=1; flip=0
      } else method=b$method
    }
  } 
  
  Rebalance=F	
  if (!is.null(b) && b$method != method) Rebalance=T
  
  if (method != "Neutral") {
    
    if (gamma > 0 ) {
      x=seq(-0.025,0.015,by=0.0001)	
    } else {
      x=seq(-0.015,0.025,by=0.0001)	
    }
    
    set.seed(1)
    mix=normalmixEM(h$r.SPY,maxit=10000)
    w=mix$lambda[1] * (pnorm(x+0.00005,mix$mu[1],mix$sigma[1])
                       -pnorm(x-0.00005,mix$mu[1],mix$sigma[1])) +
      mix$lambda[2] * (pnorm(x+0.00005,mix$mu[2],mix$sigma[2])
                       -pnorm(x-0.00005,mix$mu[2],mix$sigma[2]))	
    w=w/sum(w)
    pred= predict(e,interval="confidence",newdata=data.frame(s=x,s2=x^2), level=0.90)
    
    slope=coef(rlm(pred[,"fit"]~x, weights=w))[2]
    slope=min(max(0,slope),9)
  } else slope=beta0	
  
  j=1/(slope+1)
  
  oldSP=lastj/(lastj+(1-lastj)/3); nowSP=j/(j+(1-j)/3)
  if ( !Rebalance && abs(nowSP-oldSP) < 0.1 ) j=lastj else {lastj=j; Rebalance=T}
  
  
  if (method=="Neutral" ) {		
    leverage=1
    r1=1 #exp(nextr["r.SPY"])*3-2
    lastj=0				
    real=log(r1)*100
    expe=as.numeric(real)
  }
  
  if (method== "Short") {
    #	size=j/((1-j)/3+j)
    #	leverage=min(mmax,1/size)
    #	size=size*leverage
    #	r1=1-size
    #	r2=size*exp(nextr["r.XIV"])
    #	newsize=r2/(r1+r2)/leverage
    #	lastj=newsize/(3-2*newsize)
    #	real=log(r1+r2)*100
    #	expe=as.numeric(log((1-size/leverage)*(exp(-nextr["r.SPY"])*3-2)+r2/leverage))*100
    
    
    size=j/((1-j)/3+j);leverage=1
    r1=(1-size)*(exp(-nextr["r.SPY"])*3-2)
    r2=size*exp(nextr["r.XIV"])
    newsize=r2/(r1+r2)/leverage
    lastj=newsize/(3-2*newsize)				
    real=log(r1+r2)*100
    expe=as.numeric(real)
  }	
  
  if (method=="Long"){
    size=j/((1-j)/3+j);leverage=1
    r1=(1-size)*(exp(nextr["r.SPY"])*3-2)
    r2=size*exp(nextr["r.VXX"])
    newsize=r2/(r1+r2)/leverage
    lastj=newsize/(3-2*newsize)				
    real=log(r1+r2)*100
    expe=as.numeric(real)			
  }						
  
  names(lastj)=names(real)=NULL
  g=data.frame(date=d,method,Rebalance,real=round(real,4),
               expected=round(expe,3),pval=p,gamma=round(gamma,2),
               beta=round(slope,2),beta0=beta0,j=j,lastj=lastj,		
               vix=vix,spy=as.numeric(nextr["r.SPY"]),xiv=as.numeric(nextr["r.XIV"]),
               vxx=as.numeric(nextr["r.VXX"]),m,series, flip,
               leverage,alpha=round(alpha,4))
  rownames(g)=NULL
  if(pl) { draw(rbind(tail(h,m),nextr),method,d, -slope); print(g)}
  detach(M)	
  return(g)
}

draw <- function(h,method,d, optimS)
{ 
  hh=h
  xrng=c(min(-0.02,h$r.SPY),max(0.02,h$r.SPY))
  yrng=c(min(-0.1,h$r.XIV),max(0.1,h$r.XIV))
  x=seq(xrng[1],xrng[2],by=0.0001)
  h=head(h,nrow(h)-1)
  layout(matrix(c(1,3,3,2,3,3), 2,3, byrow=TRUE), respect=TRUE)
  par(ask=T,mar=c(3,4,2,1),outmar=c(0,0,0,0))
  plot(a$date,a$SPY,type="l",ylab="SPY",xlab="",lwd=2)
  lines(h$date,a$SPY[which (a$date %in% h$date)],lwd=2,col=2)
  plot(a$date,a$VIX,type="l",ylab="VIX",xlab="",lwd=2)
  lines(h$date,a$VIX[which (a$date %in% h$date)],lwd=2,col=2)
  h=hh
  
  
  roll=(h$r.XIV+h$r.VXX)/2;
  s=h$r.SPY; s2=I(h$r.SPY^2)
  e=rlm(I(h$r.VXX-roll)~s+s2)
  
  plot(h$r.SPY,h$r.VXX,xlab="SPY",ylab="VXX",type="n",xlim=xrng,ylim=yrng,main=d)
  pred= predict(e,interval="confidence",newdata=data.frame(s=x,s2=x^2), level=0.90)
  matlines(x,pred,col=2,lty=c(1,2,2),lwd=c(2,1,1),type="l")
  points(tail(h,1)$r.SPY,tail(h,1)$r.VXX,cex=2,pch=19)
  m=nrow(h)
  text(h$r.SPY,h$r.VXX,labels=m:1)
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  
  abline(0,b=coef(e)[2],lty=2,lwd=0.5)
  abline(a=0,b=optimS,lwd=2)	
  par(ask=F)  
}


main <- function(methodFinder){
  
  m=100
  book=NULL; lastj=0.25
  for (i in m:(nrow(r)-1))
  {	
    h<- r[(i-m+1):i,]
    vix= a[i,"VIX"]
    d=r[i,"date"]
    pd=F
    if (d> tail(r[,"date"],1)[1]) pd=T
    L=onePeriod(methodFinder, h,vix,d,lastj,r[i+1,],tail(book,1),pd)
    lastj=L$lastj
    book<-rbind(book,L)	
  }
  return(book)
}

emaweights<-function(m)
{
  alpha<-2/(m+1)
  i<-1:m
  sm<-sum((alpha*(1-alpha)^(1-i)))
  return(((alpha*(1-alpha)^(1-i)))/sm)  
}

Analyze<- function(book){
  #	sum(book[book$method=="Long", "expected"])
  #	sum(book[book$method=="Long", "real"])
  #	sum(book[book$method=="Short", "expected"])
  #	sum(book[book$method=="Short", "real"])
  
  ret=book$real/100 #*sign(book$intercept)/100
  realret=cumsum(ret)
  plot(book$date,realret,type="l",xlab="",ylab="",lwd=2)
  M=lm(realret~I(1:length(realret))-1)
  lines(book$date,fitted(M),lty=8)
  lines(book$date,cumsum(book$expected/100),lty=8,col=6)
  points(book$date[book$method=="Long"],realret[book$method=="Long"],col=2,pch=".",cex=4)
  points(book$date[book$method=="Short"],realret[book$method=="Short"],col=4,pch=".",cex=4)
  
  
  #	summary(book[book$method=="Long" , "real"]) ; mean(book[book$method=="Long" , "real"]>0)
  #	summary(book[book$method=="Short", "real"]) ; mean(book[book$method=="Short", "real"]>0)
  cat (" Sharpe Ratio:\t\t",mean(ret)/sd(ret)*sqrt(250),"\n",
       "Annualized Return\t",(exp(mean(ret)*250)-1)*100,"%\n",
       "Maximum Drawdown:\t", max(-getdrawdown(realret))*100,"%\n",
       "Average Rebal Period:\t",1/mean(book$Rebalance),"\n")
  
  nextret=tail(ret,sum(r$date>"2013-12-31"))
  
  cat ("since trading on 12/31/2013:\n","Sharpe Ratio:\t\t",mean(nextret)/sd(nextret)*sqrt(250),"\n",
       "Annualized Return\t",(exp(mean(nextret)*250)-1)*100,"%\n",
       "Actual Return\t\t",(exp(sum(nextret))-1)*100,"%\n",
       "Maximum Drawdown:\t", max(-getdrawdown(cumsum(nextret)))*100,"%\n")
  
  v=book$method
  w=c(which(c(0,diff(v=="Long"))!=0)-1,length(v))
  nextr=round(exp(c(realret[w[1]],diff(realret[w])))-1,3)
  s=data.frame(date=c(book$date[1],book$date[w[-length(w)]+1]),
               ret=nextr, method=book[w,"method"], len=c(w[1],diff(w)))
  cat("Average cumulative return by method:\n")
  print(by(s$ret,s$method,summary))
  cat("\n\nAverage holding days by method:\n")
  print(by(s$len,s$method,summary))
  
  
  print(tail(data.frame(date=book$date[book$date>="2014-03-31"],
                        acct= exp(cumsum(book[book$date>="2014-03-31","real"]/100))*285738),50))
  
  mv<- NULL; lookback=250
  for (i in lookback:nrow(book)){
    b=book[(i-lookback+1):i,]
    bs=b[b$method=="Short",]
    bl=b[b$method=="Long",]
    l=coef(summary(lm(I(bs$real/100)~poly(bs$spy,2,raw=T))))
    vs=data.frame(b$date[lookback],c((exp(l[,1]*c(1,mean(bs$spy),mean(bs$spy^2))*250)-1)*100),
                  c("alpha","beta","gamma"), "Short")
    l=coef(summary(lm(I(bl$real/100)~poly(bl$spy,2,raw=T))))
    vl=data.frame(b$date[lookback],c((exp(l[,1]*c(1,mean(bl$spy),mean(bl$spy^2))*250)-1)*100),
                  c("alpha","beta","gamma"), "Long")
    names(vl)<-names(vs)<-c("date","Contribution","Category", "method")
    mv= rbind(mv,vs,vl)
  }
  par(ask=T)  
  p <- ggplot(mv, aes(x=date, y=Contribution, group=Category,color=Category))
  p+ facet_grid(method~.,scales = "free") + theme(legend.position="bottom")+ geom_line(size=1.15)
}

############### today ###############

today<- function(book, methodFinder){
  m=100
  vix=tail(a$VIX,2)[1]
  L=onePeriod(methodFinder,tail(r,m+1)[1:m,],vix,tail(r$date,2)[1],tail(book$lastj,2)[1],
              r[nrow(r),],tail(book,2)[1,]) 
  book[nrow(book),]<-L
  vix=tail(a$VIX,1)
  L=onePeriod(methodFinder,tail(r,m),vix,tail(r$date,1),tail(book$lastj,1),r[nrow(r),],
              tail(book,1),F)
  intercept=L$intercept; slope=L$slope; convexity=L$convexity; 
  j=L$j;leverage=L$leverage
  cat("\thedge coef:",intercept,slope,convexity,"\n",
      "\tj=",j,"leverage=",leverage,"ratio=",(1-j)/j,"\n",
      "\tMethod:",as.character(L$method),"\tRebalance :",L$Rebalance,"\n")
  rbind(tail(book),L)
}
getStock <- function(tickers, start,end,compression ) {
  Sample<- NULL
  for (ticker  in tickers) {
    a<-df.get.hist.quote(ticker,start ,end,compression=compression)[,c(1,6,7)]
    Sample<-rbind(Sample,cbind(ticker,a))
  }
  Volume<- reshape(Sample,v.names="Volume",idvar="Date",timevar="ticker",direction="wide")[,-2]
  Sample<-reshape(Sample,v.names="Adj.Close",idvar="Date",timevar="ticker",direction="wide")[,-2]
  
  
  Sample[,1]<-as.Date(as.character(Sample[,1]))
  Sample<-Sample[order(Sample[,1]),]
  if (compression!="d") {
    xd<- which(diff(Sample[,1])==1)+1
    if (length(xd)>0) {
      for ( i in sort(xd,decreasing=T) ) {
        imputethis<- is.na(Sample[i-1,]) & !is.na(Sample[i,])
        Sample[i-1,imputethis]=Sample[i,imputethis]
      }
      Sample<-Sample[-xd,]
    }
  }
  Sample<- Sample[ apply(!is.na(Sample),1,sum)>2,]
  for (i in 2:dim(Sample)[1]) {
    for (j in 2:dim(Sample)[2]) {
      if (is.na(Sample[i,j])) Sample[i,j]=Sample[i-1,j] 
    }
  }
  names(Sample)=c("date",tickers)
  names(Volume)=c("date",tickers)
  return(list(data.frame(Sample),data.frame(Volume)))
}


`df.get.hist.quote` <-
  function (instrument = "ibm", start, end,compression="d",quote = c("Open","High", "Low","Close","Volume","Adj.Close"), provider = "yahoo", method = "auto") 
  {
    if (missing(start)) 
      start <- "1970-01-02"
    if (missing(end))
      end <- format(Sys.time() - 86400, "%Y-%m-%d")
    provider <- match.arg(provider)
    start <- as.POSIXct(start, tz = "GMT")
    end <- as.POSIXct(end, tz = "GMT")
    
    if (provider == "yahoo") {
      url <- paste("http://ichart.finance.yahoo.com/table.csv?s=", instrument, format(start, "&a=%m&b=%d&c=%Y"), format(end,"&d=%m&e=%d&f=%Y"), "&g=",compression,"&q=q&y=0", "&x=.csv", sep = "")
      destfile <- tempfile()
      status <- download.file(url, destfile, method = method)
      if (status != 0) {
        unlink(destfile)
        stop(paste("download error, status", status))
      }
      status <- scan(destfile, "", n = 1, sep = "\n", quiet = TRUE)
      if (substring(status, 1, 2) == "No") {
        unlink(destfile)
        stop(paste("No data available for", instrument))
      }
      x <- read.table(destfile, header = TRUE, sep = ",")
      unlink(destfile) 
      nser <- pmatch(quote, names(x))
      if (any(is.na(nser))) 
        stop("This quote is not available")
      n <- nrow(x)
      lct <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "C")
      on.exit(Sys.setlocale("LC_TIME", lct))
      dat <- gsub(" ", "0", as.character(x[, 1]))
      dat <- as.POSIXct(strptime(dat, "%Y-%m-%d"), tz = "GMT")        
      if (dat[n] != start) 
        cat(format(dat[n], "time series starts %Y-%m-%d\n"))
      if (dat[1] != end) 
        cat(format(dat[1], "time series ends   %Y-%m-%d\n"))
      return(data.frame(cbind(Date=I(format(dat[n:1],"%Y-%m-%d")),x[n:1,nser]),row.names=1:n))
    }
    else stop("Provider not implemented")
  }

AddTest <- function(Sample,Show=F){
  last<-dim(Sample)[1]
  if (Sample[last,1]< Sys.Date() ) {
    Sample<- rbind(Sample,Sample[last,])
    last<- last+1
  }
  Sample[last,-1] <- getQuote(paste(param$tickers,collapse=";"))[,2]
  Sample[last,1]<- Sys.Date()
  if (Show) print(tail(Sample))
  return(Sample)
}

`getReturns` <- function(x) return (c(0,diff(log(x))))
`getdrawdown` <- function(x) x-cummax(x)

`combine.ret.Cont` <-function(ret,w) ret %*% w
`combine.ret.Disc` <-function(ret,w) log(exp(ret) %*% t(w) - apply(w,1,sum)+ 1)
`ConttoDisc` <-function(x) exp(x)-1
`DisctoCont` <-function(x) log(1+x)


detachAllData<- function () 
{
  pos.to.detach <- (1:length(search()))[substring(search(), 
                                                  first = 1, last = 8) != "package:" & search() != ".GlobalEnv" & 
                                          search() != "Autoloads" & search() != "CheckExEnv"]
  for (i in 1:length(pos.to.detach)) {
    if (length(pos.to.detach) > 0) {
      detach(pos = pos.to.detach[1])
      pos.to.detach <- (1:length(search()))[substring(search(), 
                                                      first = 1, last = 8) != "package:" & search() != 
                                              ".GlobalEnv" & search() != "Autoloads" & search() != 
                                              "CheckExEnv"]
    }
  }
}
`f.test` <- function(x) {
  x<-  x[,!apply(is.na(x),2,any)]
  oneway.test(c(x)~rep(1:dim(x)[2],each=dim(x)[1]))
}


`last`<- function(x,n=1) 
  if (n>0) {
    if (class(x)=="matrix" ) matrix(x[(dim(x)[1]-n+1):dim(x)[1],],nr=n) 
    else x[(length(x)-n+1):length(x)]
  } else return(NULL)

`rm.last`<- function(x,n=1) if (n>0) {	
  if(class(x)=="matrix") {
    xn<-dim(x)[1]		
    x[-((xn-n+1):xn),] 
  } else {	xn<-length(x)
           x[-((xn-n+1):xn)] 
  }} else return(x)

attach.all <- function (x, overwrite = NA, name = "attach.all")  {
  rem <- names(x) %in% ls(.GlobalEnv)
  if (!any(rem)) overwrite <- FALSE
  rem <- names(x)[rem]
  if (is.na(overwrite)) {
    question <- paste("The following objects in .GlobalEnv will mask\nobjects in the attached database:\n", paste(rem, collapse = ", "), "\nRemove these objects from .GlobalEnv?", sep = "")
    if (interactive()) {
      if (.Platform$OS.type == "windows")  overwrite <- "YES" == winDialog(type = "yesno",  question)
      else overwrite <- 1 == menu(c("YES", "NO"), graphics = FALSE, title = question)
    }
    else overwrite <- FALSE
  }
  if (overwrite) remove(list = rem, envir = .GlobalEnv)
  attach(x, name = name,warn.conflicts = F)
}
