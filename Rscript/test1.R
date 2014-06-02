library(RJSONIO)
args <- commandArgs(T)

preJSON <-function(tmp){
  list(traits = colnames(tmp),values = unlist(apply(tmp,1,list),recursive=F))
}

dates=seq(as.Date('2012-01-01'),as.Date('2014-06-01'), by ='week')
n=length(dates)
data=data.frame(date= dates,ret=cumsum(rnorm(n,0.001,0.01)),ret2=cumsum(rnorm(n,0.001,0.01)))
timestamp=Sys.time()


write2Json=toJSON(list(data=preJSON(data), timestamp=toString(timestamp), 
                        args=args ), pretty=T)
jsonName=paste('returns/',args[4],'.json',sep="")

write(write2Json,jsonName)


