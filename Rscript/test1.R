library(RJSONIO)
args <- commandArgs(T)

preJSON <-function(tmp){
  list(traits = colnames(tmp),values = unlist(apply(tmp,1,list),recursive=F))
}
data=data.frame(date= c("2014-05-28","2014-05-29","2014-05-30","2014-05-31")
                ,ret=runif(4),ret2=runif(4))
timestamp=Sys.time()


write2Json=toJSON(list(data=preJSON(data), timestamp=toString(timestamp), 
                        args=args ), pretty=T)
jsonName=paste('returns/',args[4],'.json',sep="")

write(write2Json,jsonName)

