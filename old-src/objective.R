objective<-function(b){
  #objective[x1,x2,...,xn] defines the objective function to minimize, as the number of satisfied inequalities. For a specific x-vector value we get a list of numbers. The number of positives is the outcome.
  return(-(sum((matrix(unlist(dataArray,use.names=FALSE),nrow=length(dataArray),byrow=TRUE)%*%c(coefficient1,b) )>0)))
}
  
objective2<-function(b){
  #objective[x1,x2,...,xn] defines the objective function to minimize, as the number of satisfied inequalities. For a specific x-vector value we get a list of numbers. The number of positives is the outcome.
  return(-(sum((matrix(unlist(ssDataArray,use.names=FALSE),nrow=length(ssDataArray),byrow=TRUE)%*%c(coefficient1,b) )>0)))
}

objective3<-function(b){
  
  #up1[, payoff := 1+dist1*x1+dist2*x2+dist3*x3+dist4*x4+dist5*x5]
  up1[, payoff := c(1,c(dist1,dist2,dist3,dist4,dist5))*c(1,b)]
  #up1N<-up1[,.N,by=c("Market","UpStream1","UpStream2")]
  up1P<-up1[,sum(payoff),by=c("Market","UpStream1","UpStream2")]
  
  #up2[, payoff := 1+dist1*x1+dist2*x2+dist3*x3+dist4*x4+dist5*x5]
  up2[, payoff := c(1,c(dist1,dist2,dist3,dist4,dist5))*c(1,b)]
  #up2N<-up2[,.N,by=c("Market","UpStream1","UpStream2")]
  up2P<-up2[,sum(payoff),by=c("Market","UpStream1","UpStream2")]
  
  #up3[, payoff := 1+dist1*x1+dist2*x2+dist3*x3+dist4*x4+dist5*x5]
  up3[, payoff := c(1,c(dist1,dist2,dist3,dist4,dist5))*c(1,b)]
  #up3N<-up3[,.N,by=c("Market","UpStream1","UpStream2")]
  up3P<-up3[,sum(payoff),by=c("Market","UpStream1","UpStream2")]
  
  #up4[, payoff := 1+dist1*x1+dist2*x2+dist3*x3+dist4*x4+dist5*x5]
  up4[, payoff := c(1,c(dist1,dist2,dist3,dist4,dist5))*c(1,b)]
  #up4N<-up4[,.N,by=c("Market","UpStream1","UpStream2")]
  up4P<-up4[,sum(payoff),by=c("Market","UpStream1","UpStream2")]
  
  #uN<-up1N+up2N-up3N-up4N
  uP<-up1P+up2P-up3P-up4P
  
  p<-uP[V1>0,.N]
  #n<-uP[V1<0,.N]
  #p+n
  return(p)
}