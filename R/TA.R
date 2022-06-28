#' TODO
#'
#' @param X TODO
#' @param y TODO
#' @param b0 TODO
#' @param trials TODO
#' @param nS TODO
#' @param nT TODO
#' @param q TODO
#' @param stepUp TODO
#' @param nat0 TODO
#' @return TODO
#' @keywords internal
TA_estimator <- function(X,y,b0,trials,nS,nT,q,stepUp,nat0) {
  X <- as.matrix(X)
  n=length(y)
  TT <- dim(X)[1]
  pp <- dim(X)[2]
  p <- pp-1

  if (missing(nat0) || is.null(nat0)){
    nat0 <- pracma::randperm(1:TT,p)
  }

  algo <- list(x0 = nat0,
               neighbour = neighbour,
               #nS = 1000L,
               nS = nS,
               #nT = 25L,
               nT = nT,
               nD = 2000L,
               #q = 0.3,
               q = q,
               printBar = TRUE,
               printDetail = 500,
               storeF = TRUE,
               storeSolutions = FALSE,
               stepUp = stepUp,
               #scale = 0.01
               scale = 1
  )

  # ptm <- proc.time() # Start the clock!
  #try(res <- TAopt(objfun,algo,X,y,b0), silent=TRUE)
  #trials = 15
  restarts1 <- NMOF::restartOpt(NMOF::TAopt, trials, OF = objfun, algo = algo, X,y,b0)
  restarts1F <- sapply(restarts1, `[[`, "OFvalue")
  mpos <- order(restarts1F)[1] # position of min obj value

  natF <- restarts1[[mpos]]$xbest
  fvalF <- restarts1[[mpos]]$OFvalue

  # elapsed_time <- proc.time() - ptm  # Stop the clock
  #Af=X[natF,]
  #bmbf=y[natF]
  p <- pp-1

  Af=matrix(0,p,p)
  bmbf=numeric(p)
  mbf=numeric(p)

  for (j in 1:p) {
    if (y[natF[j]] >= 0) {
      mbf[j]=1
    }
    else {
      mbf[j]=0
    }
  }

  for (j in 1:p) {
    for (i in 1:p) {
      Af[i,j] <- (1-2*mbf[i])*X[natF[i],j+1]
    }
  }


  for (i in 1:p) {
    bmbf[i] <- b0 * (-1+2*mbf[i]) * X[natF[i],1]
  }


  #argminTA = solve(Af,bmbf)
  #-------------------- replace the above argminTA = solve(Af,bmbf) with the following
  #solve pxp linear system
  if (rcond(Af) > .Machine$double.eps) {
    argminTA=solve(Af,bmbf)
  } else {
    argminTA<-as.vector(MASS::ginv(Af)%*%bmbf); names(argminTA)<-colnames(Af)
  }
  #--------------------

  #fvalTA <- distanceObj(argminTA,X,ys,yc)  #verify: it should be the same as "fvalF"

  #plot(restarts1[[mpos]]$Fmat[1000:dim(restarts1[[mpos]]$Fmat)[1],2],main="TA optimization convergence", xlab="function evals",ylab="CLAD objective",type="l",lwd = 2)
  #dim(res$Fmat)

  res=list(argminTA,natF,fvalF,restarts1)
  return(res)
}

neighbour <- function(nat, X,y,b0){

  #p=dim(X)[2]
  p=dim(X)[2]-1
  T=dim(X)[1]

  status=F
  while (!status) {

    rI = floor(1+runif(1)*p)  #select randomly position rI to perturb in nat
    rJ = floor(1+runif(1)*T)  #select randomly value    rJ to perturb in nat
    nat[rI]=rJ

    status= length(unique(nat)) == p
  }
  return(nat)
}

objfun <- function(nat,X,y,b0) {

  TT=dim(X)[1]
  pp=dim(X)[2]

  p <- pp-1

  mb=numeric(p)
  mA=matrix(0,p,p)
  bmb=numeric(p)

  #compute mA
  # for (j in 1:p) {
  #   for (jj in 1:p) {
  #   #mA[1,j]=X[nat[1],j]
  #   #mA[2,j]=X[nat[2],j]
  #   #mA[3,j]=X[nat[3],j]
  #   #mA[,j]=X[nat,j]
  #   mA[jj,j]=X[nat[jj],j]
  #   }
  # }
  ###mA=X[nat,]

  for (j in 1:p) {
    if (y[nat[j]] >= 0) {
      mb[j]=1
    }
    else {
      mb[j]=0
    }
  }

  for (j in 1:p) {
    for (i in 1:p) {
      mA[i,j] <- (1-2*mb[i])*X[nat[i],j+1]
    }
  }

  #do j=1,p
  #do i=1,p
  #mA(i,j)=(1-2*mb(i))*Z(j+1,id(nat(i)))
  #!        mA(2,j)=(1-2*mb(2))*Z(j,id(nat(2)))
  #!        mA(3,j)=(1-2*mb(3))*Z(j,id(nat(3)))
  #enddo
  #enddo

  #compute bmb
  #bmb[1]=y[nat[1]]
  #bmb[2]=y[nat[2]]
  #bmb[3]=y[nat[3]]
  ###bmb=y[nat]
  for (i in 1:p) {
    bmb[i] <- b0 * (-1+2*mb[i]) * X[nat[i],1]
  }

  #do i=1,p
  #bmb(i)=b0*(-1+2*mb(i))*Z(1,id(nat(i)))
  #enddo
  #solve pxp linear system
  # if (abs(det(mA)) > 0.001) {
  # thetas=solve(mA,bmb)
  # }
  # if (abs(det(mA)) < 0.001) {
  #   thetas=MASS::ginv(mA)%*%bmb
  # }
  if (rcond(mA) > .Machine$double.eps) {
    thetas=solve(mA,bmb)
  } else {
    thetas=MASS::ginv(mA)%*%bmb
  }


  #x2=X[,2]
  #x3=X[,3]
  #res=distanceObj2(thetas,X,y,yc,ctype)
  # res=ScoreObj1(thetas,X,y,b0)
  ys=as.integer((1+y)/2)
  #res=ScoreObj2(thetas,X[-nat,],ys[-nat],b0)
  res=ScoreObj2(thetas,X,ys,b0)
  return(res)
}

# ScoreObj1 <- function(thetas,X,y,b0) {
#   b <- thetas
#   #X <- as.matrix(X[,-dim(X)[2]])
#   #X <- as.matrix(X[,-c(1)])
#   X <- as.matrix(X)
#   #yHat2 <- X%*%b + b0*rep(1,dim(X)[1])
#   #yHat2 <- X%*%b + b0*X[,1]
#   yHat2 <- X[,-c(1)]%*%b + b0*X[,1]
#   hfire <- (sum(y[yHat2>0]) - sum(y[yHat2<0]))/length(y)
#   hfire <- (1+hfire)/2
#   #score1 <- -hfire
#   score1 <- 1-hfire
#   s <- score1
#   return(s)
# }

ScoreObj2 <- function(thetas,X,ys,b0) {
  b <- thetas
  p=length(b)
  # N=length(ys)+p
  #X <- as.matrix(X[,-dim(X)[2]])
  #X <- as.matrix(X[,-c(1)])
  X <- as.matrix(X)
  #yHat2 <- X%*%b + b0*rep(1,dim(X)[1])
  #yHat2 <- X%*%b + b0*X[,1]
  # yhat <- X[,-c(1)]%*%b + b0*X[,1]
  yhat <- X%*%c(b0,b)
  temp=(yhat>=0)
  temp <- sum(ys[temp]) - sum(ys[!temp]-1)
  # valuevector <- ys*(yhat>=0) + (1-ys)*(yhat<0)
  absscore <- temp + p
  return(-absscore)
}
