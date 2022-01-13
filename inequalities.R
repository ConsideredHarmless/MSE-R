# Cineqmembers<-function(mate){
#   #Cineqmembers[mate] generates all the members required to form the inequalities for one to one relationships defined by the mate.
# 
#   members<-lapply(seq_along(mate),function(x) lapply(seq_along(mate[[x]][[1]][[1]]),function(y)  unlist(c(x,mate[[x]][[1]][[1]][y],mate[[x]][[2]][[y]]))))
# 
# 
#   comb<-lapply(seq_along(members),function(x) (t(combn(seq_along(members[[x]]),2))))
#   comb<-lapply(seq_along(comb),function(x) split(comb[[x]], 1:nrow(comb[[x]])))
# 
#   right<-lapply(seq_along(comb),function(x) lapply(seq_along(comb[[x]]),function(k) c(members[[x]][comb[[x]][[k]][1]],members[[x]][comb[[x]][[k]][2]])))
#   left<-lapply(seq_along(right), function(k) lapply(seq_along(right[[k]]),function(j) list(c(right[[k]][[j]][[1]][1:2],right[[k]][[j]][[2]][3]),c(right[[k]][[j]][[2]][1:2],right[[k]][[j]][[1]][3]))))
# 
#   return(lapply(seq_along(right),function(i) {right$line<-list(right[[i]],left[[i]])}))
# }
# Cineqmembers<-function(mate){
#   #Cineqmembers[mate] generates all the members required to form the inequalities for one to one relationships defined by the mate.
#   
#   members<-lapply(seq_along(mate),function(x) lapply(seq_along(mate[[x]][[1]][[1]]),function(y) lapply(seq_along(mate[[x]][[2]][[y]]), function(k)  unlist(c(x,mate[[x]][[1]][[1]][y],mate[[x]][[2]][[y]][[k]])))))
#   
#   
#   comb<-lapply(seq_along(members),function(x) (t(combn(seq_along(members[[x]]),2))))
#   comb<-lapply(seq_along(comb),function(x) split(comb[[x]], 1:nrow(comb[[x]])))
#   
#   right<-lapply(seq_along(comb),function(x) lapply(seq_along(comb[[x]]),function(k) unlist(c(members[[x]][comb[[x]][[k]][1]],members[[x]][comb[[x]][[k]][2]]),recursive=FALSE)))
#   left<-lapply(seq_along(comb),function(x) lapply(seq_along(comb[[x]]),function(k) unlist(lapply(seq_along(members[[x]][comb[[x]][[k]][2]][[1]]), function(j) unique(unlist(lapply(seq_along(members[[x]][comb[[x]][[k]][1]][[1]]), function(i) list(c(members[[x]][comb[[x]][[k]][1]][[1]][[1]][1:2],members[[x]][comb[[x]][[k]][2]][[1]][[j]][3]),
#                                                                                                                                                                                                                                          c(members[[x]][comb[[x]][[k]][2]][[1]][[1]][1:2],members[[x]][comb[[x]][[k]][1]][[1]][[i]][3]))),recursive=FALSE))),recursive=FALSE)))
#   
#   return(lapply(seq_along(right),function(i) {right$line<-list(right[[i]],left[[i]])}))
# }
# 
# Cineqmembers<-function(mate){
#   #Cineqmembers[mate] generates all the members required to form the inequalities for one to one relationships defined by the mate.
#   
#   members<-lapply(seq_along(mate),function(x) lapply(seq_along(mate[[x]][[1]][[1]]),function(y) lapply(seq_along(mate[[x]][[2]][[y]]), function(k)  unlist(c(x,mate[[x]][[1]][[1]][y],mate[[x]][[2]][[y]][[k]])))))
#   
#   
#   comb<-lapply(seq_along(members),function(x) (t(combn(seq_along(members[[x]]),2))))
#   comb<-lapply(seq_along(comb),function(x) split(comb[[x]], 1:nrow(comb[[x]])))
#   
#   right<-lapply(seq_along(comb),function(x) lapply(seq_along(comb[[x]]),function(k) unlist(c(members[[x]][comb[[x]][[k]][1]],members[[x]][comb[[x]][[k]][2]]),recursive=FALSE)))
#   
#   left<-lapply(seq_along(comb),function(x) lapply(seq_along(comb[[x]]),function(k) unlist(lapply(seq_along(members[[x]][comb[[x]][[k]][2]][[1]]), function(j) unique(unlist(lapply(seq_along(members[[x]][comb[[x]][[k]][1]][[1]]), function(i) list(c(members[[x]][comb[[x]][[k]][1]][[1]][[1]][1:2],members[[x]][comb[[x]][[k]][2]][[1]][[j]][3]),
#                                                                                                                                                                                                                                                      c(members[[x]][comb[[x]][[k]][2]][[1]][[1]][1:2],members[[x]][comb[[x]][[k]][1]][[1]][[i]][3]))),recursive=FALSE))),recursive=FALSE)))
#                                                                                                                                                                                                                                                         
#                                                                                                                                                                                                                                                         
#   mate2<-lapply(seq_along(mate), function(x) lapply(seq_along(mate[[x]]), function(y) lapply(seq_along(mate[[x]][[y]]), function(z) unlist(ifelse(length(mate[[x]][[y]][[z]])==0,NA,list(mate[[x]][[y]][[z]]))))))
#   members2<-lapply(seq_along(mate2),function(x) lapply(seq_along(mate2[[x]][[1]][[1]]),function(y) lapply(seq_along(mate2[[x]][[2]][[y]]), function(k)  unlist(c(x,mate2[[x]][[1]][[1]][y],mate2[[x]][[2]][[y]][[k]])))))
#   
#   is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
#   x<-lapply(seq_along(left), function(x) lapply(seq_along(left[[x]]), function(y) lapply(seq_along(left[[x]][[y]]), function(z) unlist(unlist(ifelse(is.na(left[[x]][[y]][[z]][[3]]),list(NULL),list(left[[x]][[y]][[z]])))))))
#   rmNullObs <- function(x) {
#     x <- Filter(Negate(is.NullOb), x)
#     lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
#   }
#   new<-rmNullObs(x)
#   left<-lapply(seq_along(new), function(x) lapply(seq_along(new[[x]]), function(y) unique(new[[x]][[y]])))
#   return(lapply(seq_along(right),function(i) {right$line<-list(right[[i]],left[[i]])}))
# }

Cineqmembers<-function(mate){
  #Cineqmembers[mate] generates all the members required to form the inequalities for one to one relationships defined by the mate.
  
  members<-lapply(seq_along(mate),function(x) lapply(seq_along(mate[[x]][[1]][[1]]),function(y) lapply(seq_along(mate[[x]][[2]][[y]]), function(k)  unlist(c(x,mate[[x]][[1]][[1]][y],mate[[x]][[2]][[y]][[k]])))))
  
  
  comb<-lapply(seq_along(members),function(x) (t(combn(seq_along(members[[x]]),2))))
  comb<-lapply(seq_along(comb),function(x) split(comb[[x]], 1:nrow(comb[[x]])))
  
  right<-lapply(seq_along(comb),function(x) lapply(seq_along(comb[[x]]),function(k) unlist(c(members[[x]][comb[[x]][[k]][1]],members[[x]][comb[[x]][[k]][2]]),recursive=FALSE)))
  
  
  
  
  
  temp<-lapply(seq_along(mate), function(x) lapply(seq_along(mate[[x]]), function(y) lapply(seq_along(mate[[x]][[y]]), function(z) unlist(unlist(ifelse(length(mate[[x]][[y]][[z]])==0,0,list(mate[[x]][[y]][[z]])))))))
  members<-lapply(seq_along(temp),function(x) lapply(seq_along(temp[[x]][[1]][[1]]),function(y) lapply(seq_along(temp[[x]][[2]][[y]]), function(k)  unlist(c(x,temp[[x]][[1]][[1]][y],temp[[x]][[2]][[y]][[k]])))))
  
  
  comb<-lapply(seq_along(members),function(x) (t(combn(seq_along(members[[x]]),2))))
  comb<-lapply(seq_along(comb),function(x) split(comb[[x]], 1:nrow(comb[[x]])))
  left<-lapply(seq_along(comb),function(x) lapply(seq_along(comb[[x]]),function(k) unlist(lapply(seq_along(members[[x]][comb[[x]][[k]][2]][[1]]), function(j) unique(unlist(lapply(seq_along(members[[x]][comb[[x]][[k]][1]][[1]]), function(i) list(c(members[[x]][comb[[x]][[k]][1]][[1]][[1]][1:2],members[[x]][comb[[x]][[k]][2]][[1]][[j]][3]),
                                                                                                                                                                                                                                                     c(members[[x]][comb[[x]][[k]][2]][[1]][[1]][1:2],members[[x]][comb[[x]][[k]][1]][[1]][[i]][3]))),recursive=FALSE))),recursive=FALSE)))
  x<-lapply(seq_along(left), function(x) lapply(seq_along(left[[x]]), function(y) unique(lapply(seq_along(left[[x]][[y]]), function(z) unlist(unlist(ifelse((left[[x]][[y]][[z]][[3]]==0),list(NULL),list(left[[x]][[y]][[z]]))))))))
    
  
  is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
  rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  }
  new<-rmNullObs(x)
  
  left<-lapply(seq_along(right), function(x) lapply(seq_along(right[[x]]), function(y) unlist(ifelse(class(try(new[[x]][[y]],silent = TRUE)) == "try-error",list(list()),list(new[[x]][[y]])),recursive=FALSE)))
  return(lapply(seq_along(right),function(i) {right$line<-list(right[[i]],left[[i]])}))
}

Cinequalities<-function(f,ineqmembers){
  #inequalities[f,ineqmembers] apply properly the f function to ineqmembers to create inequalities. This routine is called by CdataArray internally where as a function it uses payoffMatrix
  return(lapply(seq_along(ineqmembers),function(x) lapply(seq_along(ineqmembers[[x]][[1]]),function(y) (f[[ineqmembers[[x]][[1]][[y]][[1]][1]]][[ineqmembers[[x]][[1]][[y]][[1]][2]]][[ineqmembers[[x]][[1]][[y]][[1]][3]]]+f[[ineqmembers[[x]][[1]][[y]][[2]][1]]][[ineqmembers[[x]][[1]][[y]][[2]][2]]][[ineqmembers[[x]][[1]][[y]][[2]][3]]]-f[[ineqmembers[[x]][[2]][[y]][[1]][1]]][[ineqmembers[[x]][[2]][[y]][[1]][2]]][[ineqmembers[[x]][[2]][[y]][[1]][3]]]-f[[ineqmembers[[x]][[2]][[y]][[2]][1]]][[ineqmembers[[x]][[2]][[y]][[2]][2]]][[ineqmembers[[x]][[2]][[y]][[2]][3]]] ))))
}

CineqmembersNew <- function(mate) {
  mateList <- lapply(result$mate, as.list)
  # FIXME
  return(lapply(mateList, f))
}

# TODO name(marketMates) computes the inequality indexes (TODO find better name)
# for a single market.
# The structure marketMates is a list with members:
#   $UpStream: a vector of the unique indexes of upstreams for this market.
#   $DownMates: a list with n elements, where n is the number of upstreams.
#     element $DownMates[[i]] is a vector of the downstream indexes which the
#     upstream with index i is matched to.
#
# Each transposition (i, j) of the set {1, 2, ..., n} yields a single
# inequality.
#
# Returns:
#   TODO
f <- function(marketMates) {
  uIdxs <- marketMates$UpStream
  n <- length(uIdxs)
  # First, we create the transposition indexes {(i, j): 1 <= i < j <= n} as a
  # pair of vectors.
  iIdxs <- unlist(lapply(1:n, function (x) { rep(x, n-x) }))
  jIdxs <- unlist(lapply(1:n, function (x) { seq(from=x+1, to=n, length=max(0,n-x)) }))
  # getMates <- function(i) { m$DownMates[[i]] }
  # TODO replace list of pairs with pair of lists
  # TODO fix wording
  # p(i, j) prepends the value i to the mates of j.
  # p <- function(i, j) { lapply(getMates(j), function(x) { list(i, x) }) }
  p <- function(i, j) { lapply(marketMates$DownMates[[j]], function(x) { list(i, x) }) }
  # maybe:
  # pUp <- function(i, j) { lapply(marketMates$DownMates[[j]], function(x) {i}) }
  pUp <- function(i, j) { rep_len(i, length(marketMates$DownMates[[j]])) }
  pDn <- function(i, j) { marketMates$DownMates[[j]] }
  diag <- lapply(uIdxs, function(i) { p(i, i) })
  # maybe:
  diagUp <- lapply(uIdxs, function(i) { pUp(i, i) })
  # diagDn <- lapply(uIdxs, function(i) { pDn(i, i) })
  diagDn = marketMates$DownMates
  # mkIneq <- function(i, j) { list(f = c(p(i, i), p(j, j)), c = c(p(i, j), p(j, i))) }
  mkIneq <- function(i, j) { list(f = c(diag[[i]], diag[[j]]), c = c(p(i, j), p(j, i))) }
  ineqIdxs <- mapply(mkIneq, iIdxs, jIdxs)
  # or:
  factualIdxs = mapply(function(i, j) { c(diag[[i]], diag[[j]]) }, iIdxs, jIdxs)
  counterfactualIdxs = mapply(function(i, j) { c(p(i, j), p(j, i)) }, iIdxs, jIdxs)
  # maybe:
  fctUpIdxs = mapply(function (i, j) { c(diagUp[[i]], diagUp[[j]]) }, iIdxs, jIdxs)
  fctDnIdxs = mapply(function (i, j) { c(diagDn[[i]], diagDn[[j]]) }, iIdxs, jIdxs)
  cfcUpIdxs = mapply(function (i, j) { c(pUp(i, j),   pUp(j, i))   }, iIdxs, jIdxs)
  cfcDnIdxs = mapply(function (i, j) { c(pDn(i, j),   pDn(j, i))   }, iIdxs, jIdxs)
  return(ineqIdxs)
  # TODO time it!
}