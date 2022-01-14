CdataArray<-function(distanceMatrices,ineqmembers){
  #CdataArray creates the dataArray.  It uses ineqmembers and distanceMatrices

   return(unlist(lapply(seq_along(ineqmembers),function(x) lapply(seq_along(ineqmembers[[x]][[1]]),function(y) ((Reduce("+",lapply(seq_along(ineqmembers[[x]][[1]][[y]]), function(k) colSums(distanceMatrices[[ineqmembers[[x]][[1]][[y]][[k]][1]]][[ineqmembers[[x]][[1]][[y]][[k]][2]]][ineqmembers[[x]][[1]][[y]][[k]][3],]-distanceMatrices[[ineqmembers[[x]][[2]][[y]][[k]][1]]][[ineqmembers[[x]][[2]][[y]][[k]][2]]][ineqmembers[[x]][[2]][[y]][[k]][3],] ))))))),recursive=FALSE))
}

# The same function as above, but more readable.
CdataArrayReadable <- function(distanceMatrices, ineqmembers) {
    xs <- seq_along(ineqmembers)
    f <- function(x) {
        ys <- seq_along(ineqmembers[[x]][[1]])
        g <- function(y) {
            zs <- seq_along(ineqmembers[[x]][[1]][[y]])
            h <- function(k) {
                i1 <- ineqmembers[[x]][[1]][[y]][[k]][1]
                i2 <- ineqmembers[[x]][[2]][[y]][[k]][1]
                j1 <- ineqmembers[[x]][[1]][[y]][[k]][2]
                j2 <- ineqmembers[[x]][[2]][[y]][[k]][2]
                k1 <- ineqmembers[[x]][[1]][[y]][[k]][3]
                k2 <- ineqmembers[[x]][[2]][[y]][[k]][3]
                v1 <- distanceMatrices[[i1]][[j1]][k1,]
                v2 <- distanceMatrices[[i2]][[j2]][k2,]
                return(colSums(v1-v2))
            }
            return(Reduce("+", lapply(zs, h)))
        }
        return(lapply(ys, g))
    }
    return(unlist(lapply(xs, f), recursive=FALSE))
}

# The same function as above, but with meaningful names.
CdataArrayReorganized <- function(distanceMatrices, ineqmembers) {
    mIdxs <- seq_along(ineqmembers)
    f <- function(mIdx) {
        ineqmembersSingle <- ineqmembers[[mIdx]]
        ineqmsLHSAll <- ineqmembersSingle[[1]]
        ineqmsRHSAll <- ineqmembersSingle[[2]]
        ineqIdxs <- seq_along(ineqmsLHSAll)
        g <- function(ineqIdx) {
            ineqmsLHS <- ineqmsLHSAll[[ineqIdx]]
            ineqmsRHS <- ineqmsRHSAll[[ineqIdx]]
            termIdxs <- seq_along(ineqmsLHS)
            h <- function(termIdx) {
                idxTripleLHS <- ineqmsLHS[[termIdx]]
                idxTripleRHS <- ineqmsRHS[[termIdx]]
                mIdxLHS <- idxTripleLHS[1]
                uIdxLHS <- idxTripleLHS[2]
                dIdxLHS <- idxTripleLHS[3]
                mIdxRHS <- idxTripleRHS[1]
                uIdxRHS <- idxTripleRHS[2]
                dIdxRHS <- idxTripleRHS[3]
                valLHS <- distanceMatrices[[mIdxLHS]][[uIdxLHS]][dIdxLHS,]
                valRHS <- distanceMatrices[[mIdxRHS]][[uIdxRHS]][dIdxRHS,]
                return(colSums(valLHS-valRHS))
            }
            return(Reduce("+", lapply(termIdxs, h)))
        }
        return(lapply(ineqIdxs, g))
    }
    return(unlist(lapply(mIdxs, f), recursive=FALSE))
}
