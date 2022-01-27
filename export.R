export<-function(filename){
  y<-do.call("rbind",lapply(seq_along(distanceMatrices),function(i) do.call("rbind", lapply(seq_along(distanceMatrices[[i]]),function(j) matrix(unlist(lapply(seq_along(1:nrow(distanceMatrices[[i]][[j]])),function(k) c(i,j,k,as.numeric(distanceMatrices[[i]][[j]][k,]),matchMatrix[[i]][[j]][[k]])),recursive =FALSE),ncol=(4+noAttr),byrow=TRUE)))))
  write.table(y, file=filename, col.names=header,row.names=FALSE, sep="\t", quote=FALSE)
}

# The same function as above, but more readable.
exportReadable <- function(filename) {
    mIdxs <- seq_along(distanceMatrices)
    f <- function(mIdx) {
        g <- function(uIdx) {
            makeRow <- function(dIdx) {
                return(c(mIdx, uIdx, dIdx, as.numeric(distanceMatrices[[mIdx]][[uIdx]][dIdx, ]), matchMatrix[[mIdx]][[uIdx]][[dIdx]]))
            }
            dIdxs <- seq_along(1:nrow(distanceMatrices[[mIdx]][[uIdx]]))
            p <- lapply(dIdxs, makeRow)
            q <- unlist(p, recursive=FALSE)
            return(matrix(q, ncol=(4+noAttr), byrow=TRUE))
        }
        uIdxs <- seq_along(distanceMatrices[[mIdx]])
        return(do.call("rbind", lapply(uIdxs, g)))
    }
    y <- do.call("rbind", lapply(mIdxs, f))
    write.table(y, file=filename, col.names=header,row.names=FALSE, sep="\t", quote=FALSE)
}
