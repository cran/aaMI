aaMI <- function(file){
  AA <- c(LETTERS[-c(2,10,15,21,24,26)],"-")
  AA.F <- matrix(0,nrow=21,ncol=ncol(file),dimnames=list(AA,seq(1:ncol(file))))
  H <- vector(length=ncol(file))
  MIt <- matrix(0,nrow=ncol(file),ncol=ncol(file))

  for(i in 1:ncol(file))
    AA.F[names(summary(file[[i]])),i] <- (summary(file[[i]])/length(file[[i]]))

  for(i in 1:ncol(file))
   H[i] <- -1*sum(AA.F[AA.F[,i] != 0,i]*log2(AA.F[AA.F[,i] != 0,i]))

  for(i in 1:ncol(file)){
    for(j in i:ncol(file)){
      FrqT <- table(file[,i],x[,j])/sum(table(file[,i],x[,j]))
      MIt[i,j] <- H[i]+H[j]+sum(FrqT[FrqT != 0] * log2(FrqT[FrqT != 0]))
    }
  }
  return(MIt)
}
