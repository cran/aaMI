read.CX <- function(file){
  CX1 <- read.table(file,header=F,fill=T)
  CX1 <- CX1[2:nrow(CX1),1:2] #remove "ClustalX (n) multiple sequence alignment"
  CX1[,1] <- as.character(CX1[,1])
  CX1[,2] <- as.character(CX1[,2])
  SeqID <- unique(grep("[[:alnum:]]",CX1[,1],value=T))
  CX2 <- matrix("",nrow=length(SeqID),ncol=2)
  CX2[,1] <- SeqID
  for(i in 1:length(SeqID)){
    rep <- which(CX1[,1] == CX2[i,1])
    for(j in 1:length(rep))
      CX2[i,2] <- paste(CX2[i,2],CX1[rep[j],2],sep="")
  }
  CX2[,2] <- toupper(CX2[,2]) # Make sure all aa symbols are upper case.
  align1 <- matrix(NA,nrow=nrow(CX2),ncol=nchar(CX2[1,2]),
    dimnames=list(CX2[,1],seq(1:nchar(CX2[1,2]))))
  for(i in 1:nrow(align1))
    align1[i,] <- unlist(strsplit(CX2[i,2],split=""))
  align1 <- as.data.frame(align1)
  return(align1)
}
