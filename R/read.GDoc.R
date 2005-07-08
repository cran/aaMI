read.GDoc <- function(file){
  GD1 <- read.table(file,header=F,fill=T)
  GD1 <- GD1[(which(GD1[,1] == "1")):nrow(GD1),1:6] # remove initial gibberish
  for(i in 1:ncol(GD1))
    GD1[,i] <- as.character(GD1[,i])
  SeqID <- unique(grep("[[:alpha:]]",GD1[,1],value=T)) # extract sequence names
  GD2 <- matrix("",nrow=length(SeqID),ncol=2)
  GD2[,1] <- SeqID
  for(i in 1:length(SeqID)){
    rep <- which(GD1[,1] == GD2[i,1])
    for(j in 1:length(rep)){
      for(k in 2:ncol(GD1))
        GD2[i,2] <- paste(GD2[i,2],GD1[rep[j],k],sep="")
    }
  }
  GD2[,2] <- toupper(GD2[,2]) # Make sure all aa symbols are upper case.
  GD2[,2] <- gsub("\\.","-",GD2[,2]) # change style of gap character
  align1 <- matrix(NA,nrow=nrow(GD2),ncol=nchar(GD2[1,2]),
    dimnames=list(GD2[,1],seq(1:nchar(GD2[1,2]))))
  for(i in 1:nrow(align1))
    align1[i,] <- unlist(strsplit(GD2[i,2],split=""))
  align1 <- as.data.frame(align1)
  return(align1)
}
