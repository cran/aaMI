read.FASTA <- function(file){
  FA1 <- read.table(file,header=F,fill=T,as.is=T)
  SeqID <- grep(">",FA1[[1]]) # extract row numbers of sequence names
  skip <- nrow(FA1)/length(SeqID)
  FA2 <- matrix("",nrow=length(SeqID),ncol=2)
  for(i in 0:nrow(FA1)-1){
    F2row <- trunc(i/skip)+1
    if((i+1) %in% SeqID) {FA2[F2row,1] <- FA1[i+1,]
     }else{
       FA2[F2row,2] <- paste(FA2[F2row,2],FA1[i+1,],sep="")
     } # combine multiple rows of sequence into one row 
  }
  align1 <- matrix(NA,nrow=nrow(FA2),ncol=nchar(as.character(FA2[1,2])),
                   dimnames=list(FA2[,1],seq(1:nchar(as.character(FA2[1,2])))))
  for(i in 1:nrow(align1))
    align1[i,] <- unlist(strsplit(as.character(FA2[i,2]),split=""))
  align1 <- as.data.frame(align1)
  row.names(align1) <- gsub(">","",row.names(align1))
  return(align1)
}
