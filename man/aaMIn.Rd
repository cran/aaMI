\name{aaMIn}
\alias{aaMIn}
\title{Normalized Mutual Information for a Protein Sequence Alignment}
\description{Calculate a matrix of pairwise normalized mutual information values for a protein sequence alignment}
\usage{aaMIn(file)}
\arguments{
  \item{file}{a connection or character string giving the name of the file to load.}
}
\details{This script calculates the normalized mutual information between pairs of sites for a protein sequence alignment. The normalization constant used is the joint entropy, as described by Gloor, et al. (2005). The alignment must be in the form of a data frame with the sequence IDs as the row names and each site as a column. 
  The program begins by calculating the amino acid frequencies at each site. These frequencies are then used to calculate a vector containing the Shannon entropy H for each site. Shannon entropy is calculated using the equation
    \deqn{H_i = \sum_{i}( P(X_i)log2(P(X_i))}{%
H_i = sum[i] (P(X_i)log2(P(X_i)))}
where 	\eqn{P(X_i)}{%
P(X_i)} = frequency of amino acid X at site \emph{i} of the alignment.
  Next the program calculates the joint probabilities \eqn{P(X_i,Z_j)}{%
P(X_i,Z_j)} of pairs of amino acids X and Z at sites \emph{i} and \emph{j}. The 
joint probabilities are used to calculate the joint entropy with the formula
    \deqn{JH_ij = \sum_{i,j}(P(X_i,Z_j)log2(P(X_i,Z_j))}{%
      \sum_{i,j}(P(X_i,Z_j)log2(P(X_i,Z_j))}

  Shannon entropy and joint entropy are used to calculate the normalized mutual information MI with the formula
    \deqn{MIn_ij = (H_i + H_j - JH_ij) / JH_ij}{%
      MIn_ij = (H_i + H_j - JH_ij) / JH_ij }
}
\value{For the analysis of a protein sequence alignment data frame "file", the output is an NxN upper-triangular matrix, where N is the number of sites in the alignment. Values along the diagonal of the matrix are the entropy values (H) for each site.}
\author{Kurt Wollenberg}
\references{
Gloor, G. B, L. C. Martin, L. M. Wahl, and S. D. Dunn. (2005) Mutual information in protein multiple sequence alignments reveals two classes of coevolving positions. \emph{Biochemistry} \bold{44} 7156-7165.

Shannon, C. E. and W. Weaver. (1949) \emph{The Mathematical Theory of Communication}, University of Illinois Press.

Wollenberg, K. R. and W. R. Atchley. (2000) Separation of phylogenetic from functional associations in biological sequences by using the parametric bootstrap. \emph{Proceedings of the National Academy of Science} \bold{97} 3288-3291.}
\examples{
## Read in a protein sequence alignment file, FastA format
\dontrun{SeqDataFA <- read.FASTA("ProteinSeqFastA.txt")}
## Read in a protein sequence alignment file, ClustalX .aln format
\dontrun{SeqDataCX <- read.CX("ProteinSeq.aln")}
## Read in a protein sequence alignment file, GeneDoc .msf format
\dontrun{SeqDataGD <- read.Gdoc("ProteinSeq.msf")}

## Calculate the mutual information matrix for one of these alignments.
\dontrun{ProteinSeqminorm <- aaMIn(SeqDataGD)}
}
\keyword{file} 
\keyword{univar}
