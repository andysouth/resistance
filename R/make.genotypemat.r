#' Uses HW function to make matrix of genotype values per matrix
#' 
#' Takes frequencies of resistant allele at each locus - user inputted

#' @param P_1 todo
#' @param P_2 todo
#' 
#' @return matrix
#' @export

make.genotypemat <- function ( P_1, P_2 ){
  
  #for locus 1
  loc1 <- matrix(c(rep(0,3)), ncol=1)  	
  loc1 <- HW( P_1, loc1 )				# Entered in order SS RS RR, found using P entered above
  #for locus 2
  loc2 <- matrix(c(rep(0,3)), ncol=1)		#for locus 2
  loc2 <- HW( P_2, loc2 )				# Entered in order SS RS RR, found using P entered above
  
  mat <- matrix (c(rep(0,10)), ncol=1)
  colnames(mat) <- "Freq."
  rownames(mat) <- c("SS1SS2", "SS1RS2", "SS1RR2", 
                     "RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
                     "RR1SS2", "RR1RS2", "RR1RR2")
  mat[1,] <- loc1[1,]*loc2[1,]		   ## locus 1 matrix SS, locus 2 matrix SS (always in row 1)
  mat[2,] <- loc1[1,]*loc2[2,]
  mat[3,] <- loc1[1,]*loc2[3,]
  mat[4,] <- loc1[2,]*loc2[1,]
  ## two forms of RS1RS2
  mat[5,] <- ( loc1[2,]*loc2[2,] ) * 0.5 #cis
  mat[6,] <- ( loc1[2,]*loc2[2,] ) * 0.5 #trans
  mat[7,] <- loc1[2,]*loc2[3,]		   ## locus 1 matrix RS, locus 2 matrix RR (always in rows 2 and 3 respectively)
  mat[8,] <- loc1[3,]*loc2[1,]
  mat[9,] <- loc1[3,]*loc2[2,]
  mat[10,] <- loc1[3,]*loc2[3,]
  
  return(mat)
}