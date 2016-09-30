#' calculation Linkage Disequilibrium for this generation and save to results
#' 
#' 
#' 
#' @param a_gtypes array of genotype frequencies
#' @param recomb_rate recombination rate
#' @param gen_num generation number
#' @param results matrix
#' 
#' @examples 
#' linkage_calc()

#' @return results matrix with values for this generation added
#' @export

linkage_calc <- function ( a_gtypes, 
                           recomb_rate,
                           gen_num,
                           results ){	
  

  # the G calc here is not used outside of linkage calc so can go in the func
  # also these vars not used later : dprime*, r2
  
  # Gametes from before selection frequencies to estimate linkage disequilibrium 
  G <- createGametes( a_gtypes = a_gtypes, recomb_rate = recomb_rate ) 
  
  ## Linkage Disequilibrium of resistant allele in gametes
  # Male
  # Frequency of allele patterns
  x.R1.S2 <- G['m','R1','S2']/2
  x.S1.R2 <- G['m','R1','S2']/2
  x.R1.R2 <- G['m','R1','R2']
  
  # Frequency of alleles at each locus
  R1 <- x.R1.S2 + x.R1.R2		# frequency of R allele at locus 1
  R2 <- x.R1.R2 + x.S1.R2		# frequency of R allele at locus 2
  m.D <- x.R1.R2 - (R1 * R2)
  
  
  #Matt: Code reordered and edited to calculate D' for males and females separately. 
  
  # Finding D' for males
  D <- m.D	
  
  S1 <- 1 - R1	# frequency of S at each allele = 1 - frequency of R
  S2 <- 1 - R2
  
  p1q2 <- R1 * S2	# Find P1Q2 and P2Q1 (given P = loc 1 and 1 = R allele)
  p2q1 <- S1 * R2
  
  #dmax is the lower of these two
  if( p1q2 < p2q1 ){		
    dmax <- p1q2
  }else{
    dmax <- p2q1
  }
  
  p1q1 <- ( R1 * R2 )	#Find p1q1 and p2q2, given conditions above
  p2q2 <- ( S1 * S2 )
  
  if( p1q1  > p2q2 ){	#dmin is the lowest of these
    dmin <- p2q2
  }else{
    dmin <- p1q1
  }
  
  if( D>0 ){				# if D is greater than 0
    dprime.m <- D/dmax		# D' = D/dmax 
  }else{					# if D is less than 0
    dprime.m <- D/dmin		# D' = D/dmin
  }
  
  results[gen_num,10] <- dprime.m	# prints to column ten of results matrix
  
  
  # Female
  # Frequency of allele patterns
  x.R1.S2 <- G['f','R1','S2']/2
  x.S1.R2 <- G['f','R1','S2']/2
  x.R1.R2 <- G['f','R1','R2']
  
  # Frequency of alleles at each locus
  R1 <- x.R1.S2 + x.R1.R2		# frequency of R allele at locus 1
  R2 <- x.R1.R2 + x.S1.R2		# frequency of R allele at locus 2
  f.D <- x.R1.R2 - (R1 * R2)
  
  # save linkage disequilibrium results
  results[gen_num,4] <- m.D
  results[gen_num,7] <- f.D
  
  
  # Matt: Finding D' for females
  D <- f.D		# D is given as male LD
  
  S1 <- 1 - R1	# frequency of S at each allele = 1 - frequency of R
  S2 <- 1 - R2
  
  p1q2 <- R1 * S2	# Find P1Q2 and P2Q1 (given P = loc 1 and 1 = R allele)
  p2q1 <- S1 * R2
  
  #dmax is the lower of these two
  if( p1q2 < p2q1 ){		
    dmax <- p1q2
  }else{
    dmax <- p2q1
  }
  
  p1q1 <- ( R1 * R2 )	#Find p1q1 and p2q2, given conditions above
  p2q2 <- ( S1 * S2 )
  
  if( p1q1  > p2q2 ){	#dmin is the lowest of these
    dmin <- p2q2
  }else{
    dmin <- p1q1
  }
  
  if( D>0 ){				# if D is greater than 0
    dprime.f <- D/dmax		# D' = D/dmax 
  }else{					# if D is less than 0
    dprime.f <- D/dmin		# D' = D/dmin
  }
  
  results[gen_num,12] <- dprime.f	# prints to column twelve of results matrix
  
  
  ## R2
  denom <- sqrt(R1 * S1 * R2 * S2)	# finds R2 using the allale frequencies calculated above
  r2 <- D/denom						# use this and D to find r2
  
  results[gen_num,11] <- r2					# prints to column eleven of results matrix
  
  return(results)
  
}	