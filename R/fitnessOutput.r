#' output fitness by niche
#' 
#' @param Wniche array of niche fitnesses
#' 
#' @examples 
#' #fitnessOutput()

#' @return fitness values in a matrix
#' @export

fitnessOutput <- function ( Wniche = NULL )
{
  
  ## output fitnesses by niche & genotype
  
  fbn <- matrix( ncol=9, nrow=9 )
  
  colnames(fbn) <- c("-,-", "a,-", "A,-", "-,b", "-,B", "a,b", "A,B", "A,b", "a,B")
  #default order is different : 00 a0 A0 0b ab Ab 0B aB AB
  #potential problems in ordering of rows & cols avoided by filling by name below
  
  #29/9/16 note this doesn't output cis & trans separately
  
  rownames(fbn) <- c("SS1SS2", "SS1RS2", "SS1RR2",
                     "RS1SS2", "RS1RS2", "RS1RR2",
                     "RR1SS2", "RR1RS2", "RR1RR2" )
  
  for( locus1 in dimnames(Wniche)$locus1)
  {
    for( locus2 in dimnames(Wniche)$locus2)
    {
      #this is a good way of doing but the columns end in a different order
      #which wouldn't be a problem except that initially I'm trying
      #to keep results identical to Beths
      #fbn[paste0(locus1,locus2),] <- Wniche[locus1,locus2,,]
      #so instead go through each niche
      for( niche1 in dimnames(Wniche)$niche1)
      {
        for( niche2 in dimnames(Wniche)$niche2)
        {
          #get columnName by converting 0 to - and inserting commas
          columnName <- paste0(niche1,",",niche2)
          columnName <- gsub('0','-',columnName)
          #get rowname by pasting
          rowName <- paste0(locus1,locus2)
          
          fbn[rowName,columnName] <- Wniche[locus1,locus2,niche1,niche2]
        }
      }
    }
  }
  
  return(fbn)  
}