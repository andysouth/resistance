library(resistance)

context("fitness")

test_that("niche fitness ...", {
  
  # Wniche <- fitnessNiche( Wloci = Wloci,
  #                         niche = niche,
  #                         Wniche = Wniche )
  
  # to do add a loop here to do tests for a range of input values
  
  expect_silent( Wniche <- fitnessNiche() )
  
  #these warnings are already in fitnessNiche()
  #so I could put expect_silent() around the above call and it would already be covered
  #error check for fitnesses > 1 or < 0
  # if ( any(Wniche > 1  ) ) 
  #   warning( sum(Wniche > 1  ), " niche fitness values (Wniche) are >1 ")
  # if ( any( Wniche < 0 ) ) 
  #   warning( sum( Wniche < 0 ), " niche fitness values (Wniche) are <0")   

  
  # ..$ locus1: chr [1:3] "SS1" "RS1" "RR1"
  # ..$ locus2: chr [1:3] "SS2" "RS2" "RR2"
  # ..$ niche1: chr [1:3] "0" "a" "A"
  # ..$ niche2: chr [1:3] "0" "b" "B"
  
  #expect ...
  #just an initial test
  expect_equal( Wniche['RR1','RR2','A','B'], 1 )
  
  
})