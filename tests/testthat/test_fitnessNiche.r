library(resistance)

context("fitness")

test_that("niche fitness ...", {

  
  # to do add a loop here to do tests for a range of input values
  
  expect_silent( a_fitnic <- fitnessNiche() )
  
  #these warnings are already in fitnessNiche()
  #so I could put expect_silent() around the above call and it would already be covered
  #error check for fitnesses > 1 or < 0
  # if ( any(a_fitnic > 1  ) ) 
  #   warning( sum(a_fitnic > 1  ), " niche fitness values (a_fitnic) are >1 ")
  # if ( any( a_fitnic < 0 ) ) 
  #   warning( sum( a_fitnic < 0 ), " niche fitness values (a_fitnic) are <0")   

  
  # ..$ locus1: chr [1:3] "SS1" "RS1" "RR1"
  # ..$ locus2: chr [1:3] "SS2" "RS2" "RR2"
  # ..$ niche1: chr [1:3] "0" "a" "A"
  # ..$ niche2: chr [1:3] "0" "b" "B"
  
  #expect ...
  #just an initial test
  #expect_equal( a_fitnic['RR1','RR2','A','B'], 1 )
  
  
})