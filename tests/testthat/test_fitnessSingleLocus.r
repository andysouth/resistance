library(resistance)

context("fitness")

test_that("single locus fitness ...", {
  
  
  # to do add a loop here to do tests for a range of input values
  
  expect_silent( a_fitloc <- fitnessSingleLocus() )
  
  #these warnings are already in fitnessSingleLocus()
  #so I could put expect_silent() around the above call and it would already be covered
  # if ( any( a_fitloc > 1  ) ) 
  #   warning( sum(a_fitloc > 1 ), " locus fitness values (a_fitloc) are >1 : ", a_fitloc[a_fitloc>1])
  # if ( any( a_fitloc < 0 ) ) 
  #   warning( sum( a_fitloc < 0 ), " locus fitness values (a_fitloc) are <0")   

  # todo be careful that it is still possible to genereate fitness > 1
  # because selection_coefficient is used
  # if resistance_restoration was used here (instead of being converted to selectio_co earlier)
  # then i think it should be impossible to have fitness > 1
  
  # test like this
  # > a_fitloc['SS1',]
  # no  lo  hi 
  # 1.0 1.0 0.5 
  # > a_fitloc['SS1','hi']
  # [1] 0.5
  # > a_fitloc[,'hi']
  # SS1  RS1  RR1  SS2  RS2  RR2 
  # 0.50 0.75 1.00 0.50 0.75 1.00   
  
  #expect ...
  #just an initial test
  #expect_equal( a_fitloc['RR1','hi'], 1 )
  
  
})