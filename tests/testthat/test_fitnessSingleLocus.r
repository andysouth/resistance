library(resistance)

context("fitness")

test_that("single locus fitness ...", {
  
  # Wloci <- fitnessSingleLocus(h = h,
  #                             s = s,
  #                             phi = phi,
  #                             z = z)
  # can be called with arrays or
  # effectiveness1 effectiveness
  # effectiveness2 effectiveness
  # dominance dominance
  # selection_co selection_coefficient
  # cost fitness cost of R in no insecticide
  # SS fitness of SS if no insecticide
  
  # to do add a loop here to do tests for a range of input values
  
  expect_silent( Wloci <- fitnessSingleLocus() )
  
  #these warnings are already in fitnessSingleLocus()
  #so I could put expect_silent() around the above call and it would already be covered
  # if ( any( Wloci > 1  ) ) 
  #   warning( sum(Wloci > 1 ), " locus fitness values (Wloci) are >1 : ", Wloci[Wloci>1])
  # if ( any( Wloci < 0 ) ) 
  #   warning( sum( Wloci < 0 ), " locus fitness values (Wloci) are <0")   

  # todo be careful that it is still possible to genereate fitness > 1
  # because selection_coefficient is used
  # if resistance_restoration was used here (instead of being converted to selectio_co earlier)
  # then i think it should be impossible to have fitness > 1
  
  # test like this
  # > Wloci['SS1',]
  # no  lo  hi 
  # 1.0 1.0 0.5 
  # > Wloci['SS1','hi']
  # [1] 0.5
  # > Wloci[,'hi']
  # SS1  RS1  RR1  SS2  RS2  RR2 
  # 0.50 0.75 1.00 0.50 0.75 1.00   
  
  #expect ...
  #just an initial test
  expect_equal( Wloci['RR1','hi'], 1 )
  
  
})