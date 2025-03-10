library(resistance)

context("Refactoring")

test_that("refactoring named variables with arrays doesn't change results", {
  
  
  skip("not doing refactoring tests anymore")
  
  input <- setInputOneScenario()
  
  #expect refactored runModel2() to generate same results as runModel() for single default run
  #expect_identical( runModel2(input, produce.plots=FALSE),
  #                  runModel(input, produce.plots=FALSE) )  
  expect_equal( runModel2(input, produce.plots=FALSE),
                    runModel(input, produce.plots=FALSE) )  
  
  # removed this because they started to differ due to a valid change
  
  #set random seed (may want to move option to setInputSensiScenarios() later )
  # set.seed(1)
  # input <- setInputSensiScenarios(20, h.RS1_A0=c(0.1,1), h.RS2_0B=c(0.1,1), s.RR1_A0=c(0.2,1), s.RR2_0B=c(0.2,1))
  # 
  # #expect refactored runModel2() to generate same results as runModel() for a range of scenarios
  # listOut2 <- runModel2(input, produce.plots=FALSE)
  # listOut <- runModel(input, produce.plots=FALSE)
  # expect_equal( listOut2, listOut )
})