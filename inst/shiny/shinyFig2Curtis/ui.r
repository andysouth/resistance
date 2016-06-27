#resistance/shiny/shinyFig2Curtis/ui.r
#andy south 7/8/15

library(shiny)


shinyUI(fluidPage(
  
  title = "Curtis insecticide resistance",
  
  h4("Development of resistance to insecticides used in mixtures and sequentially"),   
  h4("Plots will appear below in a few seconds. Left plot is Fig.2 from Curtis(1985)."),
  h4("Tweak Curtis' inputs and press lower button to change right plot."),  
  
  fluidRow(
    column(6, plotOutput('plotDefault')),
    column(6, plotOutput('plot'))
  ), #end fluid row
  
  hr(),
  
  h4("Curtis' input values are indicated in bold"),
  
  fluidRow(
    column(2,
           h5("Starting frequency of resistance"),
           numericInput("P_1", "locus1: 0.01", 0.01, min = 0.0001, max = 0.1, step = 0.001),
           numericInput("P_2", "locus2: 0.01", 0.01, min = 0.0001, max = 0.1, step = 0.001)
           #numericInput("P_2", "locus2 - proportion of locus1:", 0.1, min = 0.01, max = 100, step = 0.01)
    ),
    column(2, offset = 0,
           h5("Dominance of resistance"),
           numericInput("h.RS1_A0", "locus1: 0.17", 0.17, min = 0, max = 1, step = 0.01),
           numericInput("h.RS2_0B", "locus2: 0.0016", 0.0016, min = 0, max = 1, step = 0.01)
    ),
    column(2, offset = 0,
           h5("Exposure to each insecticide"),
           #in Curtis it assumes exposure to AB the same at 0.9 & same for M&F
           numericInput("exposure", "same for both insecticides in Curtis 0.9", 0.9, min = 0.1, max = 1, step = 0.05)
           #later may want to allow setting diff exposure for each insecticide & gender
           #numericInput("a.m_A", "insecticide1:", 0.5, min = 0.1, max = 0.9, step = 0.1),
           #numericInput("a.m_B", "insecticide2:", 0.5, min = 0.1, max = 0.9, step = 0.1)
    ),    
    column(2, offset = 0,
           #h5("Selection against susceptibles(SS) exposed to insecticide"),
           h5("Effectiveness of insecticides"),
           numericInput("phi.SS1_A0", "locus1: 0.73", 0.73, min = 0, max = 1, step = 0.05),
           numericInput("phi.SS2_0B", "locus2: 1", 1, min = 0, max = 1, step = 0.05)
    ),
    column(2, offset = 0,
           h5("Selective advantage of resistance"),
           numericInput("s.RR1_A0", "locus1: 0.23", 0.23, min = 0, max = 1, step = 0.05),
           numericInput("s.RR2_0B", "locus2: 0.43", 0.43, min = 0, max = 1, step = 0.05)
    ),
    column(2, offset = 0,
           h5("Extra params not included in Curtis"),
           numericInput("correct_mix_deploy", "Mixture correct deployment", 1, min = 0, max = 1, step = 0.1),
           checkboxInput("addCombinedStrategy", "plot adaptive strategy",FALSE)
    )
  ), #end fluid row
  fluidRow(
    column(4, hr()),
    column(2, actionButton('aButtonRun', 'Run Model (takes a few seconds'))
  ) #end fluid row
))