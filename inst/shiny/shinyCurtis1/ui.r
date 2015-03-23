#resistance/shiny/shinyCurtis1/ui.r
#andy south 22/3/15

library(shiny)


shinyUI(fluidPage(
  
  title = "Curtis insecticide resistance",
  
  plotOutput('plot'),
  
  hr(),
  
  fluidRow(
    column(2,
           h5("Starting frequency of resistance"),
           numericInput("P_1", "locus1:", 0.05, min = 0.01, max = 0.1, step = 0.01),
           numericInput("P_2", "locus2 - proportion of locus1:", 0.1, min = 0.01, max = 100, step = 0.01)
    ),
    column(2, offset = 0,
           h5("Dominance of resistance"),
           numericInput("h.RS1_00", "locus1:", 0.2, min = 0, max = 1, step = 0.05),
           numericInput("h.RS2_00", "locus2:", 0.2, min = 0, max = 1, step = 0.05)
    ),
    column(2, offset = 0,
           h5("Fitness of susceptibles(SS) exposed"),
           numericInput("phi.SS1_A0", "locus1:", 0.05, min = 0, max = 0.4, step = 0.05),
           numericInput("phi.SS2_0B", "locus2:", 0.05, min = 0, max = 0.4, step = 0.05)
    ),
    column(2, offset = 0,
           h5("Selective advantage of resistance"),
           numericInput("s.RR1_A0", "locus1:", 0.5, min = 0.2, max = 1, step = 0.1),
           numericInput("s.RR2_0B", "locus2:", 0.5, min = 0.2, max = 1, step = 0.1)
    )
    
  )
))