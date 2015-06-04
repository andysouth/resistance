#resistance/shiny/shinyCurtis2/ui.r
#andy south 6/4/15

library(shiny)


shinyUI(fluidPage(
  
  title = "Curtis insecticide resistance",
  
  h3("Insecticide resistance explorer2. Modify inputs below and the plots will respond."),
  #h4("Left plot is without sex linkage in resistance, right plot is with sex linkage"),
  
  fluidRow( 
    column(5, h5("resistance not sex linked"), plotOutput('plot') ),
    column(5, h5("resistance sex linked"), plotOutput('plotSexLinked') )
  ),
    
  hr(),
  
  fluidRow(
    column(2,
           h5("Starting frequency of resistance"),
           numericInput("P_1", "locus1:", 0.05, min = 0.01, max = 0.1, step = 0.01),
           numericInput("P_2", "locus2:", 0.05, min = 0.01, max = 0.1, step = 0.01)
           #numericInput("P_2", "locus2 - proportion of locus1:", 0.1, min = 0.01, max = 100, step = 0.01)
    ),
    column(2, offset = 0,
           h5("Dominance of resistance"),
           numericInput("h.RS1_A0", "locus1:", 0.2, min = 0, max = 1, step = 0.05),
           numericInput("h.RS2_0B", "locus2:", 0.2, min = 0, max = 1, step = 0.05)
    ),
    column(2, offset = 0,
           h5("Exposure to each insecticide"),
           #in Curtis it assumes exposure to AB the same at 0.9 & same for M&F
           numericInput("a.m_AB", "same for both insecticides in Curtis", 0.5, min = 0.1, max = 0.9, step = 0.1)
           #later may want to allow setting diff exposure for each insecticide & gender
           #numericInput("a.m_A", "insecticide1:", 0.5, min = 0.1, max = 0.9, step = 0.1),
           #numericInput("a.m_B", "insecticide2:", 0.5, min = 0.1, max = 0.9, step = 0.1)
    ),    
    column(2, offset = 0,
           h5("Fitness of susceptibles(SS) exposed to insecticide"),
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