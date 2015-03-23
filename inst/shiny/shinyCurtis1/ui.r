#resistance/shiny/shinyCurtis1/ui.r
#andy south 22/3/15

library(shiny)


shinyUI(fluidPage(
  
  title = "Curtis insecticide resistance",
  
  plotOutput('plot'),
  
  hr(),
  
  fluidRow(
    column(3,
           h4("Starting frequency of resistance"),
           numericInput("P_1", "locus1:", 0.05, min = 0.01, max = 0.1),
           numericInput("P_2", "locus2:", 0.05, min = 0.01, max = 0.1)
    ),
    column(4, offset = 0,
           h4("Dominance of resistance"),
           numericInput("h.RS1_00", "locus1:", 0.2, min = 0, max = 1),
           numericInput("h.RS2_00", "locus2:", 0.2, min = 0, max = 1)
    )

  )
))