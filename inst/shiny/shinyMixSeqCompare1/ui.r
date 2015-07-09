#resistance/shiny/shinyMixSeqCompare/ui.r
#andy south 6/7/15
#to view mixtures versus sequential in pre-run sensitivity analysis results

library(shiny)

# shinyUI(pageWithSidebar(
#   headerPanel("explore resistance sensitivity analysis results"),
#   sidebarPanel(
#     numericInput('scenarioNum','choose a scenario number', value=1, min=1, max=100, step=1 )
#  
#   ),
#   mainPanel(
#     #plotOutput('plotTest')
#     plotOutput('plot')
#     )
# ))

shinyUI(fluidPage(
  
  title = "Insecticide Mixture Explorer",

  
  fluidRow(
    column(2,
           numericInput('scenarioNum','choose a run no.', value=1, min=1, max=100, step=1 ),
           helpText('100 runs are stored, use arrows on right of box to step through them')
    ),
    column(2, 
           helpText('input values for this run'),
           tableOutput('tableInputVals')
    ),
    column(5,
           plotOutput('plot')
    )
  )
))