#resistance/shiny/shinyNiche/ui.r
#andy south 29/2/16

library(shiny)


shinyUI(fluidPage(
  
  title = "Niche",
  
  h3("Niches, 2 insecticides (a,b), 2 sexes, 3 levels (0, lowercase=low, uppercase=high)"), 
  h5("(just levels 0 & high so far)"),   

  fluidRow(

    column(2, offset = 0,        
    radioButtons("insecticideUsed", "Which insecticide(s) :",
                 c('insecticide1'='insecticide1',
                   'insecticide2'='insecticide2',
                   'mixture'='mixture'))
    ),
    
    column(2, offset = 0,
           #h5("Exposure to each insecticide"),
           numericInput("exposure", "Exposure to insecticide", 0.9, min = 0, max = 1, step = 0.1)
           #later may want to allow setting diff exposure for each insecticide & gender
           #numericInput("a.m_A", "insecticide1:", 0.5, min = 0.1, max = 0.9, step = 0.1),
           #numericInput("a.m_B", "insecticide2:", 0.5, min = 0.1, max = 0.9, step = 0.1)
    ),
    column(2, offset = 0,
           numericInput("correctMixDeployProp", "Mixture correct deployment", 1, min = 0, max = 1, step = 0.1)
    ),
    column(2, offset = 0,
           numericInput("maleExposureProp", "Male exposure proportion", 0.9, min = 0, max = 1, step = 0.1)
    )
  ), #end fluid row

    # fluidRow(
  #   column(4, hr()),
  #   column(2, actionButton('aButtonRun', 'Run Model (takes a few seconds'))
  # ), #end fluid row  
    
  
  fluidRow(
    column(4, h4("female"), tableOutput('tableNicheF')),
    column(4, h4("male"), tableOutput('tableNicheM'))
    #column(6, plotOutput('plotDefault')),
    #column(6, plotOutput('plot'))
  ), #end fluid row
  
  hr()

))