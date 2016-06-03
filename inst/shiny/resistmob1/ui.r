#resistance/shiny/resistmob1/ui.r
#andy south 3/6/16
#trying to make a mobile compatible app

library(shiny)


shinyUI(fluidPage(
#shinyUI(fixedPage(
 
  #can add CSS controls in here
  #http://shiny.rstudio.com/articles/css.html
  #this seems to make no difference yet
  tags$head(
    tags$style(HTML("
                    img {
                    border: 1;
                    max-width: 100%;
                    }
                    element.style {
                    width: 33.33%;
                    }
                    
                    "))
    ),  
     
  title = "resistance to insecticide mixtures",
  
  h6("Development of resistance to insecticides used in mixtures and sequence. Modify inputs and compare 2 scenarios. by @southmapr"),  
  
  #fixedRow(
  fluidRow(
    #column(1),    
    column(6, plotOutput('plotA')),
    column(6, plotOutput('plotB'))
    #column(1)    
  ), #end fixed row
  
  fluidRow(
    #column(4, h5("takes a few secounds :")),
    column(2, p()),
    column(2, actionButton('aButtonRunA', 'Run Scenario A')),
    column(1, p()),   
    column(3, h6("(takes a few secounds)")),
    column(2, actionButton('aButtonRunB', 'Run Scenario B')),
    column(2, p())
  ), #end fixed row
  
  
  hr(),
  
  fluidRow(
    column(2,
           h5("scenario & insecticide "),
           h2("A1"),
           h2("A2"),
           hr(),
           h2("B1"),
           h2("B2")
    ),
    column(2,
           #h5("Starting frequency of resistance"),
           h5("Start Frequency"),
           #numericInput("P_1", "locus1: 0.01", 0.01, min = 0.0001, max = 0.1, step = 0.001),
           #numericInput("P_2", "locus2: 0.01", 0.01, min = 0.0001, max = 0.1, step = 0.001)
           sliderInput("frequency_A1", NULL, val=0.01, min = 0.0001, max = 0.1, step = 0.001, ticks=FALSE),
           sliderInput("frequency_A2", NULL, val=0.01, min = 0.0001, max = 0.1, step = 0.001, ticks=FALSE),
           hr(),
           sliderInput("frequency_B1", NULL, val=0.01, min = 0.0001, max = 0.1, step = 0.001, ticks=FALSE),
           sliderInput("frequency_B2", NULL, val=0.01, min = 0.0001, max = 0.1, step = 0.001, ticks=FALSE)
           
    ),    
    column(2, offset = 0,
           h5("Dominance of resistance"),
           #numericInput("h.RS1_A0", "locus1: 0.17", 0.17, min = 0, max = 1, step = 0.01),
           #numericInput("h.RS2_0B", "locus2: 0.0016", 0.0016, min = 0, max = 1, step = 0.01)
           sliderInput("dominance_A1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("dominance_A2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           hr(),
           sliderInput("dominance_B1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("dominance_B2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE)
    ),
    column(2, offset = 0,
           h5("Exposure to insecticide"),
           #in Curtis it assumes exposure to AB the same at 0.9 & same for M&F
           #numericInput("exposure", "same for both insecticides in Curtis 0.9", 0.9, min = 0.1, max = 1, step = 0.05)
           #sliderInput("exposure", NULL, val=0.2, min = 0, max = 1, step = 0.1, ticks=FALSE),
           hr(),
           sliderInput("exposure_A", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           hr(),hr(),hr(),
           sliderInput("exposure_B", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           hr()
           #sliderInput("exposure_B2", NULL, val=0.2, min = 0, max = 1, step = 0.1, ticks=FALSE)           
    ),    
    column(2, offset = 0,
           #h5("Selection against susceptibles(SS) exposed to insecticide"),
           h5("Effectiveness of insecticides"),
           #numericInput("phi.SS1_A0", "locus1: 0.73", 0.73, min = 0, max = 1, step = 0.05),
           #numericInput("phi.SS2_0B", "locus2: 1", 1, min = 0, max = 1, step = 0.05)
           sliderInput("effectiveness_A1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("effectiveness_A2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           hr(),
           sliderInput("effectiveness_B1", NULL, val=0.9, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("effectiveness_B2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE)             
           
    ),
    column(2, offset = 0,
           #h5("Selective advantage of resistance"),
           h5("Advantage of resistance"),
           #numericInput("s.RR1_A0", "locus1: 0.23", 0.23, min = 0, max = 1, step = 0.05),
           #numericInput("s.RR2_0B", "locus2: 0.43", 0.43, min = 0, max = 1, step = 0.05),
           
           sliderInput("advantage_A1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("advantage_A2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           hr(),
           sliderInput("advantage_B1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("advantage_B2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE)             
           
           
    )
    # column(2, offset = 0,
    #        h5("Extra params not included in Curtis"),
    #        numericInput("correctMixDeployProp", "Mixture correct deployment", 1, min = 0, max = 1, step = 0.1),
    #        checkboxInput("addCombinedStrategy", "plot adaptive strategy",FALSE)
    # )
  ) #end fixed row

))