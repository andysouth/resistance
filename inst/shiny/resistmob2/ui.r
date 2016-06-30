#resistance/shiny/resistmob2/ui.r
#andy south 15/6/16
#a mobile compatible app
#uses NEW system where rr_restoration used as input instead of selection coefficient

library(shiny)


shinyUI(fluidPage( theme = "bootstrap_simplex.css",
#shinyUI(fixedPage( theme = "bootstrap_simplex.css",
 
  #can add CSS controls in here
  #http://shiny.rstudio.com/articles/css.html
  #http://www.w3schools.com/css/css_rwd_mediaqueries.asp
  tags$head(
    tags$style(HTML("

                    [class*='col-'] {
                        padding: 5px;
                        border: 1px;
                        position: relative;
                        min-height: 1px;
                    }

                    .container {
                      margin-right: 0;
                      margin-left: 0;
                      float: left;
                    }
                    .col-sm-1 {width: 8.33%; float: left;}
                    .col-sm-2 {width: 16.66%; float: left;}
                    .col-sm-3 {width: 25%; float: left;}
                    .col-sm-4 {width: 33.33%; float: left;}
                    .col-sm-5 {width: 41.66%; float: left;}
                    .col-sm-6 {width: 50%;  float: left; padding: 0px;} !to make more space for plots
                    .col-sm-7 {width: 58.33%; float: left;}
                    .col-sm-8 {width: 66.66%; float: left;}
                    .col-sm-9 {width: 75%; float: left;}
                    .col-sm-10 {width: 83.33%; float: left;}
                    .col-sm-11 {width: 91.66%; float: left;}
                    .col-sm-12 {width: 100%; float: left;}
                    
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
    #column(4, h5("takes a few seconds :")),
    column(2, p()),
    column(2, actionButton('aButtonRunA', 'Run Scenario A')),
    column(1, p()),   
    column(3, h6("(takes a few seconds)")),
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
           h5("Start Freq."),
           #numericInput("P_1", "locus1: 0.01", 0.01, min = 0.0001, max = 0.1, step = 0.001),
           #numericInput("P_2", "locus2: 0.01", 0.01, min = 0.0001, max = 0.1, step = 0.001)
           sliderInput("frequency_A1", NULL, val=0.01, min = 0.0001, max = 0.1, step = 0.001, ticks=FALSE),
           sliderInput("frequency_A2", NULL, val=0.01, min = 0.0001, max = 0.1, step = 0.001, ticks=FALSE),
           hr(),
           sliderInput("frequency_B1", NULL, val=0.01, min = 0.0001, max = 0.1, step = 0.001, ticks=FALSE),
           sliderInput("frequency_B2", NULL, val=0.01, min = 0.0001, max = 0.1, step = 0.001, ticks=FALSE)
           
    ),    
    column(2, offset = 0,
           h5("Dominance"),
           #numericInput("h.RS1_A0", "locus1: 0.17", 0.17, min = 0, max = 1, step = 0.01),
           #numericInput("h.RS2_0B", "locus2: 0.0016", 0.0016, min = 0, max = 1, step = 0.01)
           sliderInput("dominance_A1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("dominance_A2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           hr(),
           sliderInput("dominance_B1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("dominance_B2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE)
    ),
    column(2, offset = 0,
           h5("Exposure"),
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
           h5("Effectiveness"),
           #numericInput("phi.SS1_A0", "locus1: 0.73", 0.73, min = 0, max = 1, step = 0.05),
           #numericInput("phi.SS2_0B", "locus2: 1", 1, min = 0, max = 1, step = 0.05)
           sliderInput("effectiveness_A1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("effectiveness_A2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
           hr(),
           sliderInput("effectiveness_B1", NULL, val=0.9, min = 0, max = 1, step = 0.1, ticks=FALSE),
           sliderInput("effectiveness_B2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE)             
           
    ),
    column(2, offset = 0,
           #h5("Advantage of resistance"),
           h5("RR restoration"),
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
    #        numericInput("correct_mix_deploy", "Mixture correct deployment", 1, min = 0, max = 1, step = 0.1),
    #        checkboxInput("addCombinedStrategy", "plot adaptive strategy",FALSE)
    # )
  ) #end fixed row

))