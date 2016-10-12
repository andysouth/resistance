#resistance/inst/shiny/expovis/ui.r

library(shiny)


shinyUI(fluidPage( theme = "bootstrap_simplex.css",
                   
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
     
     h6("Exposure of vectors to insecticides. Modify inputs and see responces. by @southmapr"),  
     
     #fixedRow(
     fluidRow(
       column(3),    
       column(6, plotOutput('plotA'))
       #column(6, plotOutput('plotB'))
       #column(1)    
     ), #end fixed row
     
     hr(),
     
     fluidRow(
       column(1,
              #h5("scenario & insecticide "),
              h5("Insecticide "),
              h2("A"),
              h2("B")
              #hr(),
              #h2("B1"),
              #h2("B2")
       ),

       column(2, offset = 0, h5("Exposure"),
              sliderInput("exposure_A1", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
              sliderInput("exposure_A2", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE)                            
              #hr(),hr(),hr(),
              #sliderInput("exposure_B", NULL, val=0.5, min = 0, max = 1, step = 0.1, ticks=FALSE),
              #hr()
              #sliderInput("exposure_B2", NULL, val=0.2, min = 0, max = 1, step = 0.1, ticks=FALSE)           
       ),       
              
       column(2, offset = 1,        
              radioButtons("insecticideUsed", "Which insecticide(s) :",
                           c('insecticide A'='insecticide1',
                             'insecticide B'='insecticide2',
                             'mixture'='mixture'))
       ),
       
       column(2, offset = 0,
              h5("Mixture correct deployment"),
              sliderInput("correct_mix_deploy", NULL, 1, min = 0, max = 1, step = 0.1, ticks=FALSE)
       ),
       column(2, offset = 0,
              h5("Male exposure proportion"),
              sliderInput("male_exposure_prop", NULL, 1, min = 0, max = 1, step = 0.1, ticks=FALSE)
       )
       
     ) #end fixed row
                   
))
