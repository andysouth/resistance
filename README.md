# resistance




An insecticide resistance population genetics model with 2 loci and 2 insecticides.

Development by Andy South, Beth Levick and Ian Hastings in 2015-6.


### Installation

    #install.packages("devtools")
    require(devtools)    
    install_github('AndySouth/resistance')  
    require(resistance)
    
    
### User Interfaces

    #install.packages("shiny")
    require(shiny) 
    
#### single scenario, mixture, show all 4 loci    
    
    runUI1() 
    
#### effect of sex-linkage on a mixture
    
    runUI2() 

#### recreates Curtis(1985) Fig2 and allows params to be varied to modify the plot      

    runUI3()
   
#### allow 2 scenarios of mixture and sequential to be compared and modified   

    runUI4()   
    
    
### Main functions to run the model

  # Run model scenarios specified in an input object
  runModel2()
  
  # Recreate figure 2 of Curtis(1985), allows inputs to be tweaked
  runcurtis_f2()
  
### Sensitivity analysis for the first paper 

  # inst/documents/sensiAnPaper1All.Rmd
  # an rmarkdown file that runs all of the sensitivity analysis for paper1
  # it runs the model itself like in this example :
  nScenarios <- 3 

  listOutMix <- sensiAnPaperPart( nScenarios, insecticideUsed = 'mixture' )
  listOutI1 <- sensiAnPaperPart( nScenarios, insecticideUsed = 'insecticide1' )
  listOutI2 <- sensiAnPaperPart( nScenarios, insecticideUsed = 'insecticide2' )
  
  # sensiAnPaperPart() contains hardcoded input ranges
  # it generates random numbers and repeatedly calls setInputOneScenario()
  # then it calls runModel2() to run the model using these inputs
  