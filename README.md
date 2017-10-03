# resistance

An insecticide resistance population genetics model with 2 loci and 2 insecticides.

Development by Andy South, Ian Hastings and Beth Levick, 2015-8.

The model is described here :
Levick, B., South, A., & Hastings, I. M. (2017). A two-locus model of the evolution of insecticide resistance to inform and optimise public health insecticide deployment strategies. PLoS Computational Biology, 13, e1005327. [https://doi.org/10.1371/journal.pcbi.1005327](https://doi.org/10.1371/journal.pcbi.1005327).

The code is licensed GPL-3, please cite the paper above if you use it.

Running the code is slightly involved, you may want to explore the user interfaces below first.


### Installation

    #install.packages("devtools")
    require(devtools)    
    install_github('AndySouth/resistance')  
    require(resistance)
    
    
### User Interfaces

    #install.packages("shiny")
    require(shiny) 
    
##### UI1: single scenario, mixture, show all 4 loci    
    
    runUI1() 
    
##### UI2: effect of sex-linkage on a mixture
    
    runUI2() 

##### UI3: recreates Curtis(1985) Fig2 and allows params to be varied to modify the plot      

    runUI3()
   
##### UI4: allows 2 scenarios of mixture and sequential to be compared and modified   

    runUI4()   
    
### Main functions to run the model

    # Run model scenarios specified in an input object
    input <- setInputOneScenario(P_1=0.01)
    listOut <- runModel2(input, produce.plots = TRUE)
  
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
  