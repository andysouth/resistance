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

  #Run model scenarios specified in an input object
  runModel2()
  
  #Recreate figure 2 of Curtis(1985), allows inputs to be tweaked
  runcurtis_f2()
  