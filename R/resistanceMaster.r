#' master function to setup and run resistance model scenarios
#' 
#' May restructure later.  
#' developed from malaria_code_andy.r
#' 
#' @param params.csv whether parameters are defined in an input file TRUE/FALSE
#' @param inFile parameters filename
#' @param calibration one of a limited set of integers effecting how scenarios are run 
#' @param produce.plots whether to produce plots
#' @param savePlots whether to save plots to hardcoded filenames
#' 
#' @return nothing yet
#' @export
#' 
resistanceMaster <- function(params.csv = TRUE,
                             inFile = system.file("extdata","input.parameters.csv", package="resistance"),
                             calibration = 1012,
                             produce.plots = TRUE,
                             savePlots = FALSE)
{
  
  #inFile <- "full-dom_SD.csv"
  #inFile <- "input.parameters.col1.csv" 
  
  ## manually editing the calibration number in a file
  ## allows for multiple runs with different calibrations

  ## Beth to run scenario to recreate curtis' plots
  #params.csv <- FALSE
  #calibration <- 1012
  
  ### OUTPUT - DATA MATRICES ####		
  # Results details freq of R allele at each loci in each sex and linkage disequilbrium of R allele in each sex, per generation
  # Genotype details frequencies of each of the ten genotypes, per generation
  # Fitness details the fitness scores of each genotype/niche combination (table 4. of Main Document)
  
  # for multiple scenarios, each of the three matrices is stored in a list
  # ie listOut$results, listOut$genotype, listOut$fitness, where scenario number gives position in list
  
  # If save.fitvals is set to 1 then a .csv file will be generated
  # containing the fitness values of each double genotype in each possible niche
  # as in table three of brief document
  
  ### OUTPUT - GRAPHICS ###
  # For each scenario run, four graphs will also be saved to the same drive as this document is located
  # frequency of R allele over time by sex (results table "results")
  # frequency of haplotypes over time by locus (results table "genotype")
  # linkage disequilibrium over time (results table "results")
  # frequency of R allele over time, but using the log percentage scale as in Curtis (1985) Fig 2
  # Where calibration 1013 (see below) is set, the latter is replaced with a plot of similar scale but more restricted axis limits
  
  ### CALIBRATIONS ####
  # Calibrations can be set to run specific stored scenarios, to generate certain plots or allow bug checking
  ## To recreate plots in Curtis (1985)
  ## Calibration 1011 runs the full dominance insecticide scenario given in Curtis (1985) Fig 1.
  ## Calibration 1012 runs the single insecticide and combination scenarios given in Curtis (1985) Fig 2. 
  
  ## For bug identifying/fixing
  ## Calibration 102 runs with no random mating
  ## Calibration 103 runs with no selection
  ## Calibration 104 runs with selection on one genotype
  ## set these below
  # select.gen.m <- fs.m.SS1RS2  ## two locus genotype to select on MUST be fs not f, unhash to run
  # select.gen.f <- fs.f.SS1RS2 # set for male and female form
  
  
  
  #calling new function to create input matrix
  input <- createInputMatrix( params.csv=params.csv, inFile=inFile, calibration=calibration)
  
  
  #### Run Model ####
  #creates listOut$results, listOut$genotype & listOut$fitness
  listOut <- runModel( input, calibration )
  
  
  ### Actions needing the full listOut$results ####
  
  ### Conditional plot commands for calibrations to Curtis paper ####
  
  ### Curtis Figures require multiple results matrices, so are called outside of looping through the input matrix
  ## Linkage Disequilibrium - top half of figure one
  if( produce.plots & calibration == 1011){
      
    plot <- plotcurtis_ld( listOut$results[[1]], listOut$results[[2]], 1, 4 )
    if (savePlots)
      {
        dev.copy(png,('LD_curtis-fig1.png'))		## WARNING: this will overwrite every time, move or rename files! ##
        dev.off()  
      }
    # Bottom half of figure one
    # Plot of total frequency of R allele over time, as fig.2 of Curtis (1985)
    genplot <- plotcurtis_f1( listOut$results[[1]], listOut$results[[2]], 1, 2 )
    if (savePlots)
    {
      dev.copy(png,('curtis-fig1.png'))		## WARNING: this will overwrite every time, move or rename files! ##
      dev.off()
    }
  }
  
  
  ## Fig 2 - plots sequential and combination, as in Curtis fig 2
  if( calibration == 1012 ){
  	curtis <- plotcurtis_f2( listOut$results[[3]], listOut$results[[1]], listOut$results[[2]], 1, 2, 3 )
  	if (savePlots)
  	{
    	dev.copy(png,('curtis-fig2.png'))		## WARNING: this will overwrite every time, move or rename files! ##
    	dev.off()
  	}
  }
  	
  
  ### Finding generations taken to reach a frequency of R of 0.5 at each locus ####
  #andy, thes now can't be done from here because max_gen doesn't make it out of runModel()
  #loc1_0.5 <- timetoFifty( 1, max_gen, listOut$results, input )
  #loc2_0.5 <- timetoFifty( 2, max_gen, listOut$results, input )
  
}